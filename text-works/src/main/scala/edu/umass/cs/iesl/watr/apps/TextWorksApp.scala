package edu.umass.cs.iesl.watr
package apps

import corpora._

import ammonite.{ops => fs} // , fs._
import segment.DocumentSegmenter
import segment.DocumentSegmentation
import TypeTags._
import scopt.Read
import shapeless._
import java.nio.{file => nio}
import tracing.VisualTracer
import _root_.io.circe, circe._, circe.syntax._

import cats.effect._

sealed trait OutputOption

object OutputOption {
  case object VisualStructure extends OutputOption
  case object ReadingStructure extends OutputOption
  case object SuperSubEscaping extends OutputOption

  implicit val OutputOptionRead: Read[OutputOption] =
    Read.reads { _.toLowerCase match {
      case "VisualStructure"        | "vs"  => VisualStructure
      case "ReadingStructure"       | "rs"  => ReadingStructure
      case "SuperSubEscaping"       | "sse" => SuperSubEscaping
      case s       =>
        throw new IllegalArgumentException(s"""'${s}' is not an output option.""")
    }}
}

object TextWorksConfig {
  implicit val NioPath: Read[nio.Path] =
    Read.reads { v =>
      nio.Paths.get(v).toAbsolutePath().normalize()
    }

  case class Config(
    ioConfig        : IOConfig = IOConfig(),
    initCorpus      : Option[nio.Path] = None,
    runTraceLogging : Boolean = VisualTracer.tracingEnabled(),
    outputOptions   : List[OutputOption] = List(),
    exec            : Option[(Config) => Unit] = Some((c) => TextWorksActions.extractText(c))
  )

  val parser = new scopt.OptionParser[Config]("text-works") {
    import scopt._

    override def renderingMode: RenderingMode = RenderingMode.OneColumn

    head("Text Works PDF text extraction, part of the WatrWorks", "0.1")

    note("Run text extraction and analysis on PDFs")

    help("help")

    /// IO Config options
    note("Specify exactly one input mode: corpus|file|file-list \n")


    opt[nio.Path]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.modify(conf) { ioConfig =>
        ioConfig.copy(
          inputMode = Option(InputMode.CorpusFile(v, None))
        )
      }
    } text ("root path of PDF corpus; output will be written to same dir as input")

    opt[nio.Path]('i', "input") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.SingleFile(v))
      }
    } text("choose single input PDF")

    opt[nio.Path]('l', "input-list") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.ListOfFiles(v))
      }
    } text("process list of input PDFs in specified file.")

    opt[String]("filter") action { (v, conf) =>
      lens[Config].ioConfig.pathFilter.modify(conf){ m =>
        val f = trimQuotes(v)
        Option(f)
      }
    } text("if specified, only files matching regex will be processed")


    def trimQuotes(s: String): String = {
      val t = s.trim
      if (t.length() >= 2) {
        val isQuote = List('"', '\'').contains(t.head)
        if (isQuote && t.head == t.last) {
          t.drop(1).dropRight(1)
        } else s
      } else s
    }

    note("\nOutput file options\n")

    opt[nio.Path]('o', "output-file") action { (v, conf) =>
      lens[Config].ioConfig.outputPath.modify(conf){ m =>
        Some(v)
      }
    } text("""|specify output file. In --corpus mode, ouput will be written to same directory as
              |           input file, otherwise relative to cwd. Use --overwrite to overwrite existing files.""".stripMargin)


    opt[nio.Path]("init-corpus") action { (v, conf) =>
      val conf1 = lens[Config].initCorpus.modify(conf){ m =>
        Some(v)
      }
      setAction(conf1, initCorpus(_))
    } text ("initialize dir of pdfs to corpus structure")

    opt[Unit]("overwrite") action { (v, conf) =>
      lens[Config].ioConfig.overwrite.modify(conf){ m =>
        true
      }
    } text ("force overwrite of existing output artifacts")

    note("\nOutput text layout options: \n")

    opt[OutputOption]('p', "layout-option") action { (v, conf) =>
      conf.copy(outputOptions = v :: conf.outputOptions)
    } text("choose layout options for extracted text [VisualStructure|ReadingStructure] [SuperSubEscaping]")

    val toAmm = PathConversions.nioToAmm(_)

    checkConfig{ c =>
      if (c.initCorpus.isDefined) {
        val corpusRoot = c.initCorpus.get
        if (fs.exists(toAmm(corpusRoot))) {
          success
        } else {
          failure(s"Corpus root ${corpusRoot} doesn't exist")
        }
      } else {
        c.ioConfig.inputMode.map { _  match {
          case InputMode.SingleFile(f) =>
            if (fs.exists(toAmm(f))) success else failure(s"file ${f} not found")
          case InputMode.ListOfFiles(f) =>
            if (fs.exists(toAmm(f))) success else failure(s"file ${f} not found")
          case InputMode.CorpusFile(f, maybeEntry) =>
            if (fs.exists(toAmm(f))) success else failure(s"corpus root ${f} not found")
        }} getOrElse{
          failure("No input specified")
        }
      }
    }
  }

  def setAction(conf: Config, action: (Config) => Unit): Config = {
    conf.copy(exec=Option(action))
  }



  def initCorpus(conf: Config): Unit = {
    println("initCorpus")
    conf.initCorpus.foreach { corpusRoot =>
      filesys.Corpus.initCorpus(corpusRoot.toString())
    }
  }
}

import PathConversions._
import geometry._
object TextWorksActions extends GeometricFigureCodecs {
  val PrettyPrint2Spaces = circe.Printer.spaces2


  def buildProcessStream(conf: TextWorksConfig.Config): fs2.Stream[IO, Either[String, DocumentSegmentation]] = {
    val ioOpts = new IOOptionParser(conf.ioConfig)

    val fsStream = ioOpts.inputStream().zipWithIndex
      .evalMap { case (input, i) =>
        IO {
          try {
            input match {
              case m@ InputMode.SingleFile(f) =>

                val pdfName = f.getFileName.toString()
                val stableId = DocumentID(pdfName)
                val input = nioToAmm(f)
                val output = conf.ioConfig.outputPath.getOrElse {
                  nio.Paths.get(pdfName + ".textgrid.json")
                }

                Right(TextWorksActions.extractText(stableId, input, nioToAmm(output), None))

              case InputMode.CorpusFile(_, Some(corpusEntry)) =>

                val stableId = DocumentID(corpusEntry.entryDescriptor)

                println(s"Processing ${stableId}")

                val maybeSegmenter = for {
                  pdfEntry <- corpusEntry.getPdfArtifact.toRight(left="Could not get PDF")
                  pdfPath <- pdfEntry.asPath.toEither.left.map(_.toString())
                  docsegPath <- ioOpts.maybeProcess(corpusEntry, "docseg.json").toRight(left="Existing output. --overwrite to force processing")
                } yield {

                  val traceLogRoot = if (conf.runTraceLogging) {
                    val traceLogGroup = corpusEntry.ensureArtifactGroup("tracelogs")
                    traceLogGroup.deleteGroupArtifacts()
                    Some(traceLogGroup.rootPath)
                  } else None


                  val ammPath = nioToAmm(docsegPath)

                  TextWorksActions.extractText(stableId, pdfPath, ammPath, traceLogRoot)
                }

                maybeSegmenter

              case m => Left(s"Unsupported InputMode ${m}")
            }
          } catch {
            case t: Throwable =>
              // println(s"t = ${t}")
              // t.printStackTrace()
              utils.Debugging.printAndSwallow(t)
              Left[String,DocumentSegmentation](t.toString())
          }
        }
      }

    fsStream

  }

  def extractText(conf: TextWorksConfig.Config): Unit = {
    val processStream = buildProcessStream(conf)
    processStream.run.unsafeRunSync()
  }

  val jsonPrinter = circe.Printer(
    preserveOrder = true,
    dropNullValues = false,
    indent = "    ",
    lbraceRight = "",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = " ",
    colonRight = " "
  )

  def extractText(
    stableId: String@@DocumentID,
    inputPdf: fs.Path,
    textOutputFile: fs.Path,
    traceLogRoot: Option[fs.Path]
  ): DocumentSegmentation = {

    println(s"Extracting ${stableId}")

    val zoningApi = new MemDocZoningApi

    val segmenter = DocumentSegmenter.createSegmenter(stableId, inputPdf, zoningApi)

    segmenter.runDocumentSegmentation()

    val allPageTextGrids = segmenter.pageSegmenters
      .map { pageSegmenter =>
        val textGrid = pageSegmenter.getTextGrid
        val gridJs = textGrid.buildOutput().gridToJson()

        val LTBounds.IntReps(l, t, w, h) = pageSegmenter.pageGeometry
        val geom = Json.arr(Json.fromInt(l), Json.fromInt(t), Json.fromInt(w), Json.fromInt(h))

        Json.obj(
          ("pageGeometry" := geom),
          ("textgrid" := gridJs)
        )
      }


    val fontDefs = segmenter.docScope.fontDefs

    val scaledMetrics = fontDefs.fontProperties.map { fp =>
      println(s"fp: ${fp}")
      println(s"sm: ${fp.scaledMetrics} ")
      val metrics = fp.scaledMetrics.map{ scaledMetrics =>
        Json.obj(
          "scale" := scaledMetrics.scalingFactor.unwrap,
          "heights" := Json.obj(
            "lowerCaseSmall" := scaledMetrics.heightLowerCaseSmall,
            "lowerCaseLarge" := scaledMetrics.heightLowerCaseLarge,
            "upperCase" := scaledMetrics.heightUpperCase
          )
        )
      }

      /**

        "fontDescriptions": {
            "namedFonts": [
                {
                    "name": "ACOONO+AdvOptima-b",
                    "naturalBigrams" : 15
                    "scaled": [
                        [104, {"heights" : {"lcSmall" : 0.0, "lcLarge" : 0.0, "uc" : 0.0 } }],
                        [208, {"heights" : {"lcSmall" : 0.0, "lcLarge" : 0.0, "uc" : 0.0 } }]
                    ]
                },
            ],
            "scaledFonts": [
                [0, "ACOONO+AdvOptima-b", 104],
                [1, "ACOONO+AdvOptima-b", 208],
                [2, "ACOONO+AdvOptima-b", 104]
            ]
       }



       "fonts" : [
        {
            "name" : "ACOONO+AdvOptima-b",
            "bigrams" : 15,
            "metrics" : [
                 {
                    "scale" : 104,
                    "heights" : {
                        "lowerCaseSmall" : 0.0,
                        "lowerCaseLarge" : 0.0,
                        "upperCase" : 0.0
                    }
                }, {
                    "scale" : 208,
                    "heights" : {"lowerCaseSmall" : 7.266,
                        "lowerCaseLarge" : 10.979999999999999,
                        "upperCase" : 10.055714285714286
                    }
                }]






        */












      Json.obj(
        "name" := fp.name,
        "bigrams" := fp.bigramEvidence.count(_ > 0),
        "metrics" := metrics
      )
    }

    val jsLog = Json.obj(
      "description" := s"Extracted Pages for ${stableId}",
      "fonts" := scaledMetrics,
      "pages" := allPageTextGrids
    )

    val gridJsStr = jsLog.pretty(jsonPrinter)

    fs.write(textOutputFile, gridJsStr)

    traceLogRoot.foreach { rootPath =>
      println("writing tracelogs")

      fs.ls(rootPath)
        .foreach{ p => fs.rm(p) }


      val allLogs = segmenter.pageSegmenters
        .foldLeft(List[Json]()) {
          case (accum, pageSegmenter) =>
            accum ++ pageSegmenter.emitLogs()
        }

      val jsonLogs = allLogs.asJson
      val jsonStr = jsonLogs.pretty(PrettyPrint2Spaces)

      fs.write(rootPath / "tracelog.json", jsonStr)
    }

    segmenter
  }

}

object TextWorks extends App {
  import TextWorksConfig._

  parser.parse(args, Config()).foreach{ config =>
    config.exec.foreach { _.apply(config) }
  }

}
