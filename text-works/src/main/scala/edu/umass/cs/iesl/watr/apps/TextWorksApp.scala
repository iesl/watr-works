package edu.umass.cs.iesl.watr
package apps

import corpora._

import ammonite.{ops => fs} // , fs._
import segment._
import TypeTags._
import scopt.Read
import shapeless._
import java.nio.{file => nio}
import tracing.VisualTracer
import _root_.io.circe, circe._, circe.syntax._
import formats._

import cats.effect._

import PathConversions._
import geometry._
import utils.Timer.time

object TextWorksConfig {
  implicit val NioPath: Read[nio.Path] =
    Read.reads { v =>
      nio.Paths.get(v).toAbsolutePath().normalize()
    }

  case class Config(
    ioConfig        : IOConfig = IOConfig(),
    initCorpus      : Option[nio.Path] = None,
    runTraceLogging : Boolean = VisualTracer.tracingEnabled(),
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


object TextWorksActions extends GeometricFigureCodecs {
  private[this] val log = org.log4s.getLogger

  val PrettyPrint2Spaces = circe.Printer.spaces2

  type MarkedInput = Either[InputMode, InputMode]
  type RunInput    = Right[InputMode, InputMode]
  type SkipInput   = Left[InputMode, InputMode]

  def cleanOldArtifacts(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    inStream => {
      inStream.map {
        case Left(inputMode) => Left(inputMode)
        case Right(inputMode) =>
          val output = InputMode.getOutputFile(inputMode, conf.ioConfig)

          if (output.toFile().exists()) {
            if (conf.ioConfig.overwrite) {
              log.info(s"Overwriting ${output}")
              fs.rm(nioToAmm(output))
              Right(inputMode)
            } else {
              Left(inputMode)
            }
          } else {
            Right(inputMode)
          }
      }
    }
  }

  def doSegment1(input: InputMode, conf: TextWorksConfig.Config): Either[String, DocumentSegmentation] = {
    try {
      input match {
        case m@ InputMode.SingleFile(f) =>

          val pdfName = f.getFileName.toString()
          val stableId = DocumentID(pdfName)
          val input = nioToAmm(f)
          val output = InputMode.getOutputFile(m, conf.ioConfig)

          if (!output.toFile().exists()) {
            Right(TextWorksActions.extractText(stableId, input, nioToAmm(output), None))
          } else {
            val msg = s"File ${output} already exists. Move file or use --overwrite"
            println(msg)
            Left(msg)
          }


        case m@ InputMode.CorpusFile(_, Some(corpusEntry)) =>

          val stableId = DocumentID(corpusEntry.entryDescriptor)

          println(s"Processing ${stableId}")

          val maybeSegmenter = for {
            pdfEntry <- corpusEntry.getPdfArtifact.toRight(left="Could not get PDF")
            pdfPath <- pdfEntry.asPath.toEither.left.map(_.toString())
          } yield {

            val traceLogRoot = if (conf.runTraceLogging) {
              val traceLogGroup = corpusEntry.ensureArtifactGroup("tracelogs")
              traceLogGroup.deleteGroupArtifacts()
              Some(traceLogGroup.rootPath)
            } else None

            val output = InputMode.getOutputFile(m, conf.ioConfig)
            val ammPath = nioToAmm(output)

            time("extractText") {
              TextWorksActions.extractText(stableId, pdfPath, ammPath, traceLogRoot)
            }
          }
          maybeSegmenter.left.map { s =>
            println(s"Error: ${s}")
            s
          }

          maybeSegmenter

        case m => Left(s"Unsupported InputMode ${m}")
      }
    } catch {
      case t: Throwable =>
        utils.Debugging.printAndSwallow(t)
        Left[String,DocumentSegmentation](t.toString())
    }

  }


  def runSegmentation(conf: TextWorksConfig.Config): fs2.Pipe[IO, Either[InputMode, InputMode], Either[String, DocumentSegmentation]] =
    inStream => {
      inStream.map {
        case Left(input) => Left("Skipping")
        case Right(input) => doSegment1(input, conf)
      }
    }

  def getInputStream(conf: TextWorksConfig.Config): fs2.Stream[IO, InputMode] = {
    new IOOptionParser(conf.ioConfig).inputStream()
  }

  def buildProcessStream(conf: TextWorksConfig.Config): fs2.Stream[IO, Either[String, DocumentSegmentation]] = {
    val ioOpts = new IOOptionParser(conf.ioConfig)

    val fsStream = ioOpts.inputStream().evalMap { input =>
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
            if (output.toFile().exists() && conf.ioConfig.overwrite) {
              fs.rm(nioToAmm(output))
              Right(TextWorksActions.extractText(stableId, input, nioToAmm(output), None))
            } else {
              val msg = s"File ${output} already exists. Move file or use --overwrite"
                println(msg)
                Left(msg)
              }


            case InputMode.CorpusFile(_, Some(corpusEntry)) =>

              val stableId = DocumentID(corpusEntry.entryDescriptor)

              println(s"Processing ${stableId}")

              val maybeSegmenter = for {
                pdfEntry <- corpusEntry.getPdfArtifact.toRight(left="Could not get PDF")
                pdfPath <- pdfEntry.asPath.toEither.left.map(_.toString())
                docsegPath <- ioOpts.maybeProcess(corpusEntry, "textgrid.json").toRight(left="Existing output. --overwrite to force processing")
              } yield {

                val traceLogRoot = if (conf.runTraceLogging) {
                  val traceLogGroup = corpusEntry.ensureArtifactGroup("tracelogs")
                  traceLogGroup.deleteGroupArtifacts()
                  Some(traceLogGroup.rootPath)
                } else None


                val ammPath = nioToAmm(docsegPath)

                time("extractText") {
                  TextWorksActions.extractText(stableId, pdfPath, ammPath, traceLogRoot)
                }
              }
              maybeSegmenter.left.map { s =>
                println(s"Error: ${s}")
                s
              }

              maybeSegmenter

            case m => Left(s"Unsupported InputMode ${m}")
          }
        } catch {
          case t: Throwable =>
            utils.Debugging.printAndSwallow(t)
            Left[String,DocumentSegmentation](t.toString())
        }
      }
    }

    fsStream

  }

  def extractText(conf: TextWorksConfig.Config): Unit = {
    val processStream = buildProcessStream(conf)

    val prog = processStream.compile.drain

    prog.unsafeRunSync()
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

    time("runDocumentSegmentation") {
      segmenter.runDocumentSegmentation()
    }

    val jsonOutput = time("build json output") {  TextGridOutputFormats.jsonOutputGridPerPage(segmenter) }

    val gridJsStr = jsonOutput.pretty(jsonPrinter)

    time("write textgrid.json") {
      fs.write(textOutputFile, gridJsStr)
    }

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
