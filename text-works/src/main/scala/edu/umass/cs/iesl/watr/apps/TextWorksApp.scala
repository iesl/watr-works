package edu.umass.cs.iesl.watr
package apps

import corpora._

import ammonite.{ops => fs}, fs._
// import java.io.{File => JFile}
import segment.DocumentSegmenter
import TypeTags._
import scopt.Read
import shapeless._
import java.nio.{file => nio}
import fs2._

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
    writeRTrees     : Boolean = false,
    outputOptions   : List[OutputOption] = List(),
    exec            : Option[(Config) => Unit] = Some((c) => extractText(c))
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



    note("\nOutput file options\n")

    opt[nio.Path]('o', "output-file") action { (v, conf) =>
      lens[Config].ioConfig.outputPath.modify(conf){ m =>
        Some(v)
      }
    } text("""|specify output file. In --corpus mode, ouput will be written to same directory as
              |           input file, otherwise relative to cwd. Use --overwrite to overwrite existing files.""".stripMargin)


    opt[Unit]("write-rtrees") action { (v, conf) =>
      lens[Config].writeRTrees.modify(conf){ m =>
        true
      }
    } text ("write the computed rtrees to disk")

    opt[Unit]("overwrite") action { (v, conf) =>
      lens[Config].ioConfig.overwrite.modify(conf){ m =>
        true
      }
    } text ("write the computed rtrees to disk")


    note("\nOutput text layout options: \n")

    opt[OutputOption]('p', "layout-option") action { (v, conf) =>
      conf.copy(outputOptions = v :: conf.outputOptions)
    } text("choose layout options for extracted text [VisualStructure|ReadingStructure] [SuperSubEscaping]")


    checkConfig{ c =>
      if (c.ioConfig.inputMode.isEmpty) {
        failure("Invalid input options")
      } else success
    }
  }

  def setAction(conf: Config, action: (Config) => Unit): Config = {
    conf.copy(exec=Option(action))
  }



  def extractText(conf: Config): Unit = {
    val ioOpts = new IOOptionParser(conf.ioConfig)

    val fsStream = ioOpts.inputStream()
      .through(pipe.zipWithIndex)
      .evalMap { case (input, i) =>
        Task.delay{

          try {

            input match {
              case InputMode.SingleFile(f) =>
              case InputMode.CorpusFile(_, Some(corpusEntry)) =>

                val stableId = DocumentID(corpusEntry.entryDescriptor)

                for {
                  pdfEntry <- corpusEntry.getPdfArtifact
                  pdfPath <- pdfEntry.asPath
                  docsegPath <- ioOpts.maybeProcess(corpusEntry, "docseg.json")
                } {
                  val rtreeGroup = corpusEntry.ensureArtifactGroup("rtrees")

                  val rtreeOutput = if (conf.writeRTrees) {
                    rtreeGroup.deleteGroupArtifacts()
                    Some(rtreeGroup.rootPath)
                  } else None

                  val ammPath = PathConversions.nioToAmm(docsegPath)

                  TextWorksActions.extractText(stableId, pdfPath, ammPath, rtreeOutput)
                }
            }
          } catch {
            case t: Throwable => println(s"error ${t}")
          }
        }
      }


    fsStream.run.unsafeRun()
  }
}

object TextWorksActions {

  def extractText(
    stableId: String@@DocumentID,
    inputPdf: fs.Path,
    textOutputFile: fs.Path,
    rtreeOutputRoot: Option[fs.Path]
  ): Unit = {

    val segmenter = DocumentSegmenter.createSegmenter(stableId, inputPdf, new MemDocZoningApi)

    segmenter.runPageSegmentation()

    val mpageIndex = segmenter.mpageIndex

    val content = formats.DocumentIO.documentToPlaintext(mpageIndex)
    write(textOutputFile, content)

    rtreeOutputRoot.foreach { rtreeRootPath =>

      for { pageNum <- segmenter.mpageIndex.getPages } {
        val pageIndex = segmenter.mpageIndex.getPageIndex(pageNum)
        val bytes = pageIndex.saveToBytes()
        val pageIndexPath = rtreeRootPath / pageIndex.rtreeArtifactName
        fs.write(pageIndexPath, bytes)
      }
    }

  }

}

object TextWorks extends App {
  import TextWorksConfig._

  parser.parse(args, Config()).foreach{ config =>
    config.exec.foreach { _.apply(config) }
  }

}
