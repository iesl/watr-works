package edu.umass.cs.iesl.watr
package apps

import corpora._

import ammonite.{ops => fs}, fs._
import java.io.{File => JFile}
import segment.DocumentSegmenter
import TypeTags._
import scopt.Read
import shapeless._

sealed trait OutputOption

object OutputOption {
  case object VisualLine extends OutputOption
  case object Dehyphenated extends OutputOption
  case object SuperSubEscaping extends OutputOption
  case object TokenLabeling extends OutputOption

  implicit val OutputOptionRead: Read[OutputOption] =
    Read.reads { _.toLowerCase match {
      case "visual-line"        | "vl"  => VisualLine
      case "dehyphenated"       | "dh"  => Dehyphenated
      case "super-sub-escaping" | "sse" => SuperSubEscaping
      case "token-labeling"     | "tl"  => TokenLabeling
      case s       =>
        throw new IllegalArgumentException(s"""'${s}' is not an output option.""")
    }}
}

object TextWorksConfig {
  case class Config(
    ioConfig: IOConfig = IOConfig(),
    outputOptions: List[OutputOption] = List(),
    exec: Option[(Config) => Unit] = Some((c) => extractText(c))
  )

  val parser = new scopt.OptionParser[Config]("text-works") {
    import scopt._

    override def renderingMode: RenderingMode = RenderingMode.OneColumn

    head("Text Works PDF text extraction, part of the WatrWorks", "0.1")

    note("Run text extraction and analysis on PDFs")

    help("help")

    /// IO Config options
    note("Specify exactly one input mode: corpus|file|file-list \n")

    // val pageIdL = lens[CharAtom].pageRegion.page.pageId
    opt[JFile]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.CorpusInput(v))
      }
    } text ("root path of PDF corpus; output will be written to same dir as input")

    opt[JFile]('i', "input") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.SingleFile(v))
      }
    } text("choose single input PDF")

    opt[JFile]('l', "input-list") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.ListOfFiles(v))
      }
    } text("process list of input PDFs in specified file. Specify '--' to read from stdin. ")

    note("\nOutput file options\n")

    opt[JFile]('o', "output-file") action { (v, conf) =>
      lens[Config].ioConfig.outputMode.modify(conf){ m =>
        Option(OutputMode.ToFile(v))
      }
    } text("""|specify output file. In --corpus mode, ouput will be written to same directory as
              |           input file, otherwise relative to cwd. Use --force to overwrite existing files.""".stripMargin)

    opt[String]('x', "output-ext") action { (v, conf) =>
      lens[Config].ioConfig.outputMode.modify(conf){ m =>
        Option(OutputMode.ToFileExt(v))
      }
    } text("write output to input-file+ext")

    note("\nOutput text layout options: \n")

    opt[OutputOption]('p', "layout-option") action { (v, conf) =>
      conf.copy(outputOptions = v :: conf.outputOptions)
    } text("choose layout options for extracted text [visual-line|dehyphenated|super-sub-escaping|token-labeling]")

    checkConfig{ c =>
      if (c.ioConfig.inputMode.isEmpty) {
        failure("Invalid input options")
      }
      else if (c.ioConfig.outputMode.isEmpty) {
        failure("Invalid output options")
      }
      else success
    }
  }

  def setAction(conf: Config, action: (Config) => Unit): Config = {
    conf.copy(exec=Option(action))
  }

  def extractText(conf: Config): Unit = {
    for {
      pdfFile <- IOOptionParser.inputPaths(conf.ioConfig)
    } {
      val rp = FilePath(pdfFile)
      val filename = rp.last
      // val filepath = rp.segments.dropRight(1)

      val pdfPath = pwd / RelPath(pdfFile)

      println(s"file: $pdfPath")

      val stableId = DocumentID(filename)

      val segmenter = DocumentSegmenter.createSegmenter(stableId, pdfPath, new MemDocZoningApi)

      segmenter.runPageSegmentation()

      conf.ioConfig.outputMode.foreach{ _ match {
        case OutputMode.ToFile(f) =>
          // val content = formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex)
          val content = formats.DocumentIO.documentToPlaintext(segmenter.mpageIndex)
          // println(content)
          val p = fs.Path(f, pwd)
          if (exists(p)) {
            rm(p)
          }
          if (!exists(p)) {
            write(p, content)
          }
      }}
    }
  }
}

object TextWorks extends App {
  import TextWorksConfig._

  // private[this] val log = org.log4s.getLogger


  parser.parse(args, Config()).foreach{ config =>
    config.exec.foreach { _.apply(config) }
  }


}
