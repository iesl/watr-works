package edu.umass.cs.iesl.watr
package apps

import corpora._

import ammonite.{ops => fs}, fs._
import java.io.{File => JFile}
import segment.DocumentSegmenter
import TypeTags._
import scopt.Read

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

import shapeless._

object TextWorks extends App {

  // private[this] val log = org.log4s.getLogger

  case class Config(
    ioConfig: IOConfig = IOConfig(),
    outputOptions: List[OutputOption] = List(),
    exec: Option[(Config) => Unit] = None
  )



  val parser = new scopt.OptionParser[Config]("text-works") {
    head("Text Works PDF text extraction, part of the WatrWorks suite", "0.1")

    note("Run text extraction and analysis on PDFs")

    help("usage")

    opt[OutputOption]('p', "output-option") action { (v, conf) =>
      conf.copy(outputOptions = v :: conf.outputOptions)
    } text("choose single input file")

    cmd("totext")
      .action((v, conf) => setAction(conf, extractText(_)))
      .text ("run basic text extraction")

    /// IO Config options
    note("Specify exactly one input option")

    // val pageIdL = lens[CharAtom].charRegion.page.pageId
    opt[JFile]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.CorpusInput(v))
      }
    } text ("root path of PDF corpus")

    opt[JFile]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.CorpusInput(v))
      }
    } text ("root path of PDF corpus")

    opt[JFile]('i', "input") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.PdfFile(v))
      }
    } text("single input PDF")

    opt[JFile]('l', "input-list") action { (v, conf) =>
      lens[Config].ioConfig.inputMode.modify(conf){ m =>
        Option(InputMode.PdfFileList(v))
      }
    } text("process PDFs listed in specified file. Specify '--' to read from stdin")

    note("Output options")

    opt[JFile]('o', "output-file") action { (v, conf) =>
      lens[Config].ioConfig.outputMode.modify(conf){ m =>
        Option(OutputMode.ToFile(v))
      }
    } text("choose single input file")

    opt[String]('x', "output-ext") action { (v, conf) =>
      lens[Config].ioConfig.outputMode.modify(conf){ m =>
        Option(OutputMode.ToFileExt(v))
      }
    } text("choose single input file")

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

  parser.parse(args, Config()).foreach{ config =>
    config.exec.foreach { _.apply(config) }
  }


  def setAction(conf: Config, action: (Config) => Unit): Config = {
    conf.copy(exec=Option(action))
  }


  def extractText(conf: Config): Unit = {
    println("extracting text")
    for {
      pdfFile <- IOOptionParser.inputPaths(conf.ioConfig)
    } {
      val pdfPath = pwd / RelPath(pdfFile)
      println(s"file: $pdfPath")

      val stableId = DocumentID(pdfPath.toString())

      val segmenter = DocumentSegmenter.createSegmenter(stableId, pdfPath, new MemDocZoningApi)

      segmenter.runPageSegmentation()

      conf.ioConfig.outputMode.foreach{ _ match {
        case OutputMode.ToFile(f) =>
          val content = formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex, Seq())
          val p = fs.Path(f, pwd)
          write(p, content)
      }}
    }
  }
}
