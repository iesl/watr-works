package edu.umass.cs.iesl.watr
package apps

import corpora._

import ammonite.{ops => fs} // , fs._
import scopt.Read
import shapeless._
import java.nio.{file => nio}
import tracing.VisualTracer
import utils.PathUtils._
import utils.TextOps._

import ProcessPipelineSteps._

object TextWorksConfig {

  // Taken from logging output:
  // To get higher rendering speed on JDK8 or later,
  //   use the option -Dsun.java2d.cmm=sun.java2d.cmm.kcms.KcmsServiceProvider
  //   or call System.setProperty("sun.java2d.cmm", "sun.java2d.cmm.kcms.KcmsServiceProvider")
  System.setProperty("sun.java2d.cmm", "sun.java2d.cmm.kcms.KcmsServiceProvider")

  implicit val NioPath: Read[nio.Path] =
    Read.reads { v =>
      nio.Paths.get(v).toAbsolutePath().normalize()
    }

  case class Config(
    ioConfig          : IOConfig = IOConfig(),

    initCorpus        : Option[nio.Path] = None,
    runTraceLogging   : Boolean = VisualTracer.tracingEnabled(),
    exec              : Option[(Config) => Unit] = Some((c) => runTextExtractionPipeline(c))
  )

  val parser = new scopt.OptionParser[Config]("text-works") {
    import scopt._

    override def renderingMode: RenderingMode = RenderingMode.OneColumn

    head("TextWorks PDF text extraction, part of WatrWorks", "0.1")

    note("Run text extraction and analysis on PDFs")

    help("help")

    // IO Config options
    note("Specify exactly one input mode: corpus|file|file-list \n")


    opt[nio.Path]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.modify(conf) { ioConfig =>
        ioConfig.copy(
          inputMode = Option(InputMode.CorpusFile(v))
        )
      }
    } text ("root path of PDF corpus; output will be written to same dir as input")

    // opt[nio.Path]('i', "input") action { (v, conf) =>
    //   lens[Config].ioConfig.inputMode.modify(conf){ m =>
    //     Option(InputMode.SingleFile(v))
    //   }
    // } text("choose single input PDF")

    // opt[nio.Path]('l', "input-list") action { (v, conf) =>
    //   lens[Config].ioConfig.inputMode.modify(conf){ m =>
    //     Option(InputMode.ListOfFiles(v))
    //   }
    // } text("process list of input PDFs in specified file.")

    opt[String]("filter") action { (v, conf) =>
      lens[Config].ioConfig.pathFilter.modify(conf){ m =>
        val f = trimQuotes(v)
        Option(f)
      }
    } text("if specified, only files matching regex will be processed")


    note("\nOutput file options\n")

    // opt[nio.Path]('o', "output-file") action { (v, conf) =>
    //   lens[Config].ioConfig.outputPath.modify(conf){ m =>
    //     Some(v)
    //   }
    // } text("""|specify output file. In --corpus mode, ouput will be written to same directory as
    //           |           input file, otherwise relative to cwd. Use --overwrite to overwrite existing files.""".stripMargin)

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

    checkConfig{ c =>
      if (c.initCorpus.isDefined) {
        val corpusRoot = c.initCorpus.get
        if (fs.exists(corpusRoot.toFsPath())) {
          success
        } else {
          failure(s"Corpus root ${corpusRoot} doesn't exist")
        }
      } else {
        c.ioConfig.inputMode.map { _  match {
          case InputMode.SingleFile(f) =>
            if (fs.exists(f.toFsPath())) success else failure(s"file ${f} not found")
          case InputMode.ListOfFiles(f) =>
            if (fs.exists(f.toFsPath())) success else failure(s"file ${f} not found")
          case InputMode.CorpusFile(f) =>
            if (fs.exists(f.toFsPath())) success else failure(s"corpus root ${f} not found")
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


object TextWorks {
  import TextWorksConfig._

  def main(args: Array[String]) = {
    parser.parse(args, Config()).foreach{ config =>
      config.exec.foreach { _.apply(config) }
    }
  }


}
