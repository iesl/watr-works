package org.watrworks
package apps

import corpora._

import ammonite.{ops => fs} // , fs._
import scopt.Read
import shapeless._
import java.nio.{file => nio}
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
    exec              : Option[(Config) => Unit] = Some((c) => runTextExtractionPipeline(c))
  )

  val appName = buildinfo.BuildInfo.appName
  val appVersion = buildinfo.BuildInfo.appVersion
  val commit = buildinfo.BuildInfo.gitHeadCommit.getOrElse("?")
  val buildTime = buildinfo.BuildInfo.builtAtMillis
  val timezone = buildinfo.BuildInfo.timezone

  val dtFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  dtFormat.setTimeZone(java.util.TimeZone.getTimeZone(timezone))
  val timeStr = dtFormat.format(buildTime)


  val buildStr = s"""|
                     | version    : ${appVersion}
                     | git commit : ${commit}
                     | build time : ${timeStr}
                     |""".stripMargin

  val parser = new scopt.OptionParser[Config]("text-works") {
    import scopt._

    override def renderingMode: RenderingMode = RenderingMode.OneColumn

    head("TextWorks PDF text extraction, part of WatrWorks", buildStr)

    note("Run text extraction and analysis on PDFs")

    help("help")

    // IO Config options
    note("Specify input mode: corpus|file \n")

    opt[nio.Path]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.modify(conf) { ioConfig =>
        ioConfig.copy(
          inputMode = Option(Processable.CorpusRoot(v))
        )
      }
    } text ("root path of PDF corpus; output will be written to same dir as input")


    opt[String]("filter") action { (v, conf) =>
      lens[Config].ioConfig.pathFilter.modify(conf){ m =>
        val f = trimQuotes(v)
        Option(f)
      }
    } text("if specified, only files matching regex will be processed")

    note("\nOutput file options\n")

    opt[nio.Path]('o', "output") action { (v, conf) =>
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

    opt[Unit]("image-seg") action { (v, conf) =>
      setAction(conf, runImageSegPipeline(_))
    } text ("run image-based segmentation")

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
          case Processable.SingleFile(f) =>
            if (fs.exists(f.toFsPath())) success else failure(s"file ${f} not found")

          case Processable.CorpusFile(entry @ _) =>
            ???

          case Processable.CorpusRoot(rootPath) =>
            if (fs.exists(rootPath.toFsPath())) success else failure(s"corpus root ${rootPath} not found")

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
