package org.watrworks
package apps

import ammonite.{ops => fs} // , fs._
import scopt.Read
import shapeless._
import java.nio.{file => nio}
import tracing.VisualTracer
import utils.PathUtils._
import utils.TextOps._
import ProcessPipelineSteps._

object TraceVisConfig {

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
    exec              : Option[(Config) => Unit] = Some(runTraceVis(_))
  )

  val parser = new scopt.OptionParser[Config]("text-works") {
    import scopt._

    override def renderingMode: RenderingMode = RenderingMode.OneColumn

    help("help")

    opt[nio.Path]('c', "corpus") action { (v, conf) =>
      lens[Config].ioConfig.modify(conf) { ioConfig =>
        ioConfig.copy(
          inputMode = Option(Processable.CorpusRoot(v))
        )
      }
    } text ("root path of PDF corpus")

    opt[String]("filter") action { (v, conf) =>
      lens[Config].ioConfig.pathFilter.modify(conf){ m =>
        val f = trimQuotes(v)
        Option(f)
      }
    } text("if specified, only files matching regex will be processed")

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

}


object TraceVis {
  import TraceVisConfig._


  def main(args: Array[String]) = {
    parser.parse(args, Config()).foreach{ config =>
      config.exec.foreach { _.apply(config) }
    }
  }
}
