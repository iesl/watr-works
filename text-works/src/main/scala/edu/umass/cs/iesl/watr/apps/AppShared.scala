package edu.umass.cs.iesl.watr
package apps

import java.io.{File => JFile}
import ammonite.{ops => fs}
// import utils.PathUtils._
import java.nio.{file => nio}


sealed trait InputMode

object InputMode {
  case class SingleFile(f: JFile) extends InputMode
  case class ListOfFiles(f: JFile) extends InputMode
  case class CorpusFiles(corpusRoot: JFile) extends InputMode
}

sealed trait OutputMode

object OutputMode {
  case class ToFile(filename: JFile) extends OutputMode
  case class ToInFilePlusExt(ext: String) extends OutputMode
}

sealed trait ArtifactOutput

object ArtifactOutput {
  case object CurrWD extends ArtifactOutput
  case object InputPath extends ArtifactOutput
}

case class IOConfig(
  inputMode: Option[InputMode] = None,
  outputMode: Option[OutputMode] = None,
  artifactOutput: ArtifactOutput = ArtifactOutput.CurrWD,
  overwrite: Boolean = false,
  numToRun: Int = 0,
  numToSkip: Int = 0
)

class IOOptionParser(conf: IOConfig) {

  def inputPaths(): Seq[JFile] = {
    conf.inputMode.map{ mode =>
      mode match {
        case InputMode.SingleFile(f) => Seq(f)
        case _ => sys.error("inputPaths(): TODO")
      }
    }.getOrElse {
      sys.error("inputPaths(): Invalid input options")
    }
  }

  def withWorkingDir(in: fs.Path)(runf: fs.Path => Unit): Unit = {

    val workingDir: nio.Path = conf.artifactOutput match {
      case ArtifactOutput.CurrWD =>
        fs.pwd.toNIO
      case ArtifactOutput.InputPath =>
        in.toNIO.normalize().getParent
    }

    val wd = fs.pwd / fs.RelPath(workingDir)
    runf(wd)
  }

  def withOutputFile(in: fs.Path)(runf: fs.Path => Unit): Unit = {
    withWorkingDir(in) { workingDir =>
      val outPath = conf.outputMode.map{ _ match {
        case OutputMode.ToFile(f) =>
          workingDir /  f.getPath
        case OutputMode.ToInFilePlusExt(ext) =>
          val outpath = in.toIO.getName + s".${ext}"
          workingDir / outpath
      }}

      outPath.foreach { outFile =>
        if (conf.overwrite && fs.exists(outFile) && fs.stat(outFile).isFile) {
          fs.rm(outFile)
        }
        runf(outFile)
      }

    }
  }

  def withOutputDir(in: fs.Path, dir: String)(runf: fs.Path => Unit): Unit = {
    withWorkingDir(in) { workingDir =>
      val outDir = workingDir / dir

      if (!fs.exists(outDir)) {
        fs.mkdir(outDir)
      }
      runf(outDir)
    }
  }
}
