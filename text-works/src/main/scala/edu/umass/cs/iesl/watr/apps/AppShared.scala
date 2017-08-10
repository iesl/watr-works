package edu.umass.cs.iesl.watr
package apps

// import java.io.{File => JFile}
import java.nio.{file => nio}
import ammonite.{ops => fs}
import corpora.filesys._

sealed trait InputMode

object InputMode {
  case class SingleFile(f: nio.Path) extends InputMode
  case class ListOfFiles(f: nio.Path) extends InputMode
  // case class CorpusFile(corpusEntry: CorpusEntry) extends InputMode
  case class CorpusFile(corpusRoot: nio.Path, corpusEntry: Option[CorpusEntry]) extends InputMode
}

// sealed trait OutputNaming

// object OutputNaming {
//   case class ToFile(filename: nio.Path) extends OutputNaming
//   case object ToStdOut extends OutputNaming
// }

case class IOConfig(
  inputMode: Option[InputMode] = None,
  outputPath: Option[nio.Path] = None,
  // outputNaming: OutputNaming = OutputNaming.ToStdOut,
  overwrite: Boolean = false,
  numToRun: Int = 0,
  numToSkip: Int = 0
)

class IOOptionParser(conf: IOConfig) {
  import fs2._
  import fs2.util.Async
  implicit val S = Strategy.fromCachedDaemonPool()

  val T = implicitly[Async[Task]]


  def inputStream(): Stream[Task, InputMode] = {
    conf.inputMode.map{ mode =>
      mode match {
        case in@ InputMode.SingleFile(f) =>
          Stream.emit(in)

        case InputMode.CorpusFile(corpusRoot, None) =>
          val skip = conf.numToSkip
          val take = conf.numToRun
          val corpus = Corpus(corpusRoot)
          val allEntries = corpus.entryStream()
          val skipped = if (skip > 0) allEntries.drop(skip.toLong) else allEntries
          val entries = if (take > 0) skipped.take(take.toLong) else skipped
          entries.map{ entry =>
            InputMode.CorpusFile(corpusRoot, Some(entry))
          }

        case _ => sys.error("inputStream(): TODO")
      }
    }.getOrElse {
      sys.error("inputStream(): Invalid input options")
    }
  }

  def inputPaths(): Seq[nio.Path] = {
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

    // val workingDir: fs.Path = conf.artifactOutput match {
    //   case ArtifactOutput.CurrWD =>
    //     fs.pwd
    //   case ArtifactOutput.InputPath =>
    //     val normal = in.toNIO.normalize()
    //     if (normal.isAbsolute()) {
    //       fs.Path(normal)
    //     } else {
    //       fs.pwd / fs.RelPath(normal)
    //     }
    // }
    // runf(workingDir)
  }

  def withOutputFile(in: fs.Path)(runf: fs.Path => Unit): Unit = {
    // withWorkingDir(in) { workingDir =>
    //   val outPath = conf.outputMode.map{ _ match {
    //     case OutputNaming.ToFile(f) =>
    //       workingDir /  f.getPath
    //     case OutputNaming.ToInFilePlusExt(ext) =>
    //       val outpath = in.toIO.getName + s".${ext}"
    //       workingDir / outpath
    //   }}

    //   outPath.foreach { outFile =>
    //     if (conf.overwrite && fs.exists(outFile) && fs.stat(outFile).isFile) {
    //       fs.rm(outFile)
    //     }
    //     runf(outFile)
    //   }

    // }
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
