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
  case class CorpusFile(corpusRoot: nio.Path, corpusEntry: Option[CorpusEntry]) extends InputMode
}

case class IOConfig(
  inputMode: Option[InputMode] = None,
  outputPath: Option[nio.Path] = None,
  overwrite: Boolean = false,
  pathFilter: Option[String] = None,
  numToRun: Int = 0,
  numToSkip: Int = 0
)

class IOOptionParser(conf: IOConfig) {
  import fs2._

  import cats.effect.Async
  import cats.effect.IO
  // implicit val S = Strategy.fromCachedDaemonPool()

  val T = implicitly[Async[IO]]


  def inputStream(): Stream[IO, InputMode] = {
    conf.inputMode.map{ mode =>
      mode match {
        case in@ InputMode.SingleFile(f) =>

          Stream.emit(in).covary[IO]

        case InputMode.CorpusFile(corpusRoot, None) =>
          val skip = conf.numToSkip
          val take = conf.numToRun
          val corpus = Corpus(corpusRoot)
          val allEntries = corpus.entryStream()
          val filteredEntries = allEntries.filter { entry =>
            conf.pathFilter.map { filter =>
              entry.entryDescriptor.matches(filter)
            }.getOrElse { true }
          }
          val skipped = if (skip > 0) filteredEntries.drop(skip.toLong) else filteredEntries
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


  // Decide if the specified output artifact exists, or if the --force option is specified
  def maybeProcess(entry: CorpusEntry, artifactOutputName: String): Option[nio.Path] = {
    val outputPath = entry.getArtifactPath(artifactOutputName).toNIO
    if (entry.hasArtifact(artifactOutputName)) {
      if (conf.overwrite) {
        entry.deleteArtifact(artifactOutputName)
        Some(outputPath)
      } else None
    } else Some(outputPath)
  }

  def maybeProcess(path: nio.Path): Option[nio.Path] = {
    val ammPath = PathConversions.nioToAmm(path)
    if (fs.exists(ammPath) && fs.stat(ammPath).isFile) {
      if (conf.overwrite) {
        fs.rm(ammPath)
        Some(ammPath.toNIO)
      } else None
    } else Some(path)
  }
}

object PathConversions {

  def nioToAmm(nioPath: nio.Path): fs.Path = {
    fs.FilePath(nioPath) match {
      case p: fs.Path =>  p
      case p: fs.RelPath => fs.pwd / p
    }
  }

}
