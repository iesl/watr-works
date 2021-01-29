package org.watrworks
package apps

import java.nio.{file => nio}
import corpora.filesys._
import segment._

sealed trait Processable
sealed trait ProcessableInput extends Processable
sealed trait ProcessedInput extends Processable

object Processable {
  case class SingleFile(f: nio.Path) extends ProcessableInput
  case class ListOfFiles(f: nio.Path) extends ProcessableInput
  case class CorpusRoot(root: nio.Path) extends ProcessableInput
  case class CorpusFile(corpusEntry: CorpusEntry) extends ProcessableInput

  case class ExtractedFile(
    segmentation: DocumentSegmentation,
    input: ProcessableInput
  ) extends ProcessedInput

  case class ExtractedTextGridFile(
    textGridFile: nio.Path,
    input: ProcessableInput
  ) extends ProcessedInput

  def getTextgridOutputFile(input: ProcessableInput, conf: IOConfig): nio.Path = {
    conf.outputPath.getOrElse {
      input match {
        case SingleFile(f) =>
          conf.outputPath.getOrElse {
            nio.Paths.get(f.getFileName().toString() + ".transcript.json")
          }

        case CorpusFile(corpusEntry) =>
          (corpusEntry.getRootPath() / "transcript.json").toNIO

        case _ => ???

        // case ExtractedFile(_, in) =>
        //   getTextgridOutputFile(in, conf)

        // case ExtractedTextGridFile(_, in) => ???
      }
    }
  }

  def withCorpusEntry(input: Processable)(f: CorpusEntry => Unit): Unit = {
    input match {
      case CorpusFile(corpusEntry) => f(corpusEntry)
      case _ =>
    }
  }
}

case class IOConfig(
  inputMode: Option[ProcessableInput] = None,
  outputPath: Option[nio.Path] = None,
  overwrite: Boolean = false,
  pathFilter: Option[String] = None,
  numToRun: Int = Int.MaxValue,
  numToSkip: Int = 0
)
