package org.watrworks
package apps

import java.nio.{file => nio}
import corpora.filesys._
import segment._
import java.nio.file.{Path => JPath}

sealed trait Pipeable
sealed trait PipeInput  extends Pipeable
sealed trait PipeOutput extends Pipeable

object Pipeable {
  case class SingleFile(f: nio.Path)              extends PipeInput
  case class CorpusFile(corpusEntry: CorpusEntry) extends PipeInput

  case class ExtractedFile(
    segmentation: DocumentSegmenter,
    input: PipeInput
  ) extends PipeOutput

  def getTranscriptOutputFile(
    input: PipeInput,
    conf: InputConfig
  ): nio.Path = {
    conf match {
      case InputConfig.FilteredCorpus(corpusRoot @ _, pathFilters @ _) =>
        input match {
          case SingleFile(f) =>
            nio.Paths.get(f.getFileName().toString() + ".transcript.json")

          case CorpusFile(corpusEntry) =>
            (corpusEntry.getRootPath() / "transcript.json").toNIO
        }
    }
  }
}

sealed trait Action extends Product with Serializable
object Action {
  final case class ExtractFile() extends Action

  final case class ExtractCorpus(
    filteredCorpus: InputConfig.FilteredCorpus
  ) extends Action

  final case class InitCorpus() extends Action

  final case class ImageSeg(
    filteredCorpus: InputConfig.FilteredCorpus
  ) extends Action
}

sealed trait InputConfig extends Product with Serializable
object InputConfig {
  case class PathFilters(
    pathFilter: Option[String],
    numToRun: Int,
    numToSkip: Int,
    overwrite: Boolean
  )

  case class FilteredCorpus(
    corpusRoot: JPath,
    pathFilters: PathFilters
  ) extends InputConfig
}

object BuildMeta {
  val appName    = buildinfo.BuildInfo.appName
  val appVersion = buildinfo.BuildInfo.appVersion
  val commit     = buildinfo.BuildInfo.gitHeadCommit.getOrElse("?")
  val buildTime  = buildinfo.BuildInfo.builtAtMillis

  val dtFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  // dtFormat.setTimeZone(java.util.TimeZone.getTimeZone(timezone))
  val timeStr = dtFormat.format(buildTime)

  val buildStr = s"""| version    : ${appVersion}
                     | git commit : ${commit}
                     | build time : ${timeStr}
                     |""".stripMargin
}
