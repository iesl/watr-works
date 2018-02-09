package edu.umass.cs.iesl.watr
package apps

import segment._
import java.nio.{file => nio}
import corpora.filesys._
import cats.effect._
import utils.DoOrDieHandlers._
import ammonite.{ops => fs} // , fs._
import _root_.io.circe, circe._, circe.syntax._
import utils.PathUtils._
import TypeTags._
import utils.Timer.time
import formats._
import corpora._


sealed trait InputMode

object InputMode {
  case class SingleFile(f: nio.Path) extends InputMode
  case class ListOfFiles(f: nio.Path) extends InputMode
  case class CorpusFile(corpusRoot: nio.Path) extends InputMode


}

sealed trait Processable
sealed trait ProcessableInput extends Processable
sealed trait ProcessedInput extends Processable

object Processable {
  case class SingleFile(f: nio.Path) extends ProcessableInput
  case class CorpusFile(corpusEntry: CorpusEntry) extends ProcessableInput

  case class ExtractedFile(
    segmentation: DocumentSegmentation,
    input: ProcessableInput
  ) extends ProcessedInput

  def getTextgridOutputFile(input: Processable, conf: IOConfig): nio.Path = {
    conf.outputPath.getOrElse {
      input match {
        case SingleFile(f) =>
          conf.outputPath.getOrElse {
            nio.Paths.get(f.getFileName() + ".textgrid.json")
          }
        case CorpusFile(_) =>
          nio.Paths.get("textgrid.json")

        case ExtractedFile(_, in) =>
          getTextgridOutputFile(in, conf)
      }
    }
  }
}

case class IOConfig(
  inputMode: Option[InputMode] = None,
  outputPath: Option[nio.Path] = None,
  overwrite: Boolean = false,
  pathFilter: Option[String] = None,
  numToRun: Int = Int.MaxValue,
  numToSkip: Int = 0
)


object ProcessPipelineSteps {
  private[this] val log = org.log4s.getLogger

  def createInputStream[F[_]: Effect](conf: IOConfig): fs2.Stream[F, ProcessableInput] = {
    conf.inputMode.map{ mode =>

      mode match {
        case in@ InputMode.SingleFile(f) =>
          fs2.Stream.emit(Processable.SingleFile(f)).covary[F]

        case InputMode.CorpusFile(corpusRoot) =>
          Corpus(corpusRoot).entryStream[F]()
            .filter { entry =>
              conf.pathFilter.map { filter =>
                entry.entryDescriptor.matches(filter)
              }.getOrElse { true }
            }
            .map{ entry => Processable.CorpusFile(entry) }

        case InputMode.ListOfFiles(p) =>
          die("TODO")
      }
    }.orDie("inputStream(): Invalid input options")

  }


  val PrettyPrint2Spaces = circe.Printer.spaces2

  type MarkedInput = Either[ProcessableInput, ProcessableInput]
  type RunInput    = Right[ProcessableInput, ProcessableInput]
  type SkipInput   = Left[ProcessableInput, ProcessableInput]

  def cleanOldArtifacts(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    inStream => {
      inStream.map {
        case Left(inputMode) => Left(inputMode)
        case Right(inputMode) =>
          val output = Processable.getTextgridOutputFile(inputMode, conf.ioConfig)

          if (output.toFile().exists()) {
            if (conf.ioConfig.overwrite) {
              log.info(s"Overwriting ${output}")
              fs.rm(nioToAmm(output))
              Right(inputMode)
            } else {
              Left(inputMode)
            }
          } else {
            Right(inputMode)
          }
      }
    }
  }

  def doSegment1(input: Processable, conf: TextWorksConfig.Config): Either[String, ProcessedInput] = {
    try {
      input match {
        case m@ Processable.SingleFile(f) =>

          val pdfName = f.getFileName.toString()
          val stableId = DocumentID(pdfName)
          val input = nioToAmm(f)

          val output = conf.ioConfig.outputPath.getOrElse {
            nio.Paths.get(f.getFileName() + ".textgrid.json")
          }

          if (!output.toFile().exists()) {
            Right(Processable.ExtractedFile(
              extractText(stableId, input, nioToAmm(output), None), m
            ))
          } else {
            val msg = s"File ${output} already exists. Move file or use --overwrite"
            println(msg)
            Left(msg)
          }


        case m@ Processable.CorpusFile(corpusEntry) =>

          val stableId = DocumentID(corpusEntry.entryDescriptor)

          println(s"Processing ${stableId}")

          val maybeSegmenter = for {
            pdfEntry <- corpusEntry.getPdfArtifact.toRight(left="Could not get PDF")
            pdfPath <- pdfEntry.asPath.toEither.left.map(_.toString())
          } yield {

            val traceLogRoot = if (conf.runTraceLogging) {
              val traceLogGroup = corpusEntry.ensureArtifactGroup("tracelogs")
              traceLogGroup.deleteGroupArtifacts()
              Some(traceLogGroup.rootPath)
            } else None

            val output = Processable.getTextgridOutputFile(m, conf.ioConfig)
            val ammPath = nioToAmm(output)

            time("extractText") {
              extractText(stableId, pdfPath, ammPath, traceLogRoot)
            }
          }
          maybeSegmenter.left.map { s =>
            println(s"Error: ${s}")
            s
          }.right.map {s =>
            Processable.ExtractedFile(
              s, m
            )
          }


        case m => Left(s"Unsupported Processable ${m}")
      }
    } catch {
      case t: Throwable =>
        utils.Debugging.printAndSwallow(t)
        Left[String, ProcessedInput](t.toString())
    }

  }

  def writeExtractedTextFile(conf: TextWorksConfig.Config): fs2.Pipe[IO, Either[String, ProcessedInput], Either[String, ProcessedInput]] = {
    inStream => {
      inStream.map {
        case m@ Right(Processable.ExtractedFile(segmentation, input)) =>
          // val textGridFile = Processable.getTextgridOutputFile(input, conf.ioConfig)


          m

        case x => x
      }
    }
  }

  def runSegmentation(conf: TextWorksConfig.Config): fs2.Pipe[IO, Either[ProcessableInput, ProcessableInput], Either[String, ProcessedInput]] =
    inStream => {
      inStream.map {
        case Left(input) => Left("Skipping")
        case Right(input) => doSegment1(input, conf)
      }
    }


  def markUnextractedProcessables(conf: TextWorksConfig.Config): fs2.Pipe[IO, ProcessableInput, Either[ProcessableInput, ProcessableInput]] = {
    s => s.map { input =>
      input match {
        case m@ Processable.CorpusFile(corpusEntry) =>
          val textGridFile = Processable.getTextgridOutputFile(m, conf.ioConfig)
          val isProcessed = fs.exists(textGridFile.toFsPath())
          if (isProcessed) {
            log.info(s"Already processed ${corpusEntry}")
            Left(input)
          } else {
            Right(input)
          }

        case _ => Left(input)
      }
    }
  }


  def runTextExtractionPipeline(conf: TextWorksConfig.Config): Unit = {

    val processStream = createInputStream[IO](conf.ioConfig)
      .through(markUnextractedProcessables(conf))
      .through(cleanOldArtifacts(conf))
      .through(runSegmentation(conf))


    val prog = processStream.compile.drain

    prog.unsafeRunSync()
  }

  val jsonPrinter = circe.Printer(
    preserveOrder = true,
    dropNullValues = false,
    indent = "    ",
    lbraceRight = "",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = " ",
    colonRight = " "
  )

  def extractText(
    stableId: String@@DocumentID,
    inputPdf: fs.Path,
    textOutputFile: fs.Path,
    traceLogRoot: Option[fs.Path]
  ): DocumentSegmentation = {

    println(s"Extracting ${stableId}")

    val zoningApi = new MemDocZoningApi

    val segmenter = DocumentSegmenter.createSegmenter(stableId, inputPdf, zoningApi)

    time("runDocumentSegmentation") {
      segmenter.runDocumentSegmentation()
    }

    val jsonOutput = time("build json output") {  TextGridOutputFormats.jsonOutputGridPerPage(segmenter) }

    val gridJsStr = jsonOutput.pretty(jsonPrinter)

    time("write textgrid.json") {
      log.trace(s"writing textgrid.json to ${textOutputFile}")
      fs.write(textOutputFile, gridJsStr)
    }

    traceLogRoot.foreach { rootPath =>
      println("writing tracelogs")

      fs.ls(rootPath)
        .foreach{ p => fs.rm(p) }


      val allLogs = segmenter.pageSegmenters
        .foldLeft(List[Json]()) {
          case (accum, pageSegmenter) =>
            accum ++ pageSegmenter.emitLogs()
        }

      val jsonLogs = allLogs.asJson
      val jsonStr = jsonLogs.pretty(PrettyPrint2Spaces)

      fs.write(rootPath / "tracelog.json", jsonStr)
    }

    segmenter
  }

}

