package edu.umass.cs.iesl.watr
package apps

import segment._
import java.nio.{file => nio}
import corpora.filesys._
import cats.effect._
import utils.DoOrDieHandlers._
import ammonite.{ops => fs} // , fs._
import _root_.io.circe // circe._, circe.syntax._
import utils.PathUtils._
import TypeTags._
import utils.Timer.time
import formats._
import corpora._
import _root_.io.circe, circe._, circe.syntax._


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

  case class ExtractedTextGridFile(
    textGridFile: nio.Path,
    input: ProcessableInput
  ) extends ProcessedInput

  def getTextgridOutputFile(input: Processable, conf: IOConfig): nio.Path = {
    conf.outputPath.getOrElse {
      input match {
        case SingleFile(f) =>
          conf.outputPath.getOrElse {
            nio.Paths.get(f.getFileName() + ".textgrid.json")
          }

        case CorpusFile(corpusEntry) =>
          (corpusEntry.getRootPath() / "textgrid.json").toNIO

        case ExtractedFile(_, in) =>
          getTextgridOutputFile(in, conf)
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
  inputMode: Option[InputMode] = None,
  outputPath: Option[nio.Path] = None,
  overwrite: Boolean = false,
  pathFilter: Option[String] = None,
  numToRun: Int = Int.MaxValue,
  numToSkip: Int = 0
)


object ProcessPipelineSteps {
  private[this] val log = org.log4s.getLogger

  def runTextExtractionPipeline(conf: TextWorksConfig.Config): Unit = {

    val processStream = createInputStream[IO](conf.ioConfig)
      .through(initMarkedInput())
      .through(cleanFileArtifacts(conf))
      .through(markUnextractedProcessables(conf))
      .through(runSegmentation(conf))
      .through(writeExtractedTextFile(conf))
      .through(cleanTraceLogArtifacts(conf))
      .through(writeTraceLogs(conf))

    val prog = processStream.compile.drain

    prog.unsafeRunSync()
  }

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
  type MarkedOutput = Either[String, ProcessedInput]

  def initMarkedInput(): fs2.Pipe[IO, ProcessableInput, MarkedInput] = {
    _.map { Right(_) }
  }

  def pickupTextgridFiles(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedInput, MarkedOutput] = {
    _.map {
      case Left(inputMode) => Left("no textgrid.json")
      case Right(inputMode) =>
        val textgridFile = Processable.getTextgridOutputFile(inputMode, conf.ioConfig)


        if (textgridFile.toFile().exists()) {
          Right(Processable.ExtractedTextGridFile(textgridFile, inputMode))
        } else {
          Left("no textgrid.json")
        }
    }
  }
  def cleanFileArtifacts(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    _.map {
      case Left(inputMode) => Left(inputMode)
      case Right(inputMode) =>
        val output = Processable.getTextgridOutputFile(inputMode, conf.ioConfig)

        if (output.toFile().exists()) {
          if (conf.ioConfig.overwrite) {
            log.info(s"Overwriting ${output}")
            fs.rm(nioToAmm(output))
            Right(inputMode)
          } else {
            log.info(s"Skipping ${output}")
            Left(inputMode)
          }
        } else {
          Right(inputMode)
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
            Right(
              Processable.ExtractedFile(extractText(stableId, input), m)
            )
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


            // val output = Processable.getTextgridOutputFile(m, conf.ioConfig)
            // val ammPath = nioToAmm(output)

            time("extractText") {
              extractText(stableId, pdfPath)
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

  def writeExtractedTextFile(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
    inStream => {
      inStream.map {
        case m@ Right(Processable.ExtractedFile(segmentation, input)) =>
          val outputFile = Processable.getTextgridOutputFile(input, conf.ioConfig)

          val jsonOutput = time("build json output") {  TextGridOutputFormats.jsonOutputGridPerPage(segmentation) }

          val gridJsStr = jsonOutput.noSpaces

          time("write textgrid.json") {
            log.trace(s"writing textgrid.json to ${outputFile}")
            fs.write(outputFile.toFsPath(), gridJsStr)
          }
          m

        case x => x
      }
    }
  }


  def writeTraceLogs(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = _.map {
    case m@ Right(Processable.ExtractedFile(segmentation, input)) =>

      Processable.withCorpusEntry(input) { corpusEntry =>
        val traceLogRoot = if (conf.runTraceLogging) {
          val traceLogGroup = corpusEntry.ensureArtifactGroup("tracelogs")
          Some(traceLogGroup.rootPath)
        } else None

        traceLogRoot.foreach { rootPath =>
          println("writing font summary")

          val docScopeLogs = segmentation.docScope.emitLogs().asJson

          fs.write(
            rootPath / "font-summary.json",
            docScopeLogs.pretty(PrettyPrint2Spaces)
          )

          val pageLogs = segmentation.pageSegmenters
            .foldLeft(List[Json]()) {
              case (accum, pageSegmenter) =>
                accum ++ pageSegmenter.emitLogs()
            }

          fs.write(rootPath / "tracelog.json",
            pageLogs.asJson.pretty(PrettyPrint2Spaces)
          )
        }
      }

      m

    case x => x
  }

  def cleanTraceLogArtifacts(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
      _.map {
        case m@ Right(Processable.ExtractedFile(segmentation, input)) =>

          Processable.withCorpusEntry(input) { corpusEntry =>

            if (conf.runTraceLogging) {
              println("cleaning tracelogs")
              val group = corpusEntry.ensureArtifactGroup("tracelogs")
              group.deleteGroupArtifacts()
            }
          }

          m

        case x => x
      }
  }

  def runSegmentation(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedInput, MarkedOutput] =
    inStream => {
      inStream.map {
        case Right(input) => doSegment1(input, conf)
        case Left(input)  => Left(s"Skipping ${input}")
      }
    }


  def markUnextractedProcessables(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    s => s.map { minput =>
      minput match {
        case Right(input) =>

          input match {
            case m@ Processable.CorpusFile(corpusEntry) =>
              val textGridFile = Processable.getTextgridOutputFile(m, conf.ioConfig)
              val isProcessed = fs.exists(textGridFile.toFsPath())
              if (isProcessed) {
                log.info(s"Already processed ${corpusEntry}: ${textGridFile}")
                Left(input)
              } else {
                Right(input)
              }

          }
        case m => m
      }
    }
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
    inputPdf: fs.Path
  ): DocumentSegmentation = {

    println(s"Extracting ${stableId}")

    val zoningApi = new MemDocZoningApi

    val segmenter = time("createSegmenter") {
        DocumentSegmenter.createSegmenter(stableId, inputPdf, zoningApi)
      }

    time("runDocumentSegmentation") {
      segmenter.runDocumentSegmentation()
    }

    segmenter
  }

}
