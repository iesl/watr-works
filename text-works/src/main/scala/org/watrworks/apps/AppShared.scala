package org.watrworks
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

import textgrid._


object ProcessPipelineSteps {
  private[this] val log = org.log4s.getLogger

  def runTextExtractionOnFile(conf: TextWorksConfig.Config): Unit = {

    if (conf.runTraceLogging) {
      log.info(s"Visual tracing is enabled")
    } else {
      log.info(s"Visual tracing is disabled")
    }

    val processStream = createInputStream[IO](conf.ioConfig)
      .through(initMarkedInput())
      .through(cleanFileArtifacts(conf))
      .through(runSegmentation(conf))
      .through(writeExtractedTextFile(conf))

    val prog = processStream.compile.drain

    prog.unsafeRunSync()
  }

  def runTextExtractionPipeline(conf: TextWorksConfig.Config): Unit = {

    if (conf.ioConfig.overwrite) {
      println("Cleaning extracted text artifacts")
      val maybeClean = createInputStream[IO](conf.ioConfig)
        .through(initMarkedInput())
        .through(dropSkipAndRun(conf.ioConfig))
        .through(cleanFileArtifacts(conf))

      maybeClean.compile.drain.unsafeRunSync()
    }

    println("Running text extraction")

    val processStream = createInputStream[IO](conf.ioConfig)
      .through(initMarkedInput())
      .through(dropSkipAndRun(conf.ioConfig))
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
        case in@ Processable.SingleFile(f) =>
          fs2.Stream.emit(in).covary[F]

        case Processable.CorpusRoot(corpusRoot) =>
          Corpus(corpusRoot).entryStream[F]()
            .filter { entry =>
              conf.pathFilter.map { filter =>
                entry.entryDescriptor.matches(filter)
              }.getOrElse { true }
            }
            .map{ entry => Processable.CorpusFile(entry) }

        case Processable.ListOfFiles(p) =>
          die("TODO")

        case Processable.CorpusFile(entry) =>
          die("TODO")
      }
    }.orDie("inputStream(): Invalid input options")

  }


  val PrettyPrint2Spaces = circe.Printer.spaces2

  type MarkedInput = Either[ProcessableInput, ProcessableInput]
  type MarkedOutput = Either[String, ProcessedInput]


  def dropSkipAndRun(conf: IOConfig): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    var skipCount = conf.numToSkip
    var runCount = conf.numToRun
    _.map {
      case r@ Right(p@ Processable.CorpusFile(corpusEntry)) =>
        if (skipCount > 0) {
          skipCount -= 1;
          Left(p)
        } else if (runCount > 0) {
          runCount -= 1
          r
        } else Left(p)
      case x => x
    }
  }

  def initMarkedInput(): fs2.Pipe[IO, ProcessableInput, MarkedInput] = {
    _.map { Right(_) }
  }

  def filterInputMatchRegex(regex: Option[String]): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    _.map {
      case r@ Right(p@ Processable.CorpusFile(corpusEntry)) =>
        // println(s"filterInputMatchRegex? ${regex} == ${corpusEntry.entryDescriptor}")
        val filterMatches = regex.map { re =>
          corpusEntry.entryDescriptor.matches(re)
        } getOrElse(true)

        if (filterMatches) {
          r
        } else {
          Left(p)
        }
      case x => x
    }
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
            log.info(s"Skipping ${inputMode}, output path ${output} already exists")
            log.info(s"use --overwrite to force overwrite")
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
            nio.Paths.get(f.getFileName().toString() + ".textgrid.json")
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
          }.map {s =>
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

  def getExecutableInfo(): Json = {
    val buildInfoMap = buildinfo.BuildInfo.toJson.jsonOrDie("BuildInfo produced invalid Json")
    val now = System.currentTimeMillis()
    val dtf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dtf.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    val nowStr = dtf.format(new java.util.Date(now))

    buildInfoMap.asObject.get
      .add("runAtMillis", now.asJson)
      .add("runAtStr", nowStr.asJson)
      .asJson
  }

  def writeExtractedTextFile(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
    _.map {
      case m@ Right(Processable.ExtractedFile(segmentation, input)) =>
        val outputFile = Processable.getTextgridOutputFile(input, conf.ioConfig)

        val jsonOutput = time("build json output") {
          TextGridOutputFormats.jsonOutputGridPerPage(segmentation)
        }

        val writeable = jsonOutput
          .add("buildInfo", getExecutableInfo())

        val gridJsStr = writeable.asJson.printWith(JsonPrettyPrinter)

        println(s"writing ${outputFile}")

        time(s"write textgrid.json") {
          fs.write(outputFile.toFsPath(), gridJsStr)
        }
        m

      case x => x
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
          log.trace(s"writing font summary")

          val docScopeLogs = segmentation.docScope.emitLogs().asJson

          fs.write(
            rootPath / "font-summary.json",
            docScopeLogs.printWith(PrettyPrint2Spaces)
          )

          log.trace(s"writing tracelogs")
          val pageLogs = segmentation.pageSegmenters
            .foldLeft(List[Json]()) {
              case (accum, pageSegmenter) =>
                accum ++ pageSegmenter.emitLogs()
            }

          fs.write(rootPath / "tracelog.json",
            pageLogs.asJson.noSpaces
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


  def markUnextractedProcessables(conf: TextWorksConfig.Config): fs2.Pipe[IO, MarkedInput, MarkedInput] = _.map {
    markedInput => markedInput match {
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

          case m@ Processable.SingleFile(filePath) =>
            ???
          case _ => ???
        }
      case m => m
    }
  }



  val JsonPrettyPrinter = circe.Printer(
    dropNullValues = false,
    indent = "  ",
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
