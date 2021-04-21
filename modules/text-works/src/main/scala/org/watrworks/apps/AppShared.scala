package org.watrworks
package apps

import segment._
import java.nio.{file => nio}
import corpora.filesys._
import utils.DoOrDieHandlers._
import ammonite.{ops => fs} // , fs._
import utils.PathUtils._
import TypeTags._
import utils.Timer.time
import _root_.io.circe, circe._, circe.syntax._

import zio._
import zio.stream._

object ProcessPipelineSteps {
  private[this] val log = org.log4s.getLogger

  val PrettyPrint2Spaces = circe.Printer.spaces2

  type MarkedInput  = Either[ProcessableInput, ProcessableInput]
  type MarkedOutput = Either[String, ProcessedInput]
  type Pipe[A, B]   = Stream[Unit, A] => Stream[Unit, B]

  implicit class RicherZStream[R, E, A](val self: ZStream[R, E, A]) extends AnyVal {
    def through[B](p: Pipe[A, B]): ZStream[R, E, B] = {

      ???
    }
  }

  def initMarkedInput(): Pipe[ProcessableInput, MarkedInput] = {
    _.map { Right(_) }
  }

  def runTextExtractionOnFile(conf: TextWorksConfig.Config): UIO[Unit] = {
    if (conf.runTraceLogging) {
      log.info(s"Visual tracing is enabled; performance will be lowered")
    }

    val processStream = createMarkedInputStream(conf.ioConfig)
      .through(cleanFileArtifacts(conf))
      .through(runSegmentation(conf))
      .through(writeExtractedTextFile(conf))

    processStream.runDrain
  }

  def runTextExtractionPipeline(conf: TextWorksConfig.Config): Unit = {

    val prog = for {
      _ <- createMarkedInputStream(conf.ioConfig)
             .via(dropSkipAndRun(conf.ioConfig))
             .via(cleanFileArtifacts(conf))
             .runDrain

      _ <- createMarkedInputStream(conf.ioConfig)
             .via(dropSkipAndRun(conf.ioConfig))
             .via(cleanFileArtifacts(conf))
             .via(markUnextractedProcessables(conf))
             .via(runSegmentation(conf))
             .via(writeExtractedTextFile(conf))
             // .via(cleanTraceLogArtifacts(conf))
             // .via(writeTraceLogs(conf))
             .runDrain
    } yield ()

    val runtime = Runtime.default
    runtime.unsafeRun(prog)

  }

  def createInputStream(conf: IOConfig): Stream[Nothing, ProcessableInput] = {
    conf.inputMode
      .map { mode =>
        mode match {
          case in @ Processable.SingleFile(_) =>
            Stream(in)

          case Processable.CorpusRoot(corpusRoot) =>
            Corpus(corpusRoot)
              .entryStream()
              .filter { entry =>
                conf.pathFilter
                  .map { filter => entry.entryDescriptor.matches(filter) }
                  .getOrElse { true }
              }
              .map { entry => Processable.CorpusFile(entry) }

          case p @ Processable.CorpusFile(_) =>
            Stream(p)
        }
      }
      .orDie("inputStream(): Invalid input options")
  }

  def createMarkedInputStream(conf: IOConfig): UStream[MarkedInput] = {
    createInputStream(conf)
      .map(Right(_))
  }

  def dropSkipAndRun(conf: IOConfig): Pipe[MarkedInput, MarkedInput] = {
    var skipCount = conf.numToSkip
    var runCount  = conf.numToRun
    _.map {
      case r @ Right(p @ Processable.CorpusFile(corpusEntry @ _)) =>
        if (skipCount > 0) {
          skipCount -= 1;
          Left(p)
        } else if (runCount > 0) {
          runCount -= 1
          r
        } else Left(p)
      case x                                                      => x
    }
  }

  def cleanFileArtifacts(
    conf: TextWorksConfig.Config
  ): Pipe[MarkedInput, MarkedInput] = {
    _.map {
      case Left(inputMode)  => Left(inputMode)
      case Right(inputMode) =>
        val output = Processable.getTextgridOutputFile(inputMode, conf.ioConfig)

        if (output.toFile().exists()) {
          if (conf.ioConfig.overwrite) {
            log.info(s"Overwriting ${output}")
            fs.rm(nioToAmm(output))
            Right(inputMode)
          } else {
            log.info(
              s"Skipping ${inputMode}, output path ${output} already exists"
            )
            log.info(s"use --overwrite to force overwrite")
            Left(inputMode)
          }
        } else {
          Right(inputMode)
        }
    }
  }


  def doSegment1(
    input: Processable,
    conf: TextWorksConfig.Config
  ): Either[String, ProcessedInput] = {
    try {
      input match {
        case m @ Processable.SingleFile(f) =>
          val pdfName  = f.getFileName.toString()
          val documentId = DocumentID(pdfName)
          val input    = nioToAmm(f)

          val output = conf.ioConfig.outputPath.getOrElse {
            nio.Paths.get(f.getFileName().toString() + ".transcript.json")
          }

          if (!output.toFile().exists()) {
            Right(Processable.ExtractedFile(extractText(documentId, input), m))
          } else {
            val msg =
              s"File ${output} already exists. Move file or use --overwrite"
            println(msg)
            Left(msg)
          }

        case m @ Processable.CorpusFile(corpusEntry) =>
          val documentId = DocumentID(corpusEntry.entryDescriptor)

          println(s"Processing ${documentId}")

          val maybeSegmenter = for {
            pdfEntry <- corpusEntry.getPdfArtifact().toRight(left = "Could not get PDF")
            pdfPath  <- pdfEntry.asPath.toEither.left.map(_.toString())
          } yield {

            // val output = Processable.getTextgridOutputFile(m, conf.ioConfig)
            // val ammPath = nioToAmm(output)

            time("extractText") {
              extractText(documentId, pdfPath)
            }
          }

          maybeSegmenter.left
            .map { s =>
              println(s"Error: ${s}")
              s
            }
            .map { s => Processable.ExtractedFile(s, m) }

        case m => Left(s"Unsupported Processable ${m}")
      }
    } catch {
      case t: Throwable =>
        utils.Debugging.printAndSwallow(t)
        Left[String, ProcessedInput](t.toString())
    }
  }

  def getExecutableInfo(): Json = {
    val buildInfoMap =
      buildinfo.BuildInfo.toJson.jsonOrDie("BuildInfo produced invalid Json")
    val now          = System.currentTimeMillis()
    val dtf          = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dtf.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    val nowStr       = dtf.format(new java.util.Date(now))

    buildInfoMap.asObject.get
      .add("runAtTime", nowStr.asJson)
      .remove("builtAtMillis")
      .asJson
  }

  def writeExtractedTextFile(
    conf: TextWorksConfig.Config
  ): Pipe[MarkedOutput, MarkedOutput] = {
    _.map {
      case m @ Right(Processable.ExtractedFile(segmentation, input)) =>
        val outputFile = Processable.getTextgridOutputFile(input, conf.ioConfig)

        val jsonOutput = time("build json output") {
          val transcript = segmentation.createTranscript()
          transcript.asJson
        }

        val writeable = jsonOutput.deepMerge(
          Json.obj("buildInfo" := getExecutableInfo())
        )

        val gridJsStr = writeable.asJson
          .printWith(JsonPrettyPrinter)

        println(s"writing ${outputFile}")

        time(s"write transcript.json") {
          fs.write(outputFile.toFsPath(), gridJsStr)
        }
        m

      case x => x
    }
  }

  // def writeTraceLogs(
  //   conf: TextWorksConfig.Config
  // ): Pipe[MarkedOutput, MarkedOutput] = _.map {
  //   case m @ Right(Processable.ExtractedFile(segmentation, input)) =>
  //     Processable.withCorpusEntry(input) { corpusEntry =>
  //       val traceLogRoot = if (conf.runTraceLogging) {
  //         val traceLogGroup = corpusEntry.ensureArtifactGroup("tracelogs")
  //         Some(traceLogGroup.rootPath)
  //       } else None

  //       traceLogRoot.foreach { rootPath =>
  //         log.trace(s"writing font summary")

  //         val docScopeLogs = segmentation.docScope.emitLogs().asJson

  //         fs.write(
  //           rootPath / "font-summary.json",
  //           docScopeLogs.printWith(PrettyPrint2Spaces)
  //         )

  //         log.trace(s"writing tracelogs")
  //         val pageLogs = segmentation.pageSegmenters.foldLeft(List[Json]()) {
  //           case (accum, pageSegmenter) =>
  //             accum ++ pageSegmenter.emitLogs()
  //         }

  //         fs.write(rootPath / "tracelog.json", pageLogs.asJson.noSpaces)
  //       }
  //     }

  //     m

  //   case x => x
  // }

  def cleanTraceLogArtifacts(
    conf: TextWorksConfig.Config
  ): Pipe[MarkedOutput, MarkedOutput] = {
    _.map {
      case m @ Right(Processable.ExtractedFile(segmentation @ _, input)) =>
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

  def runSegmentation(
    conf: TextWorksConfig.Config
  ): Pipe[MarkedInput, MarkedOutput] =
    inStream => {
      inStream.map {
        case Right(input) => doSegment1(input, conf)
        case Left(input)  => Left(s"Skipping ${input}")
      }
    }

  def markUnextractedProcessables(
    conf: TextWorksConfig.Config
  ): Pipe[MarkedInput, MarkedInput] = _.map { markedInput =>
    markedInput match {
      case Right(input) =>
        input match {
          case m @ Processable.CorpusFile(corpusEntry) =>
            val textGridFile =
              Processable.getTextgridOutputFile(m, conf.ioConfig)
            val isProcessed  = fs.exists(textGridFile.toFsPath())
            if (isProcessed) {
              log.info(s"Already processed ${corpusEntry}: ${textGridFile}")
              Left(input)
            } else {
              Right(input)
            }

          case _ @Processable.SingleFile(filePath @ _) =>
            ???
          case _                                       => ???
        }
      case m            => m
    }
  }

  val JsonPrettyPrinter = circe.Printer(
    dropNullValues = false,
    indent = "  ",
    lbraceRight = "\n",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = "",
    colonRight = " "
  )

  def extractText(
    documentId: String @@ DocumentID,
    inputPdf: fs.Path
  ): DocumentSegmenter = {

    println(s"Extracting ${documentId}")

    val segmenter = time("createSegmenter") {
      DocumentSegmenter.createSegmenter(documentId, inputPdf)
    }

    time("runDocumentSegmentation") {
      segmenter.runDocumentSegmentation()
    }

    segmenter
  }

}