package org.watrworks
package apps

import segment._
import corpora.filesys._
import utils.DoOrDieHandlers._
import ammonite.{ops => fs} // , fs._
import utils.PathUtils._
import TypeTags._
import utils.Timer.time
import _root_.io.circe, circe._, circe.syntax._
import imageseg._
import java.nio.file.{
  Paths => JPaths
}

import zio._
import zio.stream._

object Pipelines {
  private[this] val log = org.log4s.getLogger

  val PrettyPrint2Spaces = circe.Printer.spaces2

  type MarkedInput  = Either[PipeInput, PipeInput]
  type MarkedOutput = Either[String, PipeOutput]
  type Pipe[A, B]   = Stream[Unit, A] => Stream[Unit, B]

  def initMarkedInput(): Pipe[PipeInput, MarkedInput] = {
    _.map { Right(_) }
  }

  def runTextExtractionPipeline(conf: Action.ExtractCorpus): Unit = {
    val prog = for {
      _ <- createMarkedInputStream(conf.filteredCorpus)
             .via(dropSkipAndRun(conf.filteredCorpus))
             .via(cleanFileArtifacts(conf.filteredCorpus))
             .runDrain

      _ <- createMarkedInputStream(conf.filteredCorpus)
             .via(dropSkipAndRun(conf.filteredCorpus))
             .via(cleanFileArtifacts(conf.filteredCorpus))
             .via(markUnextractedPipeables(conf.filteredCorpus))
             .via(runSegmentation(conf.filteredCorpus))
             .via(writeExtractedTextFile(conf.filteredCorpus))
             .runDrain
    } yield ()

    val runtime = Runtime.default
    runtime.unsafeRun(prog)
  }

  def runImageSegPipeline(conf: Action.ImageSeg): Unit = {
    val prog = for {
      _ <- createMarkedInputStream(conf.filteredCorpus)
             .via(dropSkipAndRun(conf.filteredCorpus))
             .via(segmentPageImages(conf.filteredCorpus))
             .runDrain
    } yield ()

    val runtime = Runtime.default
    runtime.unsafeRun(prog)

  }

  def createInputStream(conf: InputConfig): Stream[Nothing, PipeInput] = {
    conf match {
      case InputConfig.FilteredCorpus(corpusRoot, pathFilters) =>
        Corpus(corpusRoot)
          .entryStream()
          .filter { entry =>
            pathFilters.pathFilter
              .map { filter => entry.entryDescriptor.matches(filter) }
              .getOrElse { true }
          }
          .map { entry => Pipeable.CorpusFile(entry) }

    }
  }

  def createMarkedInputStream(conf: InputConfig): UStream[MarkedInput] = {
    createInputStream(conf)
      .map(Right(_))
  }

  def dropSkipAndRun(conf: InputConfig.FilteredCorpus): Pipe[MarkedInput, MarkedInput] = {
    var numToSkip = conf.pathFilters.numToSkip
    var numToRun  = conf.pathFilters.numToRun

    _.map {
      case r @ Right(p @ Pipeable.CorpusFile(corpusEntry @ _)) =>
        if (numToSkip > 0) {
          numToSkip -= 1;
          Left(p)
        } else if (numToRun > 0) {
          numToRun -= 1
          r
        } else Left(p)
      case x => x
    }
  }

  def cleanFileArtifacts(
    inConf: InputConfig
  ): Pipe[MarkedInput, MarkedInput] = {
    _.map {
      case Left(inputMode) => Left(inputMode)
      case Right(inputMode) =>
        inConf match {
          case InputConfig.FilteredCorpus(_, pathFilters) =>
            val output = Pipeable.getTranscriptOutputFile(inputMode, inConf)

            if (output.toFile().exists()) {
              if (pathFilters.overwrite) {
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
  }

  def generatePageImages(
    conf: InputConfig
  ): Pipe[MarkedInput, MarkedInput] = _.map {
    case l @ Left(_) => l
    case r @ Right(inputMode @ _) =>
      r
  }

  def segmentPageImages(
    conf: InputConfig
  ): Pipe[MarkedInput, MarkedInput] = _.map {
    case l @ Left(_) => l
    case Right(input) =>
      input match {
        case m @ Pipeable.CorpusFile(corpusEntry) =>
          for {
            group     <- corpusEntry.getArtifactGroup("page-images")
            pageImage <- group.getArtifacts()
          } {
            LineDetection.runLineDetection(
              pageImage.artifactPath.toString()
            )
          }

          Right(m)

        case m => Right(m)
      }
  }
  def doSegment1(
    input: Pipeable,
    conf: InputConfig
  ): Either[String, PipeOutput] = {
    try {
      input match {
        case m @ Pipeable.SingleFile(f) =>
          val pdfName    = f.getFileName.toString()
          val documentId = DocumentID(pdfName)
          val input      = nioToAmm(f)

          val output = Pipeable.getTranscriptOutputFile(m, conf)

          if (!output.toFile().exists()) {
            Right(Pipeable.ExtractedFile(extractText(documentId, input), m))
          } else {
            val msg =
              s"File ${output} already exists. Move file or use --overwrite"
            println(msg)
            Left(msg)
          }

        case m @ Pipeable.CorpusFile(corpusEntry) =>
          val documentId = DocumentID(corpusEntry.entryDescriptor)

          println(s"Processing ${documentId}")

          val maybeSegmenter = for {
            pdfEntry <- corpusEntry.getPdfArtifact().toRight(left = "Could not get PDF")
            pdfPath  <- pdfEntry.asPath.toEither.left.map(_.toString())
          } yield {

            time("extractText") {
              extractText(documentId, pdfPath)
            }
          }

          maybeSegmenter.left
            .map { s =>
              println(s"Error: ${s}")
              s
            }
            .map { s => Pipeable.ExtractedFile(s, m) }

        case m => Left(s"Unsupported Pipeable ${m}")
      }
    } catch {
      case t: Throwable =>
        utils.Debugging.printAndSwallow(t)
        Left[String, PipeOutput](t.toString())
    }
  }

  def getExecutableInfo(): Json = {
    val buildInfoMap =
      buildinfo.BuildInfo.toJson.jsonOrDie("BuildInfo produced invalid Json")
    val now = System.currentTimeMillis()
    val dtf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    // dtf.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    val nowStr = dtf.format(new java.util.Date(now))

    buildInfoMap.asObject.get
      .add("runAtTime", nowStr.asJson)
      .remove("builtAtMillis")
      .asJson
  }

  def writeExtractedTextFile(
    conf: InputConfig
  ): Pipe[MarkedOutput, MarkedOutput] = {
    _.map {
      case m @ Right(Pipeable.ExtractedFile(segmentation, input)) =>
        val outputFile = Pipeable.getTranscriptOutputFile(input, conf)

        val jsonOutput = time("build json output") {
          val transcript = segmentation.createTranscript()
          transcript.asJson
        }

        val writeable = jsonOutput.deepMerge(
          Json.obj("buildInfo" := getExecutableInfo())
        )

        val gridJsStr = writeable.asJson
          .printWith(JsonPrettyPrinter)

        time(s"write transcript.json") {
          fs.write(outputFile.toFsPath(), gridJsStr)
        }
        m

      case x => x
    }
  }

  def runSegmentation(
    conf: InputConfig
  ): Pipe[MarkedInput, MarkedOutput] = _.map {
    case Right(input) => doSegment1(input, conf)
    case Left(input)  => Left(s"Skipping ${input}")
  }

  def markUnextractedPipeables(
    conf: InputConfig
  ): Pipe[MarkedInput, MarkedInput] = _.map {
    _ match {
      case Right(input) =>
        input match {
          case m @ Pipeable.CorpusFile(corpusEntry) =>
            val textGridFile =
              Pipeable.getTranscriptOutputFile(m, conf)
            val isProcessed = fs.exists(textGridFile.toFsPath())
            if (isProcessed) {
              log.info(s"Already processed ${corpusEntry}: ${textGridFile}")
              Left(input)
            } else {
              Right(input)
            }

          case _ @Pipeable.SingleFile(filePath @ _) =>
            ???
          case _ => ???
        }
      case m => m
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

  def config(filter: String): InputConfig.FilteredCorpus = {
    InputConfig.FilteredCorpus(
      corpusRoot = JPaths.get("corpus.d").toAbsolutePath().normalize(),
      pathFilters = InputConfig.PathFilters(
        Some(filter),
        numToRun = Int.MaxValue,
        numToSkip = 0,
        overwrite = true
      )
    )
  }

  def corpusEntryStream(implicit
    conf: InputConfig.FilteredCorpus
  ): Stream[PipeInput, CorpusEntry] = {
    for {
      maybeIn <- createMarkedInputStream(conf)
      pinput <- Stream
                  .fromZIO(ZIO.fromEither(maybeIn))
                  .map(_ match {
                    case Pipeable.CorpusFile(corpusEntry) => corpusEntry
                    case _                                => ???
                  })
    } yield pinput
  }

  def pageImageStream(
    corpusEntry: CorpusEntry
  ): Option[Seq[CorpusArtifact]] = {
    corpusEntry
      .getArtifactGroup("page-images")
      .map(_.getArtifacts())
  }

}
