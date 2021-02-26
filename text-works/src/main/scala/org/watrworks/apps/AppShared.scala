package org.watrworks
package apps

import segment._
import java.nio.{file => nio}
import corpora.filesys._
import cats.effect._
import utils.DoOrDieHandlers._
import ammonite.{ops => fs} // , fs._
import utils.PathUtils._
import TypeTags._
import utils.Timer.time
import _root_.io.circe, circe._, circe.syntax._
import transcripts.Transcript
import geometry._
import geometry.syntax._
import utils.ExactFloats._

object ProcessPipelineSteps {
  private[this] val log = org.log4s.getLogger

  val PrettyPrint2Spaces = circe.Printer.spaces2

  type MarkedInput = Either[ProcessableInput, ProcessableInput]
  type MarkedOutput = Either[String, ProcessedInput]

  def visualizeShapeClusters(transcript: Transcript): Unit = {
    val labelMap = transcript.labels
      .filter(_.id.isDefined)
      .map(label => (s"${label.name}:${label.id.get}", label))
      .toMap

    val formattedCommandInput = for {
      label <- transcript.labels
      if label.name == "TrapezoidBins"
      binChildren <- label.children.toList
      // Choose which cluster to show...
      binLabel <- binChildren
      .sortBy(bc => bc.children.map(_.length).getOrElse(0)) .reverse.drop(6).headOption.toList
      maybeInstances <- binLabel.children.toList
      _ = println(s"Running on ${maybeInstances.length} instances")
      instanceLabel <- maybeInstances
    } yield {

      val id = instanceLabel.id.get
      val key = s"Trapezoid:${id}"
      val trapShape = labelMap(key)

      val sdf = trapShape.range match {
        case Transcript.DocumentRange(_, doc) ::
            Transcript.PageRange(_, page) ::
            Transcript.GeometryRange(_, shape) ::
            Nil =>
          val pageN = transcript.pages(page.unwrap)
          val pageGeometry = pageN.bounds
          val pageImage = s"../corpus.d/${doc.unwrap}/page-images/page-${page.unwrap+1}.opt.png"
          val geom = pageGeometry.toPoint(Dir.BottomRight)
          shape match {
            case trapezoid: Trapezoid =>
              val tl = trapezoid.topLeft
              val tr = tl.translate(trapezoid.topWidth, 0.toFloatExact())
              val bl = trapezoid.bottomLeft
              val th = trapezoid.height()

              val br = bl.translate(trapezoid.bottomWidth, 0.toFloatExact())
              val fmt = (p: Point) => s"${p.x.pp()},${p.y.pp()}"
              val fmtx = (p: Point) => s"${p.x.pp()}x${p.y.pp()}"
              val path = s"${fmt(tl)} ${fmt(tr)} ${fmt(br)} ${fmt(bl)}"
              val crop = s"${geom.x.pp()}x${(th+16.toFloatExact()).pp()}+0+${(tl.y-8.toFloatExact()).pp()}"
              val args = List(
                """(""",
                pageImage,
                "-resize", fmtx(geom),
                "-fill", "blue",
                "-draw", s"""fill-opacity 0.3 path 'M ${path} Z'""",
                "-crop", crop,
                "-resize" , "200%",
                """)""",
              )

              args
            case _ => List()
          }
        case _ => List()
      }

      sdf
    }

    import scala.sys.process._
    // println(formattedCommandInput.flatten.mkString("\n"))
      // convert "${args[@]}" miff:- | montage - -bordercolor blue -border 1 -tile 3x -geometry +2+2 show:
    val combinedArgs = formattedCommandInput.flatten
      // convert "${args[@]}" miff:- | montage - -bordercolor blue -border 1 -tile 3x -geometry +2+2 show:

    // val cmdList = ("convert" :: combinedArgs) ++ List("x:")
    val cmdList = ("convert" :: combinedArgs) ++ List("miff:-")
    val montage = List("montage", "-", "-bordercolor", "blue", "-border", "1", "-tile", "3x", "-geometry", "+2+2", "x:")

    println(s"running...${cmdList} => ${montage}")
    val _ = (cmdList #| montage).!
  }

  def runTraceVis(conf: TraceVisConfig.Config): Unit = {
    println("TraceVis...")

    val processStream = createInputStream[IO](conf.ioConfig)
      .through(initMarkedInput())
      .through(dropSkipAndRun(conf.ioConfig))

    val prog = processStream.compile.toList

    val inputs = prog.unsafeRunSync()
    println(s"inputs: ${inputs}")
    val _ = inputs.map {
      case Right(p @ Processable.CorpusFile(corpusEntry)) =>
        val transcriptArtifact = corpusEntry.resolveArtifact("transcript.json", None)
        transcriptArtifact.asJson.toEither match {
          case Left(x) =>
            println(s"Error: ${x}")

          case Right(json) =>
            val mtrans = json.as[Transcript]
            mtrans match {
              case Left(err)         =>
                println(s"Error: ${err}")
              case Right(transcript) =>
                println(s"Success: ${transcript.documentId}")
                visualizeShapeClusters(transcript)
            }

        }

        val transcriptFile = Processable.getTextgridOutputFile(p, conf.ioConfig)
        println(s"entry: ${transcriptFile}")
      case x                                              =>
        println(s"Left entry: ${x}")
    }
  }

  def runTextExtractionOnFile(conf: TextWorksConfig.Config): Unit = {
    if (conf.runTraceLogging) {
      log.info(s"Visual tracing is enabled; performance will be lowered")
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

  def createInputStream[F[_]: Effect](
    conf: IOConfig
  ): fs2.Stream[F, ProcessableInput] = {
    conf.inputMode
      .map { mode =>
        mode match {
          case in @ Processable.SingleFile(_) =>
            fs2.Stream.emit(in).covary[F]

          case Processable.CorpusRoot(corpusRoot) =>
            Corpus(corpusRoot)
              .entryStream[F]()
              .filter { entry =>
                conf.pathFilter
                  .map { filter => entry.entryDescriptor.matches(filter) }
                  .getOrElse { true }
              }
              .map { entry => Processable.CorpusFile(entry) }

          case Processable.ListOfFiles(_) =>
            die("TODO")

          case Processable.CorpusFile(_) =>
            die("TODO")
        }
      }
      .orDie("inputStream(): Invalid input options")
  }

  def dropSkipAndRun(conf: IOConfig): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    var skipCount = conf.numToSkip
    var runCount = conf.numToRun
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

  def initMarkedInput(): fs2.Pipe[IO, ProcessableInput, MarkedInput] =
    _.map { Right(_) }

  def filterInputMatchRegex(
    regex: Option[String]
  ): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    _.map {
      case r @ Right(p @ Processable.CorpusFile(corpusEntry)) =>
        // println(s"filterInputMatchRegex? ${regex} == ${corpusEntry.entryDescriptor}")
        val filterMatches = regex.map { re =>
          corpusEntry.entryDescriptor.matches(re)
        } getOrElse (true)

        if (filterMatches) {
          r
        } else {
          Left(p)
        }
      case x                                                  => x
    }
  }

  def pickupTranscriptFiles(
    conf: TextWorksConfig.Config
  ): fs2.Pipe[IO, MarkedInput, MarkedOutput] = {
    _.map {
      case Left(inputMode @ _) => Left("no transcript.json")
      case Right(inputMode)    =>
        val textgridFile =
          Processable.getTextgridOutputFile(inputMode, conf.ioConfig)

        if (textgridFile.toFile().exists()) {
          Right(Processable.ExtractedTextGridFile(textgridFile, inputMode))
        } else {
          Left("no transcript.json")
        }
    }
  }

  def cleanFileArtifacts(
    conf: TextWorksConfig.Config
  ): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
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
          val pdfName = f.getFileName.toString()
          val stableId = DocumentID(pdfName)
          val input = nioToAmm(f)

          val output = conf.ioConfig.outputPath.getOrElse {
            nio.Paths.get(f.getFileName().toString() + ".transcript.json")
          }

          if (!output.toFile().exists()) {
            Right(Processable.ExtractedFile(extractText(stableId, input), m))
          } else {
            val msg =
              s"File ${output} already exists. Move file or use --overwrite"
            println(msg)
            Left(msg)
          }

        case m @ Processable.CorpusFile(corpusEntry) =>
          val stableId = DocumentID(corpusEntry.entryDescriptor)

          println(s"Processing ${stableId}")

          val maybeSegmenter = for {
            pdfEntry <- corpusEntry.getPdfArtifact().toRight(left = "Could not get PDF")
            pdfPath <- pdfEntry.asPath.toEither.left.map(_.toString())
          } yield {

            // val output = Processable.getTextgridOutputFile(m, conf.ioConfig)
            // val ammPath = nioToAmm(output)

            time("extractText") {
              extractText(stableId, pdfPath)
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
    val now = System.currentTimeMillis()
    val dtf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    dtf.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    val nowStr = dtf.format(new java.util.Date(now))

    buildInfoMap.asObject.get
      .add("runAtTime", nowStr.asJson)
      .remove("builtAtMillis")
      .asJson
  }

  def writeExtractedTextFile(
    conf: TextWorksConfig.Config
  ): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
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

  def writeTraceLogs(
    conf: TextWorksConfig.Config
  ): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = _.map {
    case m @ Right(Processable.ExtractedFile(segmentation, input)) =>
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
          val pageLogs = segmentation.pageSegmenters.foldLeft(List[Json]()) {
            case (accum, pageSegmenter) =>
              accum ++ pageSegmenter.emitLogs()
          }

          fs.write(rootPath / "tracelog.json", pageLogs.asJson.noSpaces)
        }
      }

      m

    case x => x
  }

  def cleanTraceLogArtifacts(
    conf: TextWorksConfig.Config
  ): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
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
  ): fs2.Pipe[IO, MarkedInput, MarkedOutput] =
    inStream => {
      inStream.map {
        case Right(input) => doSegment1(input, conf)
        case Left(input)  => Left(s"Skipping ${input}")
      }
    }

  def markUnextractedProcessables(
    conf: TextWorksConfig.Config
  ): fs2.Pipe[IO, MarkedInput, MarkedInput] = _.map { markedInput =>
    markedInput match {
      case Right(input) =>
        input match {
          case m @ Processable.CorpusFile(corpusEntry) =>
            val textGridFile =
              Processable.getTextgridOutputFile(m, conf.ioConfig)
            val isProcessed = fs.exists(textGridFile.toFsPath())
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
    stableId: String @@ DocumentID,
    inputPdf: fs.Path
  ): DocumentSegmentation = {

    println(s"Extracting ${stableId}")

    val segmenter = time("createSegmenter") {
      DocumentSegmenter.createSegmenter(stableId, inputPdf)
    }

    time("runDocumentSegmentation") {
      segmenter.runDocumentSegmentation()
    }

    segmenter
  }

}
