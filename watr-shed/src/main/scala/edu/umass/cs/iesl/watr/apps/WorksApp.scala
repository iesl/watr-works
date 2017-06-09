package edu.umass.cs.iesl.watr
package apps

import corpora._
import corpora.filesys._
import predsynth._

import ammonite.{ops => fs}, fs._
import java.io.{File => JFile}
import predsynth._
import segment.DocumentSegmenter
import TypeTags._

case class AppConfig(
  runRoot: Option[JFile] = None,
  corpusRoot: Option[JFile] = None,
  inputFile: Option[JFile] = None,
  inputFileList: Option[JFile] = None,
  inputEntryDescriptor: Option[String] = None,
  action: Option[String] = None,
  dbPath: Option[JFile] = None,
  preserveAnnotations: Boolean = false,
  priorPredsynthFile: Option[JFile] = None,
  priorDocsegFile: Option[JFile] = None,
  textAlignPredsynth: Boolean = false,
  force: Boolean = false,
  numToRun: Int = 0,
  numToSkip: Int = 0,
  exec: Option[(AppConfig) => Unit] = None
)


object Works extends App {
  private[this] val log = org.log4s.getLogger

  // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Off
  // tracing.VisualTracer
  // tracing.VisualTracer.clearFilters()
  // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Print
  // tracing.VisualTracer.addFilter("GutterDetection")
  // tracing.VisualTracer.addFilter("LabelAbstract")

  def die(t: Throwable): Unit = {
    val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
    log.info(s"ERROR: ${message}")
    t.printStackTrace()
  }

  def corpusRootOrDie(ac: AppConfig): Path = ac.corpusRoot
    .map({croot =>

      val fullPath = if (croot.isAbsolute()) Path(croot) else pwd/RelPath(croot)

      // val corpusSentinel =  fullPath / ".corpus-root"
      val validPath = exists(fullPath)
      val validSentinel = exists(fullPath/".corpus-root")

      if (!validPath) {
        sys.error(s"invalid corpus root specified ${fullPath}")
      } else if (!validSentinel) {
        sys.error(s"no .corpus-root sentinal file found in ${fullPath};\n run bin/works init (or create if manually you know what you're doing)")
      } else {
        fullPath
      }

    }).getOrElse(sys.error("no corpus root specified"))


  def setAction(conf: AppConfig, action: (AppConfig) => Unit): AppConfig = {
    conf.copy(exec=Option(action))
  }

  val parser = new scopt.OptionParser[AppConfig]("works") {
    head("Watr Works command line app", "0.1")

    note("Run text extraction and analysis on PDFs")

    help("usage")

    opt[JFile]('c', "corpus") action { (v, conf) =>
      conf.copy(corpusRoot = Option(v))
    } text ("root path of the corpus")

    opt[Int]('n', "number") action { (v, conf) =>
      conf.copy(numToRun = v) } text("process n corpus entries")

    opt[Int]('k', "skip") action { (v, conf) =>
      conf.copy(numToSkip = v) } text("skip first k entries")

    opt[Unit]('x', "overwrite") action { (v, conf) =>
      conf.copy(force = true) } text("force overwrite of existing files")


    opt[JFile]('i', "inputs") action { (v, conf) =>
      conf.copy(inputFileList = Option(v))
    } text("process files listed in specified file. Specify '--' to read from stdin")

    opt[JFile]("file") action { (v, conf) =>
      conf.copy(inputFile = Option(v))
    } text("choose single input file")

    opt[JFile]('d', "db") action { (v, conf) =>
      conf.copy(dbPath = Option(v))
    } text("h2 database path")

    opt[Unit]("text-align") action { (v, conf) =>
      conf.copy(textAlignPredsynth = true)
    } text("use text heuristics to align predsynth db to watrworks output")

    opt[JFile]("merge-predsynth") action { (v, conf) =>
      conf.copy(priorPredsynthFile = Option(v))
    } text("location of predsynth db export (as json)")

    opt[JFile]("merge-docseg") action { (v, conf) =>
      conf.copy(priorDocsegFile = Option(v))
    } text("location of prior docseg: annotations will carry forward into current text extraction")

    opt[Unit]("preserve-annotations") action { (v, conf) =>
      conf.copy(preserveAnnotations = true)
    } text("location of prior docseg: annotations will carry forward into current text extraction")

    cmd("init") action { (_, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        initCorpus(ac)
      })
    } text ("init (or re-init) a corpus directory structure") // children()

    cmd("docseg")
      .action((v, conf) => setAction(conf, segmentDocument(_)))
      .text ("run document segmentation")

    cmd("totext")
      .action((v, conf) => setAction(conf, extractText(_)))
      .text ("run basic text extraction")

    cmd("fast-forward-docseg")
      .action((v, conf) => setAction(conf, fastForwardDocsegs(_)))
      .text ("re-run document segmentation while preserving existing annotations")

  }


  parser.parse(args, AppConfig()).foreach{ config =>
    config.exec.foreach { _.apply(config) }
  }

  def getProcessList(conf: AppConfig): Seq[CorpusEntry] = {
    val corpus = Corpus(corpusRootOrDie(conf))

    log.info(s"running Works in corpus ${corpus}")

    val toProcess = conf.inputFileList
      .map({ inputs =>
        // User specifed input file on command line:
        val inputLines = if (inputs.toString == "--") {
          _root_.scala.io.Source.stdin.getLines.toList
        } else {
          val inputFiles =  RelPath(inputs)
          val lines = read(pwd/inputFiles)
          lines.split("\n").toList
        }


        inputLines
          .map(_.trim).filterNot(_.isEmpty())
          .filter({ inputLine =>
            if (!corpus.hasEntry(inputLine)) {
              log.info(s"warning: no corpus entry with id ${inputLine}, skipping")
              false
            } else true
          })
          .map(corpus.entry(_).get)

      }).getOrElse({
        conf.inputEntryDescriptor
          .map(e => Seq(corpus.entry(e).getOrElse{sys.error(s"unknown corpus entry${e}")}))
          .getOrElse(corpus.entries())
      })

    val skipped = if (conf.numToSkip > 0) toProcess.drop(conf.numToSkip) else toProcess
    val taken = if (conf.numToRun > 0) skipped.take(conf.numToRun) else skipped

    log.info(s"processing entries ${conf.numToRun}-${taken.length}")
    taken
  }


  // Decide if the specified output artifact exists, or if the --force option is specified
  def processOrSkipOrForce(conf: AppConfig, entry: CorpusEntry, artifactOutputName: String): Option[String] = {
    if (entry.hasArtifact(artifactOutputName)) {
      if (conf.force) {
        entry.deleteArtifact(artifactOutputName)
        Some(artifactOutputName)
      } else None
    } else Some(artifactOutputName)
  }

  def skipOrStashArtifact(conf: AppConfig, entry: CorpusEntry, artifactOutputName: String): Option[Path] = {
    if (entry.hasArtifact(artifactOutputName)) {
      entry.stashArtifact(artifactOutputName)
    } else None
  }

  def processCorpusEntryList(conf: AppConfig, processor: (CorpusEntry) => Unit): Unit = {
    var i = 0
    getProcessList(conf).foreach { entry =>
      log.info(s"${i}. processing ${entry} ")
      processor(entry)
      i += 1
    }
  }


  def initCorpus(conf: AppConfig): Unit = {
    val croot = conf.corpusRoot.getOrElse(sys.error("no corpus root specified"))
    val fullPath = pwd/RelPath(croot)
    Corpus.initCorpus(fullPath.toIO.getPath)
  }


  def loadPredsynthUberJson(conf: AppConfig): Option[Map[String, Paper]] = {
    for {
      pfile <- conf.priorPredsynthFile
      dict <- PredsynthLoad.loadPapers(pwd / RelPath(pfile))
    } yield dict
  }


  def runPageSegmentation(stableId: String@@DocumentID, pdfPath: Path): DocumentSegmenter =  {
    val segmenter = DocumentSegmenter
      .createSegmenter(stableId, pdfPath, new MemDocZoningApi)

    segmenter.runPageSegmentation()
    segmenter
  }

  def writePredsynthJson(predsynthPaper: Paper, corpusEntry: CorpusEntry): Unit = {
    new predsynth.PredsynthJsonFormats {
      import play.api.libs.json, json._
      val pjson = Json.toJson(predsynthPaper)
      val jsOut = Json.prettyPrint(pjson)
      corpusEntry.putArtifact("predsynth.json", jsOut)
    }
  }

  // Load pre-existing docseg
  // If it contains annotation:
  //   stash it via datestamped filename + dir
  //   do text extraction
  //   align old annots w/new docseg
  //   write new docseg w/annots
  // else
  //   do nothing
  def fastForwardDocsegs(conf: AppConfig): Unit = {
    val docsegFile = "docseg.json"
    log.info(s"fast-forwarding docsegs")
    for {
      corpusEntry      <- getProcessList(conf)
      priorDocsegArt   <- corpusEntry.getArtifact(docsegFile).orElse { log.info("No prior docseg") ; None }
      priorPath        <- priorDocsegArt.asPath
      priorDocseg      <- Docseg.read(priorPath)
      // stashedDocseg  <- skipOrStashArtifact(conf, corpusEntry, "docseg.json")
      hasPriorAnnots  = !priorDocseg.mentions.isEmpty
      _               = log.info(s"prior annotations=${hasPriorAnnots}")
      _               = if (hasPriorAnnots) { corpusEntry.stashArtifact("docseg.json") }

      if hasPriorAnnots

      stashed          <- priorDocsegArt.stash
      _                 = log.info(s"prior docseg stashed at ${stashed}")

      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath
    } {

      val stableId = DocumentID(corpusEntry.entryDescriptor)

      val segmenter = runPageSegmentation(stableId, pdfPath)
      val mergedZoneIndex = DocsegMerging.mergePriorDocseg(segmenter.mpageIndex, priorDocseg)

      val output = formats.DocumentIO.richTextSerializeDocument(mergedZoneIndex, Seq())
      corpusEntry.putArtifact(docsegFile, output)
    }

  }

  def segmentDocument(conf: AppConfig): Option[segment.DocumentSegmenter] = {
    val artifactOutputName = "docseg.json"

    var rsegmenter: Option[segment.DocumentSegmenter] = None

    val predsynthPapers: Map[String, Paper] =
      loadPredsynthUberJson(conf)
        .getOrElse(Map())

    processCorpusEntryList(conf, {corpusEntry =>
      try {
        for {
          output          <- processOrSkipOrForce(conf, corpusEntry, artifactOutputName)
          predsynthOutput <- processOrSkipOrForce(conf, corpusEntry, "predsynth.json")
          pdfArtifact     <- corpusEntry.getPdfArtifact
          pdfPath         <- pdfArtifact.asPath
        } {

          val stableId = DocumentID(corpusEntry.entryDescriptor)

          val segmenter = runPageSegmentation(stableId, pdfPath)

          rsegmenter = Some(segmenter)

          val entryFilename = corpusEntry.entryDescriptorRoot

          val paper = predsynthPapers.get(entryFilename)

          val output = paper.map({ p =>
            val alignedGroups =
              MITAlignPredsynth.alignPredSynthPaper(segmenter.mpageIndex, p)
            writePredsynthJson(p, corpusEntry)
            formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex, alignedGroups)
          }).getOrElse(
            formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex, Seq())
          )

          corpusEntry.putArtifact(artifactOutputName, output)
        }
      } catch {
        case t: Throwable => die(t)
      }
    })

    rsegmenter

  }

  def extractText(conf: AppConfig): Unit = {
    println("extracting text")
    for {
      pdfFile <- conf.inputFile
    } {
      val pdfPath = pwd / RelPath(pdfFile)
      println(s"file: $pdfPath")

      val stableId = DocumentID(pdfPath.toString())
      val segmenter = runPageSegmentation(stableId, pdfPath)
      formats.DocumentIO
        .documentToPlaintext(segmenter.mpageIndex)
        .foreach{ line =>
          println(line)
        }
    }
  }

}
