package edu.umass.cs.iesl.watr

package extract

import ammonite.ops._
import java.io.{ InputStream  }
import textboxing.{TextBoxing => TB}
import spindex._
import IndexShapeOperations._


object Works extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    runRoot: Option[JFile] = None,
    corpusRoot: Option[JFile] = None,
    inputFileList: Option[JFile] = None,
    action: Option[String] = None,
    dbPath: Option[JFile] = None,
    force: Boolean = false,
    numToRun: Int = 0,
    numToSkip: Int = 0,
    exec: Option[(AppConfig) => Unit] = None
  )

  def corpusRootOrDie(ac: AppConfig): Path = ac
    .corpusRoot
    .map({croot =>
      val fullPath = cwd/RelPath(croot)
      val corpusSentinel =  fullPath / ".corpus-root"
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

    help("help")
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

    opt[JFile]('d', "db") action { (v, conf) =>
      conf.copy(dbPath = Option(v))
    } text("h2 database path")


    cmd("init") action { (_, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        initCorpus(ac)
      })
    } text ("init (or re-init) a corpus directory structure") // children()

    cmd("docseg") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        segmentDocument(ac)
      })
    } text ("run document segmentation")

    cmd("richtext") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        runCmdRichText(ac)
      })
    } text ("output pdf as text, with some added embedded tex and labels")


    cmd("chars") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        extractCharacters(ac)
      })
    } text ("(dev) char extraction")

    cmd("fonts") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        examineFonts(ac)
      })
    } text ("")

    cmd("showfonts") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        showFontDB(ac)
      })
    } text ("")

    cmd("images") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        extractImages(ac)
      })
    } text ("extract pdf pages as images")

  }


  parser.parse(args, AppConfig()).foreach{ config =>
    config.exec.foreach { _.apply(config) }
  }

  val autoInit = true

  def getProcessList(conf: AppConfig): Seq[CorpusEntry] = {
    val corpus = Corpus(corpusRootOrDie(conf))

    println(s"starting Works in corpus ${corpus}")


    val toProcess = conf.inputFileList
      .map({ inputs =>
        // User specifed input file on command line:
        val inputLines = if (inputs.toString == "--") {
          io.Source.stdin.getLines.toList
        } else {
          // corpus.
          val inputFiles =  RelPath(inputs)
          val lines = read(cwd/inputFiles)
          lines.split("\n").toList
        }


        inputLines
          .map(_.trim).filterNot(_.isEmpty())
          .filter({ inputLine =>
            if (!corpus.hasEntry(inputLine)) {
              println(s"warning: no corpus entry with id ${inputLine}, skipping")
              false
            } else true
          })
          .map(corpus.entry(_).get)

      }).getOrElse({
        corpus.entries()
      })



    val skipped = if (conf.numToSkip > 0) toProcess.drop(conf.numToSkip) else toProcess
    val taken = if (conf.numToRun > 0) skipped.take(conf.numToRun) else skipped

    taken

  }


  def runProcessor(conf: AppConfig, artifactOutputName: String, process: (CorpusEntry, String) => Unit): Unit = {
    var i = 0
    getProcessList(conf).foreach { entry =>
      println(s"${i}. extracting ${entry} ${artifactOutputName}")
      if (entry.hasArtifact(artifactOutputName)) {
        if (conf.force){
          entry.deleteArtifact(artifactOutputName)
          process(entry, artifactOutputName)
        } else println(s"skipping existing ${entry}, use -x to force reprocessing")
      } else process(entry, artifactOutputName)
      i += 1
    }
  }

  def processCorpusArtifact(entry: CorpusEntry, outputName: String, processor: (InputStream, String) => String): Unit = {
    (for {
      pdfArtifact <- entry.getPdfArtifact
      pdfIns <- pdfArtifact.asInputStream.toOption
    } yield {
      try {
        val output = processor(pdfIns, pdfArtifact.artifactPath.toString)
        entry.putArtifact(outputName, output)
      } catch {
        case t: Throwable =>
          println(s"could not extract ${outputName}  for ${pdfArtifact}: ${t.getMessage}\n")

          println(t.toString())
          t.printStackTrace()
          println(t.getCause.toString())
          t.getCause.printStackTrace()
          s"""{ "error": "exception thrown ${t}: ${t.getCause}: ${t.getMessage}" }"""
      } finally {
        pdfIns.close()
      }})
  }

  def processCorpusEntryList(conf: AppConfig, processor: (CorpusEntry) => Unit): Unit = {
    getProcessList(conf).foreach { entry =>
      println(s"processing ${entry}")
      processor(entry)
    }
  }

  def processCorpus(conf: AppConfig, artifactOutputName: String, processor: (InputStream, String) => String): Unit = {
    runProcessor(conf, artifactOutputName, process)

    def process(entry: CorpusEntry, outputName: String): Unit = {
      val pdfArtifact = entry.getPdfArtifact()
      processCorpusArtifact(entry, outputName, processor)
    }
  }

  def normalizeCorpusEntry(corpus: Corpus, pdf: Path): Unit = {
    val artifactPath = corpus.corpusRoot / s"${pdf.name}.d"
    if (pdf.isFile && !(exists! artifactPath)) {
      println(s" creating artifact dir ${pdf}")
      mkdir! artifactPath
    }
    if (pdf.isFile) {
      val dest = artifactPath / pdf.name

      if (exists(dest)) {
        println(s"corpus already contains file ${dest}, skipping...")
      } else {
        println(s" stashing ${pdf}")
        mv.into(pdf, artifactPath)
      }
    }
  }

  def initCorpus(conf: AppConfig): Unit = {
      conf.corpusRoot
      .map({croot =>
        val fullPath = cwd/RelPath(croot)
        val validPath = exists(fullPath)

        if (!validPath) {
          sys.error(s"init: invalid corpus root specified ${fullPath}")
        } else {
          Corpus(fullPath).touchSentinel()
        }
      }).getOrElse(sys.error("no corpus root specified"))


    normalizeCorpusEntries(conf)
  }

  def normalizeCorpusEntries(conf: AppConfig): Unit = {
    val corpus = Corpus(corpusRootOrDie(conf))

    println(s"normalizing corpus at ${corpus.corpusRoot}")

    ls(corpus.corpusRoot)
    .filter(p=> p.isFile && (p.ext=="pdf" || p.ext=="ps"))
    .foreach { pdf =>
      normalizeCorpusEntry(corpus, pdf)
    }
  }


  def runCmdRichText(conf: AppConfig): Unit = {

    val artifactOutputName = "richtext.txt"
    processCorpus(conf, artifactOutputName, proc)

    def proc(pdfins: InputStream, outputPath: String): String = {
      val segmenter = segment.DocumentSegmenter.createSegmenter(pdfins)
      segmenter.runPageSegmentation()
      val output = format.RichTextIO.serializeDocumentAsText(segmenter.zoneIndexer, None)
      output
    }

  }

  def segmentDocument(conf: AppConfig): Unit = {

    val artifactOutputName = "docseg.json"
    processCorpus(conf, artifactOutputName, proc)

    def proc(pdfins: InputStream, outputPath: String): String = {
      try {

        val segmenter = segment.DocumentSegmenter.createSegmenter(pdfins)
        segmenter.runPageSegmentation()
        val output = format.DocumentIO.serializeDocument(segmenter.zoneIndexer).toString()
        output
      } catch {
        case t: Throwable =>
          println(s"could not extract ${outputPath}: ${t.getMessage}\n")
          println(t.toString())
          t.printStackTrace()
          s"""{ "error": "exception thrown ${t}: ${t.getCause}: ${t.getMessage}" }"""
      }
    }

  }

  def extractImages(conf: AppConfig): Unit = {
    import ammonite.{ops => fs}
    import fs._
    import fs.ImplicitWd._


    processCorpusEntryList(conf, {corpusEntry =>

      for {
        pdf <- corpusEntry.getPdfArtifact
        pdfPath <- pdf.asPath
      } {
        val pageImagePath = corpusEntry.artifactsRoot / "page-images"
        if (! exists(pageImagePath)) {
          mkdir(pageImagePath)
        }
        val pageImageFilespec = pageImagePath / "page-%d.png"

        val res = %%("mudraw", "-r", "128", "-o", pageImageFilespec, pdfPath)

      }

    })
  }

  def examineFonts(conf: AppConfig): Unit = {
    import ammonite.{ops => fs}
    import extract.fonts._

    val dbfile = conf.dbPath.getOrElse {
      sys.error("please specify database path")
    }

    val dbpath = cwd / RelPath(dbfile)

    val db = new FontDatabaseApi(dbpath)

    try {
      db.createDBDir()

      processCorpusEntryList(conf, {corpusEntry =>

        if (corpusEntry.hasArtifact("fonts")) {
          for {
            fontDir <- corpusEntry.getArtifact("fonts")
            pdir <- fontDir.asPath
            sfdirs = fs.ls(pdir).filter(_.ext=="sfdir")
            sfdir <- sfdirs
          } {
            println(s"adding fonts from ${sfdir}")
            val sfs = SplineFonts.loadSfdir(sfdir)

            db.addFontDir(sfs)
          }
        }
      })

    } finally {
      db.shutdown()

    }


  }


  def showFontDB(conf: AppConfig): Unit = {
    import extract.fonts._

    val dbfile = conf.dbPath.getOrElse {
      sys.error("please specify database path")
    }

    val dbpath = cwd / RelPath(dbfile)

    val db = new FontDatabaseApi(dbpath)

    try {
      // db.showFontTrees()
      db.showHashedGlyphs()

    } finally {
      db.shutdown()
    }


  }

  def extractCharacters(conf: AppConfig): Unit = {
    import TB._
    import GeometricFigure._
    // import watrmarks._
    val artifactOutputName = "chars.txt"

    processCorpus(conf, artifactOutputName: String, proc)

    def proc(pdf: InputStream, outputPath: String): String = {
      val pageChars = format.DocumentIO
        .extractChars(pdf)
        .map({case(pageRegions, pageGeom) =>
          val sortedYPage = pageRegions.regions
            .collect({case c: CharAtom => c})
            .groupBy(_.region.bbox.top.pp)
            .toSeq
            .sortBy(_._1.toDouble)

          val sortedXY = sortedYPage
            .map({case (topY, charBoxes) =>


              val sortedXLine = charBoxes
                .sortBy(_.region.bbox.left)
                .map({ charBox =>
                  charBox.wonkyCharCode
                    .map({ code =>
                      if (code==32) { (s"${code}".box , "_") }
                      else { (s"${code}".box, "?") }
                    })
                    .getOrElse({
                      if (charBox.subs.isEmpty()) {
                        (charBox.char.box, " ")
                      } else {
                        (charBox.subs.box, charBox.char)
                      }
                    })
                })

              val cbs = charBoxes.sortBy(_.region.bbox.left)
              val top = cbs.map(_.region.bbox.top).min
              val bottom = cbs.map(_.region.bbox.bottom).max
              val l=cbs.head.region.bbox.left
              val r=cbs.last.region.bbox.right


              val cbspp = charBoxes.mkString(", ")

              val lineBounds = LTBounds(l, top, r-l, bottom-top).prettyPrint

              val lineChars = if(sortedXLine.exists(_._2!=" ")) {
                cbspp.box %
                  ((">>".box % "?>") +| hcat(sortedXLine.map(x => x._1 % x._2)))
              } else {
                cbspp.box %
                  (">>".box +| hcat(sortedXLine.map(x => x._1)))
              }

              lineChars % lineBounds
            })
          vcat(sortedXY)
        })

      vsep(pageChars, 2, left).toString()
    }

  }





}
