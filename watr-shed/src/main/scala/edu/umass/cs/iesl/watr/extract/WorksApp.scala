package edu.umass.cs.iesl
package watr
package extract

import ammonite.ops._
import java.io.{ InputStream  }


object Works extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    entry: Option[JFile] = None,
    // file: Option[JFile] = None,
    singleFileMode: Boolean = false,
    corpusRoot: Option[JFile] = None,
    action: Option[String] = None,
    force: Boolean = false,
    exec: Option[(AppConfig) => Unit] = None
  )

  def corpusRootOrDie(ac: AppConfig): Path = ac.corpusRoot.map(
   croot => cwd/RelPath(croot)
  ).getOrElse(sys.error("no corpus root specified"))


  def setAction(conf: AppConfig, action: (AppConfig) => Unit): AppConfig = {
    conf.copy(exec=Option(action))
  }

  val parser = new scopt.OptionParser[AppConfig]("scopt") {
    head("Works command line app", "0.1")

    note("Run svg text extraction and analysis")


    opt[Unit]('x', "overwrite") action { (v, conf) =>
      conf.copy(force = true) } text("force overwrite of existing files")

    // opt[JFile]('a', "artifact") action { (v, conf) =>
    //   conf.copy(entry = Option(v))
    // } text("artifact id (same as *.d directory name)")

    // opt[JFile]('f', "file") action { (v, conf) =>
    //   conf.copy(
    //     file = Option(v),
    //     singleFileMode = true)
    // } text("run on a single file")


    opt[JFile]('c', "corpus") action { (v, conf) =>
      conf.copy(corpusRoot = Option(v))
    } text ("root path of the corpus")

    cmd("init") action { (_, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        normalizeCorpusEntry(ac)
      })
    } text ("init (or re-init) a corpus directory structure") // children()

    cmd("chars") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        extractCharacters(ac)
      })
    } text ("char extraction (debugging)")

    cmd("lseg") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        lineseg(ac)
      })
    } text ("run line segmentation (for debugging)")

    cmd("bbsvg") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        createBoundingBoxSvg(ac)
      })
    } text ("run column detection (for debugging)")

    cmd("sec") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        findSectionHeaders(ac)
      })
    } text ("section heading (debug)")

    cmd("docseg") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        detectParagraphs(ac)
      })
    } text ("run document segmentation")
  }


  parser.parse(args, AppConfig()).foreach{ config =>



  val croot = config
    .corpusRoot.map{ r => cwd/RelPath(r) }
    .filter { _.isDir }
    .getOrElse { sys.error(s"corpus root is not dir ('${config.corpusRoot}')") }


    config.exec.foreach { _.apply(config) }
  }

  def getProcessList(conf: AppConfig): Seq[CorpusEntry] = {
      val corpus = Corpus(corpusRootOrDie(conf))

      val toProcess = conf.entry match {
        case Some(entry) => Seq(corpus.entry(entry.getName))
        case None => corpus.entries()
      }
      toProcess
  }


  def runProcessor(conf: AppConfig, artifactOutputName: String, process: (CorpusEntry, String) => Unit): Unit = {
    getProcessList(conf).foreach { entry =>
      println(s"extracting ${entry.corpus}: ${entry} ${artifactOutputName}")
      if (entry.hasArtifact(artifactOutputName)) {
        if (conf.force){
          entry.deleteArtifact(artifactOutputName)
          process(entry, artifactOutputName)
        } else println(s"skipping existing ${entry}, use -x to force reprocessing")
      } else process(entry, artifactOutputName)
    }
  }

  def processCorpusArtifact(entry: CorpusEntry, outputName: String, processor: (InputStream, String) => String): Unit = {
    val pdfArtifact = entry.getPdfArtifact()
    val outputString = pdfArtifact.asInputStream
      .map({ pdf =>  try {
        processor(pdf, pdfArtifact.artifactPath.toString)
      } catch {
        case t: Throwable =>
          println(s"could not extract ${outputName}  for ${pdfArtifact}: ${t.getMessage}\n")

          println(t.toString())
          t.printStackTrace()
          println(t.getCause.toString())
          t.getCause.printStackTrace()
          s"""{ "error": "exception thrown ${t}: ${t.getCause}: ${t.getMessage}" }"""
      }})
      .map({ output =>
        entry.putArtifact(outputName, output)
      })
      .recover({ case t: Throwable =>
        val msg = (s"ERROR: could not extract ${outputName} for ${pdfArtifact}: ${t.getMessage}")
        println(msg)
        println(t.toString())
        t.printStackTrace()
        println(t.getCause.toString())
        t.getCause.printStackTrace()
      })
      .getOrElse({
        sys.error(s"ERROR: processing ${pdfArtifact} -> ${outputName}")
      })
  }

  def processCorpus(conf: AppConfig, artifactOutputName: String, processor: (InputStream, String) => String): Unit = {
    runProcessor(conf, artifactOutputName, process)

    def process(entry: CorpusEntry, outputName: String): Unit = {
      val pdfArtifact = entry.getPdfArtifact()
      processCorpusArtifact(entry, outputName, processor)
    }

  }


  def detectParagraphs(conf: AppConfig): Unit = {

    val artifactOutputName = "docseg.json"
    processCorpus(conf, artifactOutputName, proc)

    def proc(pdfins: InputStream, outputPath: String): String = {
      val segmenter = segment.DocumentSegmenter.createSegmenter(pdfins)
      segmenter.runPageSegmentation()
    }

  }

  def findSectionHeaders(conf: AppConfig): Unit = {

    processCorpus(conf, "bbox.svg", (pdfins: InputStream, outputPath: String) => {
      val dx = extract.DocumentExtractor
      val segmenter = segment.DocumentSegmenter.createSegmenter(pdfins)
      segmenter.runLineDetermination()
      segmenter.findMostFrequentLineDimensions()
      segmenter.buildExpandedTOC()
      val pageZones = for {
        pageId <- segmenter.pages.getPages
      } yield {
        println(s"segmenting page ${pageId}")
      }

      // dx.pages
      segmenter.runLineDetermination()
    //   pageSegAccum = findMostFrequentLineDimensions(pageSegAccum)

    //   val pageZones = for {
    //     pageId <- pages.getPages
    //   } yield {
    //     println(s"segmenting page ${pageId}")
    //     buildTableOfSections(pageId, pageSegAccum)
    //     determineZones(pageId, pageSegAccum)
    //   }
      ""
    })

  }


  def createBoundingBoxSvg(conf: AppConfig): Unit = {

    processCorpus(conf, "bbox.svg", (pdfins: InputStream, outputPath: String) => {
      extract.DocumentExtractor.extractBBoxesAsSvg(pdfins, Some(outputPath))
    })

  }

  def extractCharacters(conf: AppConfig): Unit = {
    import watrmarks.TB._
    import watrmarks.Bounds._
    import watrmarks._
    val artifactOutputName = "chars.txt"

    processCorpus(conf, artifactOutputName: String, proc)

    def proc(pdf: InputStream, outputPath: String): String = {
      val pageChars = DocumentExtractor
        .extractChars(pdf)
        .map({case(pageChars, pageGeom) =>
          val sortedYPage = pageChars.chars
            .groupBy(_.bbox.top.pp)
            .toSeq
            .sortBy(_._1.toDouble)

          val sortedXY = sortedYPage
            .map({case (topY, charBoxes) =>


              val sortedXLine = charBoxes
                .sortBy(_.bbox.left)
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

              val cbs = charBoxes.sortBy(_.bbox.left)
              val top = cbs.map(_.bbox.top).min
              val bottom = cbs.map(_.bbox.bottom).max
              val l=cbs.head.bbox.left
              val r=cbs.last.bbox.right

              val lineBounds = LTBounds(l, top, r-l, bottom-top).prettyPrint

              val lineChars = if(sortedXLine.exists(_._2!=" ")) {
                (">>".box % "?>") +| hcat(sortedXLine.map(x => x._1 % x._2))
              } else {
                ">>".box +| hcat(sortedXLine.map(x => x._1))
              }

              lineChars % lineBounds
            })
          vcat(sortedXY)
        })

      vsep(pageChars, 2, left).toString()
    }

  }


  def lineseg(conf: AppConfig): Unit = {

    val artifactOutputName = "lineseg.txt"
    processCorpus(conf, artifactOutputName, proc)


    def proc(pdfins: InputStream, outputPath: String): String = {
      val segmenter = segment.DocumentSegmenter.createSegmenter(pdfins)
      segmenter.runPageSegmentation()

      // val lineCols = lines
      //   .sortBy(_.findCenterY())
      //   .map({ l =>
      //     val lineBounds = l.bounds.prettyPrint
      //     val tokenized = l.tokenizeLine().toText
      //     // s"${tokenized}               ${lineBounds}"
      //     (tokenized.box, lineBounds.box)
      //   })
      // val justified =
      //   s"\nPage:${page} file://${outputPath}" %|
      //     (vcat(left)(lineCols.map(_._1).toList) + "    " + vcat(right)(lineCols.map(_._2).toList))
      // justified.toString()
      // }.mkString(s"\nDocument: file://${outputPath}\n", "\n", "\n")

      ""

    }
  }


  def normalizeCorpusEntry(conf: AppConfig): Unit = {
    val corpus = Corpus(corpusRootOrDie(conf))

    println(s"normalizing corpus at ${corpus.corpusRoot}")
    ls! corpus.corpusRoot foreach { pdf =>
      val artifactPath = corpus.corpusRoot / s"${pdf.name}.d"
      if (pdf.isFile && !(exists! artifactPath)) {
        println(s" creating artifact page for  ${pdf}")
        mkdir! artifactPath
      }
      if (pdf.isFile) {
        println(s" stashing ${pdf}")
        mv.into(pdf, artifactPath)
      }
    }
  }


}
