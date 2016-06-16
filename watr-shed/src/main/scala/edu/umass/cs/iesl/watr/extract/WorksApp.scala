package edu.umass.cs.iesl.watr

package extract

import ammonite.ops._
import java.io.{ InputStream  }
import textboxing.{TextBoxing => TB}
import spindex._
import IndexShapeOperations._
// import ComponentTypeEnrichments._
  // import ComponentOperations._
  // import ComponentRendering._



object Works extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    // entry: Option[JFile] = None,
    corpusRoot: Option[JFile] = None,
    inputFileList: Option[JFile] = None,
    action: Option[String] = None,
    force: Boolean = false,
    numToRun: Int = 0,
    numToSkip: Int = 0,
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

    opt[Int]('n', "number") action { (v, conf) =>
      conf.copy(numToRun = v) } text("process n corpus entries")

    opt[Int]('k', "skip") action { (v, conf) =>
      conf.copy(numToSkip = v) } text("skip first k entries")

    opt[Unit]('x', "overwrite") action { (v, conf) =>
      conf.copy(force = true) } text("force overwrite of existing files")

    opt[JFile]('i', "input") action { (v, conf) =>
      conf.copy(inputFileList = Option(v))
    } text("process files listed in specified file, use -input=-- to read inputs from stdin")

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


    cmd("bbsvg") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        createBoundingBoxSvg(ac)
      })
    } text ("run column detection (for debugging)")


    cmd("docseg") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        segmentDocument(ac)
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
    val corpus = Corpus(conf.corpusRoot
        .map(croot => cwd/RelPath(croot))
        .getOrElse(cwd))



    val toProcess = conf.inputFileList
      .map({ inputs =>

        val inputLines = if (inputs.toString == "--") {
          io.Source.stdin.getLines.toList
        } else {
          val inputFiles =  RelPath(inputs)
          val lines = read(cwd/inputFiles)
          lines.split("\n").toList
        }

        val inputFiles =
          inputLines
            .map(_.trim).filterNot(_.isEmpty())
            .map(corpus.entry(_))
        inputFiles

      }).getOrElse({
        corpus.entries()
      })



    val skipped = if (conf.numToSkip > 0) toProcess.drop(conf.numToSkip) else toProcess
    val taken = if (conf.numToRun > 0) skipped.take(conf.numToRun) else toProcess

    taken

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


  def segmentDocument(conf: AppConfig): Unit = {

    val artifactOutputName = "docseg.json"
    processCorpus(conf, artifactOutputName, proc)

    def proc(pdfins: InputStream, outputPath: String): String = {
      val segmenter = segment.DocumentSegmenter.createSegmenter(pdfins)
      segmenter.runPageSegmentation()
      ComponentRendering.serializeDocument(segmenter.pages).toString()
    }

  }


  def createBoundingBoxSvg(conf: AppConfig): Unit = {

    processCorpus(conf, "bbox.svg", (pdfins: InputStream, outputPath: String) => {
      extract.DocumentExtractor.extractBBoxesAsSvg(pdfins, Some(outputPath))
    })

  }

  def extractCharacters(conf: AppConfig): Unit = {
    import TB._
    // import watrmarks._
    val artifactOutputName = "chars.txt"

    processCorpus(conf, artifactOutputName: String, proc)

    def proc(pdf: InputStream, outputPath: String): String = {
      val pageChars = DocumentExtractor
        .extractChars(pdf)
        .map({case(pageRegions, pageGeom) =>
          val sortedYPage = pageRegions.regions
            .collect({case c: CharRegion => c})
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
