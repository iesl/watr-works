package edu.umass.cs.iesl
package watr
package extract

import ammonite.ops._
import edu.umass.cs.iesl.watr.watrmarks.ZoneIndexer

// import java.nio.{file => nio}

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

    cmd("cols") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        printColumns(ac)
      })
    } text ("run column detection (for debugging)")

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


  def detectParagraphs(conf: AppConfig): Unit = {
    import segment._

    val artifactOutputNames = "docseg.json"

    runProcessor(conf, artifactOutputNames: String, process)

    def process(entry: CorpusEntry, outputName: String): Unit = {
      val pdfArtifact = entry.getPdfArtifact()
      entry.putArtifact(outputName,

        pdfArtifact.asInputStream.map{ pdf =>

          try {
            val output = DocstrumSegmenter.segmentPages(
              CermineExtractor.extractChars(pdf)
            )

            val charsSeen = CharacterAccumulator.charSet.grouped(40).map(
              _.mkString(", ")
            ).mkString("Chars\n", "\n" , "\n\n")
            println(charsSeen)
            // CharacterAccumulator.charSet.clear()

            output
          } catch {
            case t: Throwable =>
              println(s"could not extract ${outputName}  for ${pdfArtifact}: ${t.getMessage}")
              s"""{ "error": "exception thrown ${t}: ${t.getCause}: ${t.getMessage}" }"""
          }
        }. recover({
          case t: Throwable =>
            (s"could not extract ${outputName}  for ${pdfArtifact}: ${t.getMessage}")
        }).getOrElse {
          println(s"could not extract ${outputName}  for ${pdfArtifact}")
          """{ "error" }"""
        }
        )
      }
  }


  def extractCharacters(conf: AppConfig): Unit = {
    import watrmarks.TB._
    import watrmarks.Bounds._
    import watrmarks._
    val artifactOutputName = "chars.txt"

    runProcessor(conf, artifactOutputName: String, process: (CorpusEntry, String) => Unit)

    def process(entry: CorpusEntry, outputName: String): Unit = {
      val pdfArtifact = entry.getPdfArtifact()

      entry.putArtifact(artifactOutputName,
        pdfArtifact.asInputStream.map{ pdf =>
          try {
            val pageChars = CermineExtractor
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

            } catch {
              case t: Throwable =>
                sys.error(s"could not extract ${artifactOutputName}  for ${pdfArtifact}: ${t.getMessage}")
            }
          }. recover({
            case t: Throwable =>
              sys.error(s"could not extract ${artifactOutputName}  for ${pdfArtifact}: ${t.getMessage}")
          }).getOrElse { sys.error(s"could not extract ${artifactOutputName}  for ${pdfArtifact}") }
        )
      }

  }


  def printColumns(conf: AppConfig): Unit = {
    // import watrmarks.TB._
    import watrmarks._
    import segment._

    val artifactOutputName = "columns.txt"
    runProcessor(conf: AppConfig, artifactOutputName: String, process: (CorpusEntry, String) => Unit)

    def process(entry: CorpusEntry, outputName: String): Unit = {
      val pdfArtifact = entry.getPdfArtifact()


      entry.putArtifact(artifactOutputName,

        pdfArtifact.asInputStream.map{ pdf =>

          try {


            val zoneIndex = ZoneIndexer.loadSpatialIndices(
              CermineExtractor.extractChars(pdf)
            )

            val docstrum = new DocstrumSegmenter(zoneIndex)

            val allPageLines = for {
              pageId <- docstrum.pages.getPages
            } yield {
              docstrum.determineLines(pageId, docstrum.pages.getComponents(pageId))
            }

            val accum = PageSegAccumulator(
              allPageLines
            )
            // get document-wide stats
            val accum2 = docstrum.getDocumentWideStats(accum)

            val pageZones = for {
              pageId <- docstrum.pages.getPages
            } yield {
              println(s"zoning page ${pageId}")
              docstrum.determineZones(pageId, accum2)
            }

            // val output = pageZones.zipWithIndex.map{ case (zones, pagenum) =>
            //   zones.toList.map({ zone =>
            //     zone.map(_.tokenizeLine().toText).toList.mkString(s"Column\n", "\n", "\n")
            //   }).mkString(s"Page $pagenum\n", "\n", "\n\n")
            // }.mkString(s"Document \n", "\n", "\n\n")

            // output
            ""
          } catch {
            case t: Throwable =>
              sys.error(s"could not extract ${artifactOutputName}  for ${pdfArtifact}: ${t.getMessage}")
          }
        }. recover({
          case t: Throwable =>
            sys.error(s"could not extract ${artifactOutputName}  for ${pdfArtifact}: ${t.getMessage}")
        }).getOrElse { sys.error(s"could not extract ${artifactOutputName}  for ${pdfArtifact}") }
      )
    }


  }




  def lineseg(conf: AppConfig): Unit = {
    import watrmarks.TB._

    val artifactOutputName = "lineseg.txt"
    runProcessor(conf: AppConfig, artifactOutputName: String, process: (CorpusEntry, String) => Unit)

    def process(entry: CorpusEntry, outputName: String): Unit = {
      val pdfArtifact = entry.getPdfArtifact()


      entry.putArtifact(artifactOutputName,
        pdfArtifact.asInputStream.map{ pdf =>

          val zoneIndex = ZoneIndexer.loadSpatialIndices(
            CermineExtractor.extractChars(pdf)
          )

          zoneIndex.getPages.take(2).map { page =>

            val docstrum = new segment.DocstrumSegmenter(zoneIndex)

            val lines = docstrum.determineLines(
              page,
              zoneIndex.getComponents(page)
            )

            val lineCols = lines
              .sortBy(_.findCenterY())
              .map({ l =>
                val lineBounds = l.bounds.prettyPrint
                val tokenized = l.tokenizeLine().toText
                // s"${tokenized}               ${lineBounds}"
                (tokenized.box, lineBounds.box)
              })

            val justified =
              s"\nPage:${page} file://${pdfArtifact.artifactPath}" %|
                (vcat(left)(lineCols.map(_._1).toList) + "    " + vcat(right)(lineCols.map(_._2).toList))

            justified.toString()

            // .mkString(s"\nPage:${page} file://${pdfArtifact.artifactPath}\n  ", "\n", "\n")

          }.mkString(s"\nDocument: file://${pdfArtifact.artifactPath}\n", "\n", "\n")


        }.getOrElse { sys.error(s"could not extract ${artifactOutputName}  for ${pdfArtifact}") }
      )
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
