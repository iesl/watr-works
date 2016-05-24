package edu.umass.cs.iesl
package watr
package ext

import ammonite.ops._
import edu.umass.cs.iesl.watr.watrmarks.ZoneIndexer

import java.nio.{file => nio}

object Works extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    entry: Option[JFile] = None,
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
    head("itext zone extractor", "0.1")

    note("Run svg text extraction and analysis")

    opt[Unit]('x', "overwrite") action { (v, conf) =>
      conf.copy(force = true) } text("force overwrite of existing files")

    opt[JFile]('a', "artifact") action { (v, conf) =>
      conf.copy(entry = Option(v))
    } text("artifact id (same as *.d directory name)")


    opt[JFile]('c', "corpus") action { (v, conf) =>
      conf.copy(corpusRoot = Option(v))
    } text ("root path of the corpus")

    cmd("init") action { (_, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        normalizeCorpusEntry(ac)
      })
    } text ("init (or re-init) a corpus directory structure") // children()

    cmd("cerm") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        extractCermineZones(ac)
      })
    } text ("run Cermine zone extractor") // children()

    cmd("lseg") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        lineseg(ac)
      })
    } text ("run Cermine zone extractor") // children()

    cmd("cols") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        printColumns(ac)
      })
    } text ("run Cermine zone extractor") // children()
  }


  val config = parser.parse(args, AppConfig()).getOrElse{
    sys.error(parser.usage)
  }


  val croot = config
    .corpusRoot.map{ r => cwd/RelPath(r) }
    .filter { _.isDir }
    .getOrElse { sys.error(s"corpus root is not dir ('${config.corpusRoot}')") }


  config.exec.foreach { _.apply(config) }

  def getProcessList(conf: AppConfig): Seq[CorpusEntry] = {
    val corpus = Corpus(corpusRootOrDie(conf))

    val toProcess = conf.entry match {
      case Some(entry) => Seq(corpus.entry(entry.getName))
      case None => corpus.entries()
    }
    toProcess
  }

  def printColumns(conf: AppConfig): Unit = {
    // import watrmarks.TB._
    import watrmarks._
    import docseg._

    val artifactOutputName = "columns.txt"
    getProcessList(conf).foreach { entry =>
      def process(): Unit = {
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
                docstrum.determineLines_v2(pageId, docstrum.pages.getComponents(pageId))
              }

              val accum = PageSegAccumulator(
                allPageLines,
                Point(0, 0)
              )
              // get document-wide stats
              val accum2 = docstrum.getDocumentWideStats(accum)

              val pageZones = for {
                pageId <- docstrum.pages.getPages
              } yield {
                println(s"zoning page ${pageId}")
                docstrum.determineZones_v2(pageId, accum2)
              }

              val output = pageZones.zipWithIndex.map{ case (zones, pagenum) =>
                zones.toList.map({ zone =>
                  zone.map(_.tokenizeLine().toText).toList.mkString(s"Column\n", "\n", "\n")
                }).mkString(s"Page $pagenum\n", "\n", "\n\n")
              }.mkString(s"Document \n", "\n", "\n\n")

              output
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


      println(s"extracting ${entry.corpus}: ${entry} ${artifactOutputName}")
      if (entry.hasArtifact(artifactOutputName)) {
        if (conf.force){
          entry.deleteArtifact(artifactOutputName)
          process()
        } else {
          println(s"skipping existing ${entry}, use -x to force reprocessing")
        }
      } else {
        process()
      }
    }
  }



  def lineseg(conf: AppConfig): Unit = {
    import watrmarks.TB._

    val artifactOutputName = "lineseg.txt"
    getProcessList(conf).foreach { entry =>
      def process(): Unit = {
        val pdfArtifact = entry.getPdfArtifact()


        entry.putArtifact(artifactOutputName,
          pdfArtifact.asInputStream.map{ pdf =>

            val zoneIndex = ZoneIndexer.loadSpatialIndices(
              CermineExtractor.extractChars(pdf)
            )

            zoneIndex.getPages.take(2).map { page =>

              val docstrum = new docseg.DocstrumSegmenter(zoneIndex)

              val lines = docstrum.determineLines_v2(
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


      println(s"extracting ${entry.corpus}: ${entry} ${artifactOutputName}")
      if (entry.hasArtifact(artifactOutputName)) {
        if (conf.force){
          entry.deleteArtifact(artifactOutputName)
          process()
        } else {
          println(s"skipping existing ${entry}, use -x to force reprocessing")
        }
      } else {
        process()
      }
    }
  }

  def extractCermineZones(conf: AppConfig): Unit = {
    getProcessList(conf).foreach { entry =>
      def process(): Unit = {
        val pdfArtifact = entry.getPdfArtifact()

        val sha1 = pdfArtifact.asPath.map{p =>
          val bytes = nio.Files.readAllBytes(p.toNIO)
          DigestUtils.shaHex(bytes)
        }.getOrElse { sys.error(s"could not read byte array for ${pdfArtifact}") }

        entry.putArtifact("cermine-zones.json",
          pdfArtifact.asInputStream.map{is =>
            CermineExtractor.cermineZonesToJson(is)
          }.getOrElse { sys.error(s"could not extract cermine data for ${pdfArtifact}") }
        )
      }

      println(s"extractCermineZones(${entry.corpus}, ${entry}")
      if (entry.hasArtifact("cermine-zones.json")) {
        if (conf.force){
          entry.deleteArtifact("cermine-zones.json")
          process()
        } else {
          println(s"skipping existing ${entry}, use -x to force reprocessing")
        }
      } else {
        process()
      }
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
