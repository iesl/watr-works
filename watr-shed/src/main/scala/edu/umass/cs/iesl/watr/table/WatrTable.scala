package edu.umass.cs.iesl.watr
package table

import ammonite.ops._

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import extract.images._
import corpora._
import segment._
import bioarxiv._

import TypeTags._

import docstore._

object WatrTable extends App {

  import ShellCommands._

  def run(args: Array[String]): Unit = {
    val dbname = args(0)

    val db = initReflowDB(dbname)

    replMain().run(
      "corpus" -> initCorpus(),
      "db" -> db,
      "barx" -> BioArxivOps
    )

    db.shutdown()
  }

  val predef =
    s"""| import edu.umass.cs.iesl.watr
        | import ammonite.ops._
        |import ammonite.ops.ImplicitWd._
        |import watr._, spindex._, geometry._, table._
        |import corpora._
        |import textreflow._
        |import textreflow.data._
        |import docstore._
        |import bioarxiv._, BioArxiv._, BioArxivOps._
        |import watrmarks.StandardLabels._
        |import ShellPrettyPrinters._
        |import ShellCommands._
        |import labeling.SampleLabelWidgets
        |implicit val pp0 = pprintComponent
        |implicit val pp1 = pprintBox
        |implicit val pp2 = pprintTextReflow
        |implicit val pp3 = pprintLabelWidget
        |implicit val db0: TextReflowDB = db
        |implicit val corpus0: Corpus = corpus
        |implicit val docStore: DocumentCorpus = db.docStore
        |""".stripMargin

  val welcomeBanner = s""">> WatrTable Shell <<"""

  def replMain() = ammonite.Main(
    // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
    predef = predef,
    defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.err,
    verboseOutput = false
  )


  run(args)

}

object ShellCommands extends CorpusEnrichments with DocumentCorpusEnrichments {

  def initReflowDB(dbname: String): TextReflowDB = {
    val doLogging = false
    val loggingProp = if (doLogging) "?loglevel=2" else ""

    val tables = new TextReflowDBTables()
    new TextReflowDB(tables,
      dbname=dbname,
      dbuser="watrworker",
      dbpass="watrpasswd"
    )
  }

  def initCorpus(): Corpus = {
    initCorpus(pwd)
  }

  def setPageImages(corpusEntry: CorpusEntry)(implicit docStore: DocumentCorpus): Unit = {
    for {
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
    } yield {

      val stableId = DocumentID(corpusEntry.entryDescriptor)

      docStore.getDocument(stableId).foreach({docId =>
        val pageImageArtifacts = corpusEntry.ensureArtifactGroup("page-images")
        val pageImages = if (pageImageArtifacts.getArtifacts.isEmpty) {
          ExtractImages.extract(pdfPath, pageImageArtifacts.rootPath)
        } else {
          ExtractImages.load(pageImageArtifacts.rootPath)
        }

        pageImages.images.zipWithIndex.foreach {
          case (image, i) =>
            docStore
              .getPage(docId, PageNum(i))
              .foreach{ pageId =>
                docStore.setPageImage(pageId, image.bytes)
              }
        }
      })
    }
  }

  def segment(corpusEntry: CorpusEntry, extractImages: Boolean=true)(implicit docStore: DocumentCorpus): Unit = {
    for {
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
    } yield {

      val stableId = DocumentID(corpusEntry.entryDescriptor)

      docStore.getDocument(stableId)
        .flatMap({docId =>
          println(s"segmenting ${stableId} ## ${docId} failed. Already exists.")
          None
        })
        .getOrElse {

          println(s"segmenting ${stableId}")

          val segmenter = DocumentSegmenter
            .createSegmenter(stableId, pdfPath, docStore)

          segmenter.runPageSegmentation()
          if (extractImages) {
            print(s"extracting page images")
            val pageImageArtifacts = corpusEntry.ensureArtifactGroup("page-images")
            val pageImages = if (pageImageArtifacts.getArtifacts.isEmpty) {
              ExtractImages.extract(pdfPath, pageImageArtifacts.rootPath)
            } else {
              ExtractImages.load(pageImageArtifacts.rootPath)
            }

            pageImages.images.zipWithIndex
              .foreach {
                case (image, i) =>
                  docStore
                    .getPage(segmenter.docId, PageNum(i))
                    .foreach{ pageId =>
                      print(".")
                      docStore.setPageImage(pageId, image.bytes)
                    }
              }
          }
          println()
        }
    }
  }


}
