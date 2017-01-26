package edu.umass.cs.iesl.watr
package table

import ammonite.ops._
import pprint.PPrinter

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import spindex._
import textreflow._
import textreflow.data._
import extract.images._
import corpora._
import segment._

import textboxing.{TextBoxing => TB}, TB._
import watrmarks.{StandardLabels => LB}
import TypeTags._

import display._
import display.data._
import geometry._

object WatrTable {

  import ShellCommands._

  def main(args: Array[String]): Unit = {

    replMain().run(
      "corpus" -> initCorpus(),
      "db" -> initReflowDB()
    )
  }

  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import watr._, spindex._, geometry._, table._
        |import textreflow._
        |import watrmarks.StandardLabels._
        |import ShellCommands._
        |implicit val pp0 = pprintComponent
        |implicit val pp1 = pprintBox
        |implicit val pp2 = pprintTextReflow
        |implicit val pp3 = pprintLabelWidget
        |implicit val db0: TextReflowDB = db
        |""".stripMargin

  val welcomeBanner = s""">> WatrTable Shell <<"""

  def replMain() = ammonite.Main(
    // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
    predef = predef,
    // defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.err,
    verboseOutput = false
  )

}

object ShellCommands extends CorpusEnrichments {

  def initReflowDB(): TextReflowDB = {
    import doobie.imports._
    import scalaz.concurrent.Task

    val xa = DriverManagerTransactor[Task](
      "org.postgresql.Driver",
      "jdbc:postgresql:watrdev",
      "watrworker", "watrpasswd"
    )

    val tables = new TextReflowDBTables(xa)
    new TextReflowDB(tables)
  }

  val pprintComponent: PPrinter[Component] = PPrinter({(component, config) =>
    val box = component.show
    Iterator(box.toString())
  })

  val pprintBox: PPrinter[TB.Box] = PPrinter({(box, config) =>
    Iterator(box.toString())
  })

  val pprintTextReflow: PPrinter[TextReflow] = PPrinter({(textReflow, config) =>
    val text = textReflow.toText()
    Iterator("text")
  })

  val pprintLabelWidget: PPrinter[LabelWidget] = PPrinter({(lwidget, config) =>
    val pp = LabelWidgetIndexing.prettyPrintLabelWidget(lwidget)
    Iterator(pp.toString)
  })

  def initCorpus(): Corpus = {
    initCorpus(pwd)
  }

  implicit class RicherDocumentSegmenter(val theSegmentation: DocumentSegmentation) extends AnyVal {

    def lines(): Seq[TextReflow] = {
      theSegmentation.mpageIndex
        .getVisualLineTextReflows()
    }

    def pageImages(): PageImages = {
      theSegmentation.pageImages
    }

  }

  implicit class RicherTextReflowDB(val theDB: TextReflowDB) extends AnyVal {
    def dropAndCreateTables(): Unit = {
      theDB.tables.dropAndCreate.unsafePerformSync
    }

    def addSegmentation(ds: DocumentSegmentation): Unit = {
      theDB.addSegmentation(ds)
    }


    def documents(): List[String@@DocumentID] = {
      theDB.getDocuments()
    }

    def titleLabelers(n: Int): LabelWidget = {
      val lws = documents.take(n)
        .map(titleLabeler(_))

      LW.col(lws:_*)
    }

    def titleLabeler(docId: String@@DocumentID): LabelWidget = {
      // val Lw = LabelWidgets

      // - presumptively label the title lines
      //    val tallestLines = page0.vlines.filter(_.height == tallest font height)
      //    val titleZone: List[Zone] = Zone(tallestLines, LB.Title)

      // - display:
      //   - top half of page 1 w/ title labeling rects and visual line indicators
      //   - "accept" button
      //   - clear title label button
      //   - rectangle select for labeling
      //   - ? maybe show the extracted text w/button to indicate errors

      val page0 = PageID(0)
      val r0 = RegionID(0)

      val pageGeometry = theDB.getPageGeometry(docId, page0)

      val pageTargetRegion = TargetRegion(r0, docId, page0,
        pageGeometry.bounds.copy(
          top = pageGeometry.bounds.top,
          height = pageGeometry.bounds.height / 3.0
        )
      )

      // println(s"titleLabeler: pageGeometry=${pageGeometry},  half-page = ${pageTargetRegion}")

      // Find VisualLine zones that are likely titles and pre-select them
      val zones = theDB.selectZones(docId, page0, LB.VisualLine)
      val vlines = for {
        zone   <- zones
        region <- zone.regions
      } yield { region }

      val titlePreselects = vlines.drop(0).take(2)

      val halfPage = LW.targetImage(
        pageTargetRegion // , List(LB.VisualLine), titlePreselects
      )

      val halfPageWSelects = LW.withSelections(halfPage, titlePreselects:_*)

      val vlineText = for {
        region <- titlePreselects
        (zone, reflow) <- theDB.getTextReflowsForTargetRegion(region)
      } yield {
        LW.reflow(reflow)
      }

      val textCol = LW.col(
        vlineText:_*
      )

      // val selector = LW.panel(
      //   LW.row(
      //     LW.mouseOverlay(
      //       halfPageWSelects
      //     ),
      //     textCol
      //   )
      // )
      // val selector = LW.panel(halfPageWSelects)
      val selector = halfPageWSelects

      // val buttons = col(
      //   toggle("accept", "unaccept")
      //   button("clear selections")
      //   ok, err, clearSelects
      // )

      // val finalWidget = row(
      //   col(
      //     annotWidget,
      //     textDisplay
      //   ),
      //   buttons
      // )

      // // web client implementation
      // val okButton = fabric.Text("✓")
      // val errButton = "𝐗"

      selector
    }

  }

  implicit class RicherCorpusEntry(val theCorpusEntry: CorpusEntry) extends AnyVal {

    def segment(): Option[DocumentSegmentation] = {
      for {
        pdfArtifact    <- theCorpusEntry.getPdfArtifact
        pdfPath        <- pdfArtifact.asPath.toOption
      } yield {

        val docId = DocumentID(theCorpusEntry.entryDescriptor)

        val segmenter = DocumentSegmenter
          .createSegmenter(docId, pdfPath)

        segmenter.runPageSegmentation()


        val pageImageArtifacts = theCorpusEntry.ensureArtifactGroup("page-images")

        val pageImages = if (pageImageArtifacts.getArtifacts.isEmpty) {
          ExtractImages.extract(pdfPath, pageImageArtifacts.rootPath)
        } else {
          ExtractImages.load(pageImageArtifacts.rootPath)
        }



        DocumentSegmentation(
          segmenter.mpageIndex,
          pageImages
        )
      }
    }

  }


}
