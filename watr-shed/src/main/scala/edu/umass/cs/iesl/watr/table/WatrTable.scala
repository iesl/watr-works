package edu.umass.cs.iesl.watr
package table

import ammonite.ops._
import pprint.PPrinter

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import spindex._
import textreflow.data._
import extract.images._
import corpora._
import segment._
import bioarxiv._

import textboxing.{TextBoxing => TB}, TB._
// import watrmarks.{StandardLabels => LB}
import TypeTags._

import geometry._
import labeling._
import labeling.data._
import docstore._

object WatrTable {

  import ShellCommands._

  def main(args: Array[String]): Unit = {
    val dbname = args(0)

    replMain().run(
      "corpus" -> initCorpus(),
      "db" -> initReflowDB(dbname),
      "barx" -> BioArxivOps
    )
  }

  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import ammonite.ops._
        |import ammonite.ops.ImplicitWd._
        |import watr._, spindex._, geometry._, table._
        |import corpora.Corpus
        |import textreflow._
        |import watrmarks.StandardLabels._
        |import ShellCommands._
        |import BioArxiv._
        |import BioArxivOps._
        |implicit val pp0 = pprintComponent
        |implicit val pp1 = pprintBox
        |implicit val pp2 = pprintTextReflow
        |implicit val pp3 = pprintLabelWidget
        |implicit val db0: TextReflowDB = db
        |implicit val corpus0: Corpus = corpus
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

}

object ShellCommands extends CorpusEnrichments {

  def initReflowDB(dbname: String): TextReflowDB = {
    import doobie.imports._
    import scalaz.concurrent.Task

    val doLogging = false
    val loggingProp = if (doLogging) "?loglevel=2" else ""

    val xa = DriverManagerTransactor[Task](
      "org.postgresql.Driver",
      s"jdbc:postgresql:${dbname}${loggingProp}",
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
    Iterator(text)
  })

  val pprintLabelWidget: PPrinter[LabelWidget] = PPrinter({(lwidget, config) =>
    val pp = LabelWidgetIndex.prettyPrintLabelWidget(lwidget)
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
    def docStorage = theDB.docstorage

    def dropAndCreateTables(): Unit = {
      theDB.tables.dropAndCreate.unsafePerformSync
    }

    def addSegmentation(ds: DocumentSegmentation): Unit = {
      theDB.addSegmentation(ds)
    }


    def documents(n: Int=0, skip: Int=0): List[String@@DocumentID] = {
      val allEntries = theDB.getDocuments()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }

    def titleLabelers(n: Int): LabelWidget = {
      val lws = documents(n)
        .map(titleLabeler(_))

      LW.col(lws:_*)
    }

    import BioArxiv._
    import AlignBioArxiv._
    import scala.collection.mutable
    import LabelWidgetUtils._

    def bioarxivLabelers(n: Int = 0, skip: Int = 0)(implicit corpus: Corpus): LabelWidget = {
      val lws = for {
        docId <- documents(n, skip)
        entry <- corpus.entry(docId.unwrap)
        rec   <- entry.getBioarxivJsonArtifact
      } yield { bioArxivLabeler(docId, rec) }

      val controls = makeButtons(
        "Next Labeling Tasks",
        "Finished"
      )

      LW.panel(
        LW.col(
          controls,
          LW.col(lws:_*)
        )
      )
    }

    def nameLabelers(n: Int = 0, skip: Int = 0)(implicit corpus: Corpus): LabelWidget = {
      val docs = for {
        docId <- documents(n, skip)
        entry <- corpus.entry(docId.unwrap)
        rec   <- entry.getBioarxivJsonArtifact
      } yield (docId, rec)

      val nameLabeler  = AuthorNameLabelers.nameLabeler(theDB, docs)

      LW.panel(
        LW.col(
          pageControls,
          nameLabeler
        )
      )
    }


    def getZonesForPage(stableId: String@@DocumentID, pageNum: Int@@PageNum): Seq[Int@@ZoneID] = {
      val pageZones = for {
        docId <- docStorage.getDocument(stableId)
        pageId <- docStorage.getPage(docId, pageNum)
      } yield for {
        tr <- docStorage.getTargetRegions(pageId)
        zoneId <- docStorage.getZonesForTargetRegion(tr)
      } yield { zoneId }

      pageZones
        .getOrElse(sys.error(s"getZonesForPage: error getting ${stableId} page ${pageNum}"))
    }

    def bioArxivLabeler(stableId: String@@DocumentID, paperRec: PaperRec): LabelWidget = {
      val docStorage = theDB.docstorage

      val paperRecWidget = LW.textbox(
        TB.vjoin()(
          paperRec.title,
          indent(4)(
            TB.vcat(
              paperRec.authors.map(_.box)
            )
          )
        )
      )

      val page0 = PageNum(0)
      val r0 = RegionID(0)
      val docId = docStorage.getDocument(stableId)
        .getOrElse(sys.error(s"Trying to access non-existent document ${stableId}"))

      val pageId = docStorage.getPage(docId, page0)
        .getOrElse(sys.error(s"Trying to access non-existent page in doc ${stableId} page 0"))

      val pageGeometry = docStorage.getPageGeometry(pageId)
        // .getOrElse(sys.error(s"Trying to access non-existent page geometry in doc ${stableId} page ${page0}"))

      val pageTargetRegion = TargetRegion(r0, stableId, page0, pageGeometry)



      val allPageLines = for {
        (zoneId, linenum) <- getZonesForPage(stableId, page0).zipWithIndex
        lineReflow <- docStorage.getTextReflowForZone(zoneId)
      } yield {
        val lt = LW.labeledTarget(lineReflow.targetRegion, None, None)
        (linenum, (0d, lt))
      }

      val scores: Seq[AlignmentScores] = AlignBioArxiv.alignPaperWithDB(theDB, paperRec, stableId)

      // linenum -> best-score, label-widget
      val allLineScores = mutable.HashMap[Int, (Double, LabelWidget)]()
      allLineScores ++= allPageLines

      val overlays = scores.map({alignScores =>
        val label = alignScores.alignmentLabel

        val scoreList = alignScores.lineScores.toList
        val maxScore = scoreList.map(_._2).max
        val lineScores = scoreList.filter(_._2 > maxScore/2)

        lineScores.foreach({case (linenum, score) =>
          val lineReflow = alignScores.lineReflows(linenum)
          val lineTarget = lineReflow.targetRegion
          val normalScore = score/maxScore

          val lt = LW.labeledTarget(lineTarget, Some(label), Some(normalScore))

          if (allLineScores.contains(linenum)) {
            val Some((currScore, currWidget)) = allLineScores.get(linenum)

            if (normalScore > currScore) {
              allLineScores.put(linenum, (normalScore, lt))
            }

          } else {
            allLineScores.put(linenum, (normalScore, lt))
          }

        })
      })

      val lwidgets = allLineScores.toList
        .sortBy(_._1)
        .map({case (linenum, (score, lwidget)) =>
          lwidget
        })

      val controls = makeButtons(
        "Clear Selections",
        "Skip",
        "Report Error"
      )

      val body = LW.row(
        LW.targetOverlay(pageTargetRegion, lwidgets),
        paperRecWidget
      )

      LW.pad(
        LW.panel(LW.col(
          controls,
          body
        )),
        Padding(2d, 2d, 2d, 4d)
      )

    }

    def titleLabeler(stableId: String@@DocumentID): LabelWidget = {
      // - presumptively label the title lines
      //    val tallestLines = page0.vlines.filter(_.height == tallest font height)
      //    val titleZone: List[Zone] = Zone(tallestLines, LB.Title)

      // - display:
      //   - top half of page 1 w/ title labeling rects and visual line indicators
      //   - "accept" button
      //   - clear title label button
      //   - rectangle select for labeling
      //   - ? maybe show the extracted text w/button to indicate errors

      val page0 = PageNum(0)
      val r0 = RegionID(0)
      val docId = docStorage.getDocument(stableId)
        .getOrElse(sys.error(s"Trying to access non-existent document ${stableId}"))

      val pageId = docStorage.getPage(docId, page0)
        .getOrElse(sys.error(s"Trying to access non-existent page in doc ${stableId} page 0"))

      val pageGeometry = docStorage.getPageGeometry(pageId)
      // .getOrElse(sys.error(s"Trying to access non-existent page geometry in doc ${stableId} page ${page0}"))


      val pageTargetRegion = TargetRegion(r0, stableId, page0,
        pageGeometry.copy(
          top = pageGeometry.top,
          height = pageGeometry.height / 3.0
        )
      )


      val vlines = for {
        zoneId <- getZonesForPage(stableId, page0)
        zone = docStorage.getZone(zoneId)
        region <- zone.regions
      } yield region

      val titlePreselects = vlines.drop(0).take(2)

      val halfPageWSelects = LW.targetOverlay(
        pageTargetRegion,
        titlePreselects.map(LW.labeledTarget(_))
      )

      // val halfPageWSelects = LW.withSelections(halfPage, titlePreselects:_*)

      val vlineText = for {
        region <- titlePreselects
        (zone, reflow) <- theDB.getTextReflowsForTargetRegion(region)
      } yield  LW.reflow(reflow)

      val textCol = LW.col(vlineText:_*)

      LW.row(halfPageWSelects, textCol)

    }
  }

  implicit class RicherCorpusEntry(val theCorpusEntry: CorpusEntry) extends AnyVal {
    import BioArxiv._
    import BioArxivOps._
    import play.api.libs.json, json._
    import play.api.data.validation.ValidationError

    def getBioarxivJsonArtifact(): Option[PaperRec] = {
      for {
        rec      <- theCorpusEntry.getArtifact("bioarxiv.json")
        asJson   <- rec.asJson.toOption
        paperRec <- asJson.validate[PaperRec].fold(
          (errors: Seq[(JsPath, Seq[ValidationError])]) => {
            println(s"errors: ${errors.length}")

            errors.take(10).foreach { case (errPath, errs) =>
              println(s"$errPath")
              errs.foreach { e =>
                println(s"> $e")
              }
            }
            None

          }, ps => Option(ps))
      } yield  paperRec
    }

    def addEntryToDatabase(implicit db: TextReflowDB): Unit = {
      val docId = DocumentID(theCorpusEntry.entryDescriptor)
      if (!db.hasDocumentID(docId)) {
        segment.foreach(db.addSegmentation(_))
      } else {
        println(s"Skipping ${docId}: already in database")
      }
    }

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

  implicit class RicherCorpus(val thisCorpus: Corpus)  {

    def chooseEntries(n: Int = 0, skip: Int = 0): Seq[CorpusEntry] = {
      val allEntries = thisCorpus.entries()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }

    def formatLineComponent(entry: CorpusEntry, c: Component): TB.Box = {
      c.showUnknowns beside indent(8)(entry.entryDescriptor.box)
    }

  }

}
