package edu.umass.cs.iesl.watr
package segment



import edu.umass.cs.iesl.watr.corpora.DocumentZoningApi
import edu.umass.cs.iesl.watr.extract.PdfTextExtractor
import spindex._
  // import utils.SlicingAndDicing._

import ammonite.{ops => fs}, fs._
import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import shapeless.lens
import PageComponentImplicits._
import edu.umass.cs.iesl.watr.tracing.VisualTracer
import edu.umass.cs.iesl.watr.tracing.TraceCallbacks
import textgrid._
import com.sksamuel.scrimage.{X11Colorlist => Clr, Color}

import org.dianahep.histogrammar.ascii._


object DocumentSegmenter {
  import spindex._

  val noopVisualTracer = new VisualTracer {
    def traceCallbacks: TraceCallbacks = new TraceCallbacks {

    }

  }


  def createSegmenter(
    stableId: String@@DocumentID,
    pdfPath: Path,
    docStore: DocumentZoningApi,
    tracer: VisualTracer = noopVisualTracer
  ): DocumentSegmenter = {
    println(s"extracting ${stableId} chars")

    val pageAtomsAndGeometry = PdfTextExtractor.extractChars(stableId, pdfPath)
    val mpageIndex = new MultiPageIndex(stableId, docStore)

    val pageIdL = lens[CharAtom].pageRegion.page.pageId
    val imgPageIdL = lens[PageItem.ImageAtom].pageRegion.page.pageId
    val pathPageIdL = lens[PageItem.Path].pageRegion.page.pageId

    val docId = docStore.addDocument(stableId)
    pageAtomsAndGeometry.foreach { case(regions, geom)  =>
      val pageId = docStore.addPage(docId, geom.id)
      docStore.setPageGeometry(pageId, geom.bounds)
      mpageIndex.addPage(geom)

      regions.foreach {
        case cb:CharAtom if !cb.isNonPrintable =>
          // modify the pageId to match the one assigned by docStore
          val update = pageIdL.modify(cb){_ => pageId}
          mpageIndex.addCharAtom(update)

        case cb:PageItem.ImageAtom =>
          val update = imgPageIdL.modify(cb){_ => pageId}
          mpageIndex.addImageAtom(update)

        case cb:PageItem.Path =>
          val update = pathPageIdL.modify(cb){_ => pageId}
          mpageIndex.addPathItem(update)

        case cb => println(s"error adding ${cb}")
      }
    }

    new DocumentSegmenter(mpageIndex, tracer)
  }

  val DebugLabelColors: Map[Label, Color] = {
    Map(
      (LB.VisualLineModal        , Clr.Cornsilk4),
      (LB.VisualLine             , Clr.Plum),
      (LB.PageAtomTmp            , Clr.DarkBlue),
      (LB.PageAtomGrp            , Clr.YellowGreen),
      (LB.PageAtom               , Clr.Grey80),
      (LB.PathBounds             , Clr.Thistle),
      (LB.LinePath               , Clr.Green),
      (LB.HLinePath              , Clr.Black),
      (LB.VLinePath              , Clr.LimeGreen),
      (LB.Image                  , Clr.DarkCyan),
      (LB.LineByHash             , Clr.Firebrick3),
      (LB.LeftAlignedCharCol     , Clr.Orange4),
      (LB.WhitespaceColCandidate , Clr.Green),
      (LB.WhitespaceCol          , Clr.Peru),
      (LB.ReadingBlock           , Clr.Red),
      (LB.Marked                 , Clr.Red4)
    )
  }

}

class DocumentSegmenter(
  val mpageIndex: MultiPageIndex,
  val tracer: VisualTracer,
  val docStats: DocumentLayoutStats = new DocumentLayoutStats()
) { documentSegmenter =>

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))


  lazy val pageIdMap: Map[Int@@PageID, Int@@PageNum] =
    docStore.getPages(docId).zipWithIndex.map{
      case (pageId, pageNum) => (pageId, PageNum(pageNum))
    }.toMap


  def createZone(label: Label, pageRegions: Seq[PageRegion]): Option[Int@@ZoneID] = {
    docStore.labelRegions(label, pageRegions)
  }


  def runPageSegmentation(): Unit = {

    val pageSegmenters = for {
      (pageId, pagenum) <- docStore.getPages(docId).zipWithIndex
    } yield {
      new PageSegmenter(pageId, PageNum(pagenum), documentSegmenter)
    }

    val pageRegions = pageSegmenters
      .map { pageSegmenter =>
        pageSegmenter.runLineSegmentation()

        docStore.getTargetRegion(
          docStore.addTargetRegion(pageSegmenter.pageId, pageSegmenter.pageGeometry)
        )
      }

    val _ = createZone(LB.DocumentPages, pageRegions)


    pageSegmenters.zipWithIndex
      .foreach { case (pageSegmenter, pageNum)  =>
        val pageStats = pageSegmenter.pageStats
        println(s"Page ${pageNum} Stats")
        println(pageStats.trapezoidHeights.ascii)
        println("\n\n" )
        println(pageStats.leftAcuteBaseAngles.ascii)
        println("\n\n" )
        println(pageStats.leftObtuseBaseAngles.ascii)
        println("\n\n" )
      }

    pageSegmenters
      .foreach { pageSegmenter =>
        pageSegmenter.runLineClassification()
      }

  }

}

