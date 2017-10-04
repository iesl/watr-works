package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}, fs._


import segment.{SegmentationLabels => LB}
import corpora.DocumentZoningApi
import geometry._
import extract._
import spindex._

import TypeTags._

trait DocumentLevelFunctions extends DocumentScopeSegmenter

trait DocumentSegmentation extends DocumentLevelFunctions { self =>

  def pageAtomsAndGeometry: Seq[(Seq[ExtractedItem], PageGeometry)]

  protected[segment] def init(): Unit = {
    initPageIndexes()
    // initStartingComponents()
  }

  def getNumberedPages(): Seq[(Int@@PageID, Int@@PageNum)] =
    docStore.getPages(docId).zipWithIndex.map {
      case (a, b) => (a, PageNum(b))
    }

  def getNumberedPageIndexes(): Seq[(Int@@PageID, PageIndex)] =
    docStore.getPages(docId).zipWithIndex.map {
      case (a, b) => (a, mpageIndex.getPageIndex(PageNum(b)))
    }

  private def initPageIndexes(): Unit = {
    val pageRegions = pageAtomsAndGeometry
      .map { case(extractedItems, pageGeometry)  =>
        val pageId = docStore.addPage(docId, pageGeometry.pageNum)
        docStore.setPageGeometry(pageId, pageGeometry.bounds)
        val tr = docStore.getTargetRegion(
          docStore.addTargetRegion(pageId, pageGeometry.bounds)
        )
        tr
      }

    createZone(LB.DocumentPages, pageRegions)
  }

  // private def initStartingComponents(): Unit = {
  //   import utils.SlicingAndDicing._

  //   pageAtomsAndGeometry.zip(getNumberedPageIndexes).foreach {
  //     case ((extractedItems: Seq[ExtractedItem], pageGeometry), (pageId, pageIndex)) =>

  //       val charRuns = extractedItems
  //         .groupByPairsWithIndex {
  //           case (item1, item2, i) =>
  //             item1.charProps.charRunId == item2.charProps.charRunId
  //         }

  //       charRuns.foreach { run =>

  //         val runBeginPt =  Point(run.head.bbox.left, run.head.bbox.bottom)
  //         val runEndPt = Point(run.last.bbox.left, run.last.bbox.bottom)
  //         val runLine = Line(runBeginPt, runEndPt)
  //         val baselineShape = pageIndex.shapes.indexShape(runLine, LB.CharRunBaseline)

  //         pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](baselineShape.id, LB.ExtractedItems, run)

  //         run.foreach { _ match  {
  //           case item:ExtractedItem.CharItem =>
  //             val charAtom = CharAtom(
  //               item.id,
  //               PageRegion(
  //                 StablePage(stableId, pageGeometry.pageNum, pageId),
  //                 item.bbox
  //               ),
  //               item.char,
  //               item.wonkyCharCode
  //             )

  //             val cc = mpageIndex.addCharAtom(charAtom)

  //             if (item.charProps.isRunBegin) {
  //               pageIndex.components.appendToOrdering(LB.CharRunBegin, cc)
  //             }

  //             pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, baselineShape)


  //           case item:ExtractedItem.ImgItem =>

  //             // val pageRegion = PageRegion(
  //             //   StablePage(stableId, pageGeometry.pageNum, pageId),
  //             //   item.bbox
  //             // )

  //             // val imgRegion = PageItem.ImageAtom(pageRegion)
  //             // mpageIndex.addImageAtom(imgRegion)

  //             pageIndex.shapes.indexShape(item.bbox, LB.Image)

  //             val underline = item.bbox.toLine(Dir.Bottom)
  //             val pathUnderline = pageIndex.shapes.indexShape(underline, LB.CharRunBaseline)
  //             pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, pathUnderline)
  //             pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](pathUnderline.id, LB.ExtractedItems, run)


  //           case item:ExtractedItem.PathItem =>

  //             val underline = item.bbox.toLine(Dir.Bottom)
  //             val pathUnderline = pageIndex.shapes.indexShape(underline, LB.CharRunBaseline)
  //             // pageIndex.shapes.extractedItemShapes.put(item.id, LB.CharRun, pathUnderline)
  //             pageIndex.shapes.setShapeAttribute[Seq[ExtractedItem]](pathUnderline.id, LB.ExtractedItems, run)

  //         }}
  //       }
  //   }
  // }


  lazy val pageSegmenters = {

    def createPageSegmenters(): Seq[PageSegmenter] = for {
      (pageId, pageNum) <- getNumberedPages()
    } yield PageSegmenter(pageId, pageNum, self)

    createPageSegmenters()
  }


  def runDocumentSegmentation(): Unit = {
    tracing.VisualTracer.clearPages()

    pageSegmenters.foreach { pageSegmenter =>
      pageSegmenter.runPageSegmentation()
    }

    // pageSegmenters.foreach { pageSegmenter =>
    //   pageSegmenter.runLineClassification()
    // }

  }

}


trait DocumentSegmenter extends DocumentSegmentation

object DocumentSegmenter {
  import spindex._

  def createSegmenter(
    stableId0: String@@DocumentID,
    pdfPath: Path,
    docStore0: DocumentZoningApi
  ): DocumentSegmentation = {

    val pages = PdfTextExtractor.extractPages(stableId0, pdfPath)

    val segmenter = new DocumentSegmentation {
      override val pageAtomsAndGeometry = pages
      override val mpageIndex: MultiPageIndex = new MultiPageIndex(stableId0, docStore0, pages)

      override val docStore: DocumentZoningApi = docStore0
      override val stableId = stableId0
      override val docId = docStore0.addDocument(stableId0)

      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()

    }

    segmenter.init()
    segmenter
  }
}
