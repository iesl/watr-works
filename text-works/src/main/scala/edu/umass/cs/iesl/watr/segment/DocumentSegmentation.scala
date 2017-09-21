package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}, fs._

import watrmarks.{StandardLabels => LB}
import corpora.DocumentZoningApi
import extract.PdfTextExtractor
import geometry._
// import geometry.syntax._
import extract._
// import utils.{RelativeDirection => Dir}
import spindex.PageIndex

// import utils.ExactFloats._
import TypeTags._

trait DocumentLevelFunctions extends DocumentScopeSegmenter

trait DocumentSegmentation extends DocumentLevelFunctions { self =>

  def pageAtomsAndGeometry: Seq[(Seq[ExtractedItem], PageGeometry)]

  protected[segment] def init(): Unit = {
    initPageIndexes()
    initStartingComponents()
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
        mpageIndex.addPage(pageGeometry)
        docStore.getTargetRegion(
          docStore.addTargetRegion(pageId, pageGeometry.bounds)
        )
      }
    createZone(LB.DocumentPages, pageRegions)
  }

  private def initStartingComponents(): Unit = {

    pageAtomsAndGeometry.zip(getNumberedPageIndexes).foreach {
      case ((extractedItems: Seq[ExtractedItem], pageGeometry), (pageId, pageIndex)) =>


        extractedItems.foreach { _ match {

          case item:ExtractedItem.CharItem =>
            val charAtom = CharAtom(
              item.id,
              PageRegion(
                StablePage(stableId, pageGeometry.pageNum, pageId),
                item.bbox
              ),
              item.char,
              item.wonkyCharCode
            )

            val cc = mpageIndex.addCharAtom(charAtom)
            if (item.charProps.isRunBegin) {
              pageIndex.appendToOrdering(LB.ExtractedLineStarts, cc)
            }


          case item:ExtractedItem.ImgItem =>
            val pageRegion = PageRegion(
              StablePage(stableId, pageGeometry.pageNum, pageId),
              item.bbox
            )

            val imgRegion = PageItem.ImageAtom(pageRegion)
            mpageIndex.addImageAtom(imgRegion)

            pageIndex.addShape(item.bbox, LB.Image)


          case item:ExtractedItem.PathItem =>

        }}

        val pageNum = pageIndex.pageNum

        // Label all page images
        mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
          mpageIndex.labelRegion(Seq(imgCC), LB.Image)
        }

        // markLeftRightColumns()
    }

  }


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
      override val mpageIndex: MultiPageIndex = new MultiPageIndex(stableId0, docStore0)

      override val docStore: DocumentZoningApi = docStore0
      override val stableId = stableId0
      override val docId = docStore0.addDocument(stableId0)

      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()

    }

    segmenter.init()
    segmenter
  }
}


