package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}, fs._

import watrmarks.{StandardLabels => LB}
import corpora.DocumentZoningApi
import extract.PdfTextExtractor
import geometry._
import extract._

import TypeTags._

trait DocumentLevelFunctions extends DocumentScopeSegmenter

trait DocumentSegmentation extends DocumentLevelFunctions { self =>

  lazy val pageSegmenters = {

    def createPageSegmenters(): Seq[PageSegmenter] = for {
      (pageId, pagenum) <- docStore.getPages(docId).zipWithIndex
    } yield PageSegmenter(pageId, PageNum(pagenum), self)

    createPageSegmenters()
  }


  def runDocumentSegmentation(): Unit = {
    tracing.VisualTracer.clearPages()

    pageSegmenters.foreach { pageSegmenter =>
      pageSegmenter.runPageSegmentation()
    }

    pageSegmenters.foreach { pageSegmenter =>
      pageSegmenter.runLineClassification()
    }

    recordPageRegion()

  }

  def recordPageRegion(): Unit = {
    val pageRegions = pageSegmenters.map { pageSegmenter =>
      docStore.getTargetRegion(
        docStore.addTargetRegion(pageSegmenter.pageId, pageSegmenter.pageGeometry)
      )
    }

    createZone(LB.DocumentPages, pageRegions)
  }

  // def printPageStats(): Unit = {
  //   import org.dianahep.histogrammar.ascii._
  //   pageSegmenters.zipWithIndex
  //     .foreach { case (pageSegmenter, pageNum)  =>
  //       val pageStats = pageSegmenter.pageStats
  //       println(s"Page ${pageNum} Stats")
  //       println(pageStats.trapezoidHeights.ascii)
  //       println("\n\n" )
  //       println(pageStats.leftAcuteBaseAngles.ascii)
  //       println("\n\n" )
  //       println(pageStats.leftObtuseBaseAngles.ascii)
  //       println("\n\n" )
  //     }
  // }

}


trait DocumentSegmenter extends DocumentSegmentation

object DocumentSegmenter {
  import spindex._

  def createSegmenter(
    stableId0: String@@DocumentID,
    pdfPath: Path,
    docStore0: DocumentZoningApi
  ): DocumentSegmentation = {
    println(s"extracting ${stableId0} chars")

    val pageAtomsAndGeometry = PdfTextExtractor.extractPages(stableId0, pdfPath)
    val mpageIndex0 = new MultiPageIndex(stableId0, docStore0)

    val docId0 = docStore0.addDocument(stableId0)

    pageAtomsAndGeometry.foreach { case(extractedItems, geom)  =>
      val pageId = docStore0.addPage(docId0, geom.pageNum)
      docStore0.setPageGeometry(pageId, geom.bounds)
      val pageIndex = mpageIndex0.addPage(geom)

      extractedItems.foreach { _ match {

        case item:ExtractedItem.CharItem if !item.isNonPrintable =>
          val charAtom = CharAtom(
            item.id,
            PageRegion(
              StablePage(stableId0, geom.pageNum, pageId),
              item.bbox
            ),
            item.char,
            item.wonkyCharCode
          )
          mpageIndex0.addCharAtom(charAtom)

          item.charProps match {

            case prop: CharBioProp.LastChar =>
            case prop: CharBioProp.InsChar =>
            case prop: CharBioProp.BegChar =>
            case prop: CharBioProp.OutChar.type =>
          }

        case item:ExtractedItem.ImgItem =>
          val pageRegion = PageRegion(
            StablePage(stableId0, geom.pageNum, pageId),
            item.bbox
          )

          val imgRegion = PageItem.ImageAtom(pageRegion)
          mpageIndex0.addImageAtom(imgRegion)

        case item:ExtractedItem.PathItem =>
          // val pageRegion = PageRegion(
          //   StablePage(stableId0, geom.pageNum, pageId),
          //   item.bbox
          // )

          // val imgRegion = PageItem.Path(pageRegion)
          //   mpageIndex0.addPathItem(update)

        case _: ExtractedItem.CharItem =>
          // skip non-printable items


      }}

    }

    // pageIndexes.foreach { pageIndex =>
    //   val pageId = docStore0.addPage(docId0, pageIndex.pageGeometry.pageNum)
    //   docStore0.setPageGeometry(pageId, pageIndex.pageGeometry.bounds)
    //   mpageIndex0.addPageIndex(pageIndex)
    // }

    new DocumentSegmentation {
      override val mpageIndex: MultiPageIndex = mpageIndex0
      override val docStore: DocumentZoningApi = docStore0
      override val stableId = stableId0
      override val docId = docId0

      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()

    }
  }

  // def createSegmenter(
  //   stableId0: String@@DocumentID,
  //   pdfPath: Path,
  //   docStore0: DocumentZoningApi
  // ): DocumentSegmentation = {
  //   println(s"extracting ${stableId0} chars")

  //   val pageAtomsAndGeometry = PdfTextExtractor.extractChars(stableId0, pdfPath)
  //   val mpageIndex0 = new MultiPageIndex(stableId0, docStore0)

  //   val pageIdL = lens[CharAtom].pageRegion.page.pageId
  //   val imgPageIdL = lens[PageItem.ImageAtom].pageRegion.page.pageId
  //   val pathPageIdL = lens[PageItem.Path].pageRegion.page.pageId

  //   val docId0 = docStore0.addDocument(stableId0)

  //   pageAtomsAndGeometry.foreach { case(regions, geom)  =>
  //     val pageId = docStore0.addPage(docId0, geom.id)
  //     docStore0.setPageGeometry(pageId, geom.bounds)
  //     mpageIndex0.addPage(geom)

  //     regions.foreach {
  //       case cb:CharAtom if !cb.isNonPrintable =>
  //         // modify the pageId to match the one assigned by docStore0
  //         val update = pageIdL.modify(cb){_ => pageId}
  //         mpageIndex0.addCharAtom(update)

  //       case cb:PageItem.ImageAtom =>
  //         val update = imgPageIdL.modify(cb){_ => pageId}
  //         mpageIndex0.addImageAtom(update)

  //       case cb:PageItem.Path =>
  //         val update = pathPageIdL.modify(cb){_ => pageId}
  //         mpageIndex0.addPathItem(update)

  //       case cb => println(s"error adding ${cb}")
  //     }
  //   }

  //   new DocumentSegmentation {
  //     override val mpageIndex: MultiPageIndex = mpageIndex0
  //     override val docStore: DocumentZoningApi = docStore0
  //     override val stableId = stableId0
  //     override val docId = docId0

  //     override val docStats: DocumentLayoutStats = new DocumentLayoutStats()

  //   }
  // }


}
