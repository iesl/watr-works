package edu.umass.cs.iesl.watr
package segment

import edu.umass.cs.iesl.watr.corpora.DocumentZoningApi
import edu.umass.cs.iesl.watr.extract.PdfTextExtractor

import ammonite.{ops => fs}, fs._
import watrmarks.{StandardLabels => LB, _}

import geometry._
import TypeTags._
import shapeless.lens
import PageComponentImplicits._

import org.dianahep.histogrammar.ascii._

trait DocumentLevelFunctions extends DocumentScopeSegmenter

trait DocumentSegmentation extends DocumentLevelFunctions { self =>


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
      new PageSegmenter(pageId, PageNum(pagenum), docScope)
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


trait DocumentSegmenter extends DocumentSegmentation

object DocumentSegmenter {
  import spindex._


  def createSegmenter(
    stableId: String@@DocumentID,
    pdfPath: Path,
    docStore: DocumentZoningApi
  ): DocumentSegmentation = {
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

    new DocumentSegmentation {
      override val mpageIndex: MultiPageIndex = mpageIndex
      override val docStore: DocumentZoningApi = docStore
      override val stableId = mpageIndex.getStableId
      override val docId = docStore.getDocument(stableId)
        .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()

    }
  }


}
