package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._

import geometry._

import corpora._
import utils.EnrichNumerics._

import LabelWidgets._
import LabelWidgetF._
import TypeTags._


abstract class LabelWidgetTestUtil extends FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {

  def add4x3x3SampleDoc(): Unit = {
    /**
      *  Test document layout (4 papers, grid layout)
      *      0123456
      *    0 abc012|
      *    1 def345|
      *    2 ghi678|
      *    3 jklstu|
      *    4 mnovwx|
      *    5 pqryzz|

      *  */
    val docs = List(
      List( // Single 4-page document
        "abc\ndef\nghi",
        "012\n345\n678",
        "jkl\nmno\npqr",
        "stu\nvwx\nyzz"
      )
    )

    for { (doc, i) <- docs.zipWithIndex } {
      addDocument(DocumentID(s"doc#${i}"), doc)
    }
  }

  def putStrLn(s: String): Seq[Unit] = Seq({
    println(s)
  })

  def debugPrintDocStore(): Unit = {
    for {
      stableId <- docStore.getDocuments
      docId    <- docStore.getDocument(stableId).toSeq
      _        <- putStrLn(s"Document $stableId id:${docId}")
      pageId   <- docStore.getPages(docId)
      _        <- putStrLn(s"  Page  ${pageId}")
    }  {
      // val pageGeometry = docStore.getPageGeometry(pageId)
    }
  }

  def generatePageRegions(divs: Int): Seq[PageRegion] = {
    val allRegions = for {
      stableId <- docStore.getDocuments
      docId    <- docStore.getDocument(stableId).toSeq
      pageId   <- docStore.getPages(docId)
    }  yield {
      val pageGeometry = docStore.getPageGeometry(pageId)

      for {
        yslice <- intervalSlices(DoubleInterval(pageGeometry.top, pageGeometry.height), divs).tails.filterNot(_.isEmpty).map(_.reduce(_ union _))
        // _  =  println(s"yslice ${yslice}")
        xslice <- intervalSlices(DoubleInterval(pageGeometry.left, pageGeometry.width), divs).tails.filterNot(_.isEmpty).map(_.reduce(_ union _))
      } yield {
        // println(s"   x ${xslice}")
        val regionBbox = LTBounds(
          xslice.min, yslice.min,
          xslice.len, yslice.len
        )
        PageRegion(pageId, regionBbox)
      }
    }
    allRegions.flatten
  }

  def printPageRegions(pageRegions: Seq[PageRegion]): Unit = {
    val res = pageRegions
    .sortBy({ p => (p.pageId.unwrap, p.bbox.top, p.bbox.height, p.bbox.left, p.bbox.width) })
      .map(_.toString)
      .mkString("\n  ", "\n  ", "\n")
    println(res)
  }

  def mkTargetRegion(pageId: Int, bbox: Int*): TargetRegion = {
    val pgRegion = mkPageRegion(pageId, bbox:_*)
    val regionId = docStore.addTargetRegion(pgRegion.pageId, pgRegion.bbox)
    docStore.getTargetRegion(regionId)
  }

  def mkPageRegion(pageId: Int, bbox: Int*): PageRegion = {
    val List(x, y, w, h) = bbox.toList
    // val regionId = regionIdGen.nextId
    // PageRegion(PageID(pageId), getRegionBounds(x, y, w, h), Some(regionId))
    PageRegion(PageID(pageId), getRegionBounds(x, y, w, h), None)
  }

  def pageDivs3(pageId: Int): LabelWidget = {
    col(
      targetOverlay(mkPageRegion(pageId, 0, 0, 3, 1), List()),
      targetOverlay(mkPageRegion(pageId, 0, 1, 3, 1), List()),
      targetOverlay(mkPageRegion(pageId, 0, 2, 3, 1), List())
    )
  }

  def pageDivs2(pageId: Int): LabelWidget = {
    col(
      targetOverlay(mkPageRegion(pageId, 0, 0, 3, 2), List()),
      targetOverlay(mkPageRegion(pageId, 0, 2, 3, 1), List())
    )
  }

}
