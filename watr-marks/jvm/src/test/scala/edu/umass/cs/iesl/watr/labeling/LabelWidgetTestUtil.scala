package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._

import geometry._

import corpora._
import utils.EnrichNumerics._

import LabelWidgets._
import LabelWidgetF._
import TypeTags._

import textboxing.{TextBoxing => TB}, TB._

abstract class LabelWidgetTestUtil extends FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {

  def add4pg_3x3SampleDoc(): Unit = {
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

  def visualizeDocStore(): Unit = {
    for {
      stableId     <- docStore.getDocuments
      docId        <- docStore.getDocument(stableId).toSeq
      _            <- putStrLn(s"Document $stableId id:${docId}")
      pageId       <- docStore.getPages(docId)
      pageGeometry  = docStore.getPageGeometry(pageId)
      _            <- putStrLn(s"  Page  ${pageId}: ${pageGeometry}")
    }  {
    }
  }

  def visualizeDocument(stableId: String@@DocumentID): TB.Box = {
    val docBoxes = for {
      docId <- docStore.getDocument(stableId).toSeq
    } yield {
      val pagesBox = for {
        pageId <- docStore.getPages(docId)
      } yield {
        val pageGeometry = docStore.getPageGeometry(pageId)

        val allTargetRegions = docStore.getTargetRegions(pageId)

        val regions = allTargetRegions
          .map(r => docStore.getTargetRegion(r).toString().box)
        val pageRegions = indent(4)(vcat(regions))

        val regionCount =  s"TargetRegions for page ${pageId}: ${allTargetRegions.length} ".box

        (
          indent(2)("PageGeometry")
            % indent(4)(pageGeometry.toString.box)
            % indent(2)(regionCount)
            % indent(2)(pageRegions)
            % indent(2)("Page Zones")
        )
      }

      val zoneBoxes = for {
        zoneId <- docStore.getZonesForDocument(docId)
      } yield {
        val text = docStore.getTextReflowForZone(zoneId).map(
          _.toText.box
        ).getOrElse("<no text>".box)
          (docStore.getZone(zoneId).toString().box %
            indent(2)(text))
      }

      (s"Document ${docId} (${stableId}) report"
        % indent(4)(vcat(pagesBox))
        % indent(2)("Zones")
        % indent(4)(vcat(zoneBoxes))
      )
    }
    vcat(docBoxes)
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
