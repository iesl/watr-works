package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._
import corpora._

import LabelWidgets._
import LabelWidgetF._
import TypeTags._


abstract class LabelWidgetTestUtil extends FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {

  def add4pg_3x3SampleDoc(): String@@DocumentID = {
    /**
      *  Test document layout (4 pages, grid layout)
      *      0123456
      *    0 abc012|
      *    1 def345|
      *    2 ghi678|
      *    3 jklstu|
      *    4 mnovwx|
      *    5 pqryzz|
      **/

    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr",
      "stu\nvwx\nyzz"
    )

    val stableId = DocumentID("doc#0")
    addDocument(stableId, doc)
    stableId
  }



  // def generatePageRegions(divs: Int): Seq[PageRegion] = {
  //   val allRegions = for {
  //     stableId <- docStore.getDocuments
  //     docId    <- docStore.getDocument(stableId).toSeq
  //     pageId   <- docStore.getPages(docId)
  //   }  yield {
  //     val pageGeometry = docStore.getPageGeometry(pageId)
  //     for {
  //       yslice <- intervalSlices(DoubleInterval(pageGeometry.top, pageGeometry.height), divs).tails.filterNot(_.isEmpty).map(_.reduce(_ union _))
  //       xslice <- intervalSlices(DoubleInterval(pageGeometry.left, pageGeometry.width), divs).tails.filterNot(_.isEmpty).map(_.reduce(_ union _))
  //     } yield {
  //       val regionBbox = LTBounds(
  //         xslice.min, yslice.min,
  //         xslice.len, yslice.len
  //       )
  //       PageRegion(pageId, regionBbox)
  //     }
  //   }
  //   allRegions.flatten
  // }


  def pageDivs3(pageId: Int@@PageID): LabelWidget = {
    val pageRegion = getRegionBounds(0, 0, 3, 3)
    col(
      targetOverlay(pageId, pageRegion, getRegionBounds(0, 0, 3, 1).some, List()),
      targetOverlay(pageId, pageRegion, getRegionBounds(0, 1, 3, 1).some, List()),
      targetOverlay(pageId, pageRegion, getRegionBounds(0, 2, 3, 1).some, List())
    )
  }

  def pageDivs_1_2(pageId: Int@@PageID): LabelWidget = {
    val pageRegion = getRegionBounds(0, 0, 3, 3)
    col(
      targetOverlay(pageId, pageRegion, getRegionBounds(0, 0, 3, 1).some, List()),
      targetOverlay(pageId, pageRegion, getRegionBounds(0, 2, 3, 2).some, List())
    )
  }
  def pageDivs2(pageId: Int@@PageID): LabelWidget = {
    val pageRegion = getRegionBounds(0, 0, 3, 3)
    col(
      targetOverlay(pageId, pageRegion, getRegionBounds(0, 0, 3, 2).some, List()),
      targetOverlay(pageId, pageRegion, getRegionBounds(0, 2, 3, 1).some, List())
    )
  }

}
