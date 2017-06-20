package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._
import corpora._

import LabelWidgets._
import LabelWidgetF._
import TypeTags._

import geometry._

abstract class LabelWidgetTestUtil extends FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {

  def assertExpectedLayout(layout: LabelWidget, expectedLayout: String): Unit = {
    val lwindex = LabelWidgetIndex.create(docStore, layout)
    val graphPaper = lwindex.toGraphPaper()
    // val strictBounds = lwindex.layout.strictBounds
    // val bleedBounds = lwindex.layout.bleedBounds
    // println(s"strictBounds: ${strictBounds}")
    // println(s"bleedBounds: ${bleedBounds}")
    // println(prettyPrintLabelWidget(layout))
    // println(graphPaper.asString())
    // println(graphPaper.asMonocolorString())

    assertExpectedText(
      expectedLayout,
      graphPaper.asMonocolorString()
    )
  }

  def assertExpectedText(expected: String, actual: String): Unit = {
    val l1s = actual.split("\n").map(_.trim())
    val l2s = expected.split("\n").map(_.trim())
    val zipped = l1s.zip(l2s)
    zipped.foreach { case (l1, l2) =>
      assertResult(l1)(l2)
    }
  }

  def reportQueryHits(queryBox: LTBounds, qhits: Seq[QueryHit]): Unit = {
    println(s"querying: $queryBox")
    val qstr = qhits.map{qhit =>
      val wid = qhit.positioned.widget.wid
      val pid = qhit.pageId
      val pageQuery = qhit.pageQueryBounds

      s"""widget: $wid page: $pid page-space query: $pageQuery""" +
        qhit.iTextReflows
        .map{_.textReflow.toText()}
        .mkString("{\n  ", "\n  ", "\n  }")
    }.mkString("[\n  ", "\n  ", "\n]")

    println(qstr)

  }

  def page(n: Int) = PageID(n)

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


  def pageDiv1(pageId: Int@@PageID, overlays: List[LabelWidget]=List()): LabelWidget = {
    val targetRegion = mkTargetRegion(pageId, 0, 0, 3, 3)
    col(
      targetOverlay(targetRegion, overlays)
    )
  }

  def pageDivs3(pageId: Int@@PageID): LabelWidget = {
    col(
      targetOverlay(mkTargetRegion(pageId, 0, 0, 3, 1), List()),
      targetOverlay(mkTargetRegion(pageId, 0, 1, 3, 1), List()),
      targetOverlay(mkTargetRegion(pageId, 0, 2, 3, 1), List())
    )
  }
  def pageDivs3Stacked(pageId: Int@@PageID): LabelWidget = {
    zstack(
      targetOverlay(mkTargetRegion(pageId, 0, 0, 3, 1), List()),
      targetOverlay(mkTargetRegion(pageId, 0, 1, 3, 1), List()),
      targetOverlay(mkTargetRegion(pageId, 0, 2, 3, 1), List())
    )
  }

  def pageDivs_1_2(pageId: Int@@PageID): LabelWidget = {
    col(
      targetOverlay(mkTargetRegion(pageId, 0, 0, 3, 1), List()),
      targetOverlay(mkTargetRegion(pageId, 0, 2, 3, 2), List())
    )
  }
  def pageDivs2(pageId: Int@@PageID): LabelWidget = {
    // val targetRegion = mkTargetRegion(pageId, 0, 0, 3, 3)
    col(
      targetOverlay(mkTargetRegion(pageId, 0, 0, 3, 2), List()),
      targetOverlay(mkTargetRegion(pageId, 0, 2, 3, 1), List())
    )
  }

}
