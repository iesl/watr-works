package edu.umass.cs.iesl.watr
package labeling


import geometry._
// import textreflow.data._
import data._
// import utils.EnrichNumerics._
import LabelWidgets._

import watrmarks.{StandardLabels => LB}
import TypeTags._
import corpora._

class LabelWidgetIndexingSpec extends LabelWidgetTestUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  initEmpty()

  /**

    Test document layout (4 papers, grid layout)

      0123456
    0 abc012|
    1 def345|
    2 ghi678|
    3 jklstu|
    4 mnovwx|
    5 pqryzz|

    */

  val docs = List(
    List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr",
      "stu\nvwx\nyzz"
    )
  )

  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }

  behavior of "LabelWidgetIndexing"

  behavior of "rectangular selection"

  val regionIdGen = utils.IdGenerator[RegionID]()

  def mkRegion(pageId: Int, bbox: Int*): PageRegion = {
    val List(x, y, w, h) = bbox.toList
    val regionId = regionIdGen.nextId
    PageRegion(PageID(pageId), getRegionBounds(x, y, w, h), Some(regionId))
  }

  def pageDivs3(pageId: Int): LabelWidget = {
    LW.col(
      targetOverlay(mkRegion(pageId, 0, 0, 3, 1), List()),
      targetOverlay(mkRegion(pageId, 0, 1, 3, 1), List()),
      targetOverlay(mkRegion(pageId, 0, 2, 3, 1), List())
    )
  }

  def pageDivs2(pageId: Int): LabelWidget = {
    col(
      targetOverlay(mkRegion(pageId, 0, 0, 3, 2), List()),
      targetOverlay(mkRegion(pageId, 0, 2, 3, 1), List())
    )
  }

  it should "apply a label to selected regions" in {
    regionIdGen.reset()


    val layout = col(
      row(pageDivs3(1), pageDivs2(2))
    )
    val lwindex = LabelWidgetIndex.create(docStore, layout)

    // val state = UIState(
    //   Constraint.ByChar,
    //   Option(LB.Title),
    //   Create
    // )
    // val req = UIRequest(
    //   state, SelectRegion(bbox)
    // )
    // val response = lwindex.runUIRequest(req)

    val bbox = getRegionBounds(0, 0, 2, 1)

    lwindex.addLabel(bbox, Constraint.ByChar, LB.Title)

  }

  // it should "demonstrate layout" in {
  //   regionIdGen.reset()
  //   val layout0 = col(
  //     row(pageDivs3(1), pageDivs2(2)),
  //     row(pageDivs3(3), pageDivs2(4))
  //   )

  //   // val layout1 = row(pageDivs3(1), pageDivs2(2))
  //   // val layout2 = pageDivs3(1)
  //   // val layout3 = pageDivs2(1)

  //   // val layout4 = col(
  //   //   targetOverlay(mkRegion(1, 0, 0, 1, 1), List()),
  //   //   targetOverlay(mkRegion(1, 0, 1, 1, 1), List())
  //   // )

  //   val layout = layout0

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)

  //   // lwindex.debugPrint()
  // }


  // it should "select region(s) by line" in {
  //   regionIdGen.reset()

  //   val layout = col(
  //     row(pageDivs3(1), pageDivs2(2)),
  //     row(pageDivs3(3), pageDivs2(4))
  //   )

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)

  //   val queryHits = lwindex.select(getRegionBounds(2, 2, 2, 2))

  //   queryHits.foreach { qhit =>
  //     qhit.iTextReflows.foreach { iReflow =>
  //       val clippedReflows = iReflow.textReflow.clipToBoundingRegion(qhit.pageSpaceBounds)
  //       val lineText = iReflow.textReflow.toText()
  //       println(s"hit line: ${lineText}")
  //       clippedReflows.foreach { case (cr, interval)  =>
  //         val clippedText = cr.toText()
  //         val ctr = cr.targetRegion()
  //         println(s" clipped: ${clippedText} interval ${interval}, region: $ctr")
  //       }
  //     }
  //   }

  //   // To create a new zone:
  //   //   add PageRegion to DB (can now be a TargetRegion)
  //   //   docStore.createZone();  zone.addTargetRegions(..)

  // }

  // it should "select region(s) by char" in {
  // }

  // it should "select (single) region by rect (unconstrained)" in {
  // }



}
