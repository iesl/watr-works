package edu.umass.cs.iesl.watr
package labeling


import geometry._
// import textreflow.data._
import data._
// import utils.EnrichNumerics._
import LabelWidgets._

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

   =  0123467890
     _________
   0 |abc 012|
   1 |def 345|
   2 |ghi 678|
   3 |       |
   4 |jkl stu|
   5 |mno vwx|
   6 |pqr yzz|
     ^^^^^^^^^

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

  def mkRegion(pageId: Int, bbox: Int*): PageRegion = {
    val List(x, y, w, h) = bbox.toList
    PageRegion(PageID(pageId), getRegionBounds(x, y, w, h))
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

  it should "select region(s) by line" in {
    val layout = col(
      row(pageDivs3(1), pageDivs2(2)),
      row(pageDivs3(3), pageDivs2(4))
    )

    val lwindex = LabelWidgetIndex.create(docStore, layout)

    val pageRegions = lwindex.select(getRegionBounds(2, 2, 1, 1), Constraint.ByLine)

    // List of PageRegions per overlay PageRegion
    // val selection: List[List[PageRegion]] = lwindex.select(bbox, ByLine)

    // To create a new zone:
    //   add PageRegion to DB (can now be a TargetRegion)
    //   docStore.createZone();  zone.addTargetRegions(..)

  }

  it should "select region(s) by char" in {
  }

  it should "select (single) region by rect (unconstrained)" in {
  }



}
