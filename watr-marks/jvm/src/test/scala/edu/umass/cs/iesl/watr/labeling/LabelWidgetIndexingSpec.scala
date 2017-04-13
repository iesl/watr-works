package edu.umass.cs.iesl.watr
package labeling

import geometry._
// import textreflow._
// import textreflow.data._
// import data._
import utils.EnrichNumerics._

import LabelWidgets._

import watrmarks.{StandardLabels => LB}
import TypeTags._
import corpora._

class LabelWidgetIndexingSpec extends LabelWidgetTestUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  val page1 = PageID(1)
  val page2 = PageID(2)
  val page3 = PageID(3)
  val page4 = PageID(4)

  behavior of "LabelWidgetIndexing"

  behavior of "rectangular selection"

  def shrinkBbox(bbox: LTBounds, byX: Double@@Percent): LTBounds = {
    val scale = byX.unwrap/100d
    val w = bbox.width - (bbox.width*scale)
    val h = bbox.height - (bbox.height*scale)
    bbox.copy(width=w, height=h)
  }

  it should "apply a label to selected regions" in new CleanDocstore {
    val stableId = add4pg_3x3SampleDoc()

    val layout = col(
      row(pageDivs3(PageID(1)), pageDivs2(PageID(2)))
    )
    val lwindex = LabelWidgetIndex.create(docStore, layout)

    val queryBbox = getRegionBounds(0, 0, 1, 2)
    val bbox = shrinkBbox(queryBbox, 3.percent)

    println(
      visualizeDocument(stableId)
    )

    val qres = lwindex.queryRegion(bbox)
    println(s"querying: $bbox")

    val qstr = qres.map{qhit =>
      val wid = qhit.positioned.widget.wid
      val pid = qhit.pageId
      val hbbox = qhit.pageSpaceBounds

      s"""widget: $wid page: $pid pbounds: $hbbox""" +
      qhit.iTextReflows
        .map{_.textReflow.toText()}
        .mkString("{\n  ", "\n  ", "\n  }")
    }.mkString("[\n  ", "\n  ", "\n]")

    println(qstr)

    lwindex.addLabel(queryBbox, ByLine, LB.Title)

    // lwindex.debugPrint()
    println(
      visualizeDocument(stableId)
    )
  }

  // it should "demonstrate layout" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()

  //   val layout0 = col(
  //     row(pageDivs3(PageID(1)), pageDivs2(PageID(2))),
  //     row(pageDivs3(PageID(3)), pageDivs2(PageID(4)))
  //   )

  //   // val layout1 = row(pageDivs3(1), pageDivs2(2))
  //   // val layout2 = pageDivs3(1)
  //   val layout3 = pageDivs2(PageID(1))

  //   // val layout4 = col(
  //   //   targetOverlay(mkPageRegion(1, 0, 0, 1, 1), List()),
  //   //   targetOverlay(mkPageRegion(1, 0, 1, 1, 1), List())
  //   // )

  //   val layout = layout3

  //   println(prettyPrintLabelWidget(layout))

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)

  //   // lwindex.debugPrint()
  // }


  // it should "select region(s) by line" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()

  //   val layout = col(
  //     row(pageDivs3(page1), pageDivs2(page2)),
  //     row(pageDivs3(page3), pageDivs2(page4))
  //   )

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)

  //   val queryHits = lwindex.queryRegion(getRegionBounds(2, 2, 2, 2))

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
