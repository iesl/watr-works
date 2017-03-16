package edu.umass.cs.iesl.watr
package labeling

import geometry._
// import textreflow.data._
// import data._
// import utils.EnrichNumerics._
import LabelWidgets._

import watrmarks.{StandardLabels => LB}
// import TypeTags._
import corpora._

class LabelWidgetIndexingSpec extends LabelWidgetTestUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "LabelWidgetIndexing"

  behavior of "rectangular selection"


  it should "apply a label to selected regions" in new CleanDocstore {

    add4x3x3SampleDoc()

    val layout = col(
      row(pageDivs3(1), pageDivs2(2))
    )
    val lwindex = LabelWidgetIndex.create(docStore, layout)

    val bbox = LTBounds(0, 0, 19.9, 9.9)

    lwindex.addLabel(bbox, ByLine, LB.Title)

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
  //   //   targetOverlay(mkPageRegion(1, 0, 0, 1, 1), List()),
  //   //   targetOverlay(mkPageRegion(1, 0, 1, 1, 1), List())
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
