package edu.umass.cs.iesl.watr
package labeling

// import geometry._
import geometry.syntax._
// import geometry.zones._

import utils.EnrichNumerics._
import LabelWidgets._

import watrmarks.{StandardLabels => LB}
import corpora._


class LabelWidgetInteractionSpec extends LabelWidgetTestUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "widget interactions: click,select, keypress, etc."

  val LWT = LabelWidgetTransforms

  trait CommonSetup extends CleanDocstore {
    val stableId = add4pg_3x3SampleDoc()
    val docId = docStore.getDocument(stableId).get

    val labelWidget = col(
      row(pageDivs3(page(1)), pageDivs2(page(2)))
    )

    val lwindex = LabelWidgetIndex.create(docStore, labelWidget)

  }

  it should "select a zone" in new CommonSetup {

    val queryBox = getRegionBounds(2, 1, 2, 2).scale(-3.percent)

    // val queryHits = lwindex.queryRegion(queryBox)
    // val constrainedHits = lwindex.applyConstraint(ByChar, queryHits)
    // reportQueryHits(queryBox, constrainedHits)

    lwindex.addLabel(queryBox, ByChar, LB.Authors)

    // println(visualizeDocument(stableId))
    // DebugLayout.debugPrint(
    //   lwindex.layout.strictBounds,
    //   lwindex.layout.bleedBounds,
    //   lwindex.layout.positioning,
    //   Some(queryBox)
    // )
    // lwindex.debugPrint(Some(queryBox))


    println(prettyPrintLabelWidget(labelWidget))

    val labelWidgetWithIndicators = LWT.addZoneIndicators(LB.Authors, labelWidget, NilLabelerIdentifier, docStore)

    // println(prettyPrintLabelWidget(labelWidgetWithIndicators))
    val lwdiff = labelWidgetDiff(labelWidget, labelWidgetWithIndicators)

    println(drawLabelWidgetDiff(lwdiff))
    val mods = labelWidgetDiffToMods(lwdiff)
    // mods.groupBy(_.id)
    // val str = mods.mkString("\n  ", "\n  ", "\n")
    // println(str)

    val lwindexWithIndicators = LabelWidgetIndex.create(docStore, labelWidgetWithIndicators)
    lwindexWithIndicators.debugPrint(Some(queryBox))

    val absPositioned = lwindexWithIndicators.layout.positioning
    val absPosMapx = absPositioned.groupBy(_.widget.wid)
    val absPosMap = absPositioned.map(a => (a.widget.wid, a)).toMap

    val updates = mods


    val str = updates.mkString("\n  ", "\n  ", "\n")
    println(str)

  }
}
