package edu.umass.cs.iesl.watr
package labeling

import geometry._
import LabelWidgets._

import watrmarks.{StandardLabels => LB}
import corpora._

import TypeTags._

class LabelWidgetInteractionSpec extends LabelWidgetTestUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "widget interactions: click,select, keypress, etc."

  val LWT = LabelWidgetTransforms

  it should "             " in new CleanDocstore {

    add4x3x3SampleDoc()

    debugPrintDocStore()


    val labelWidget = col(
      row(pageDivs3(1), pageDivs2(2))
    )

    val withIndicators = LWT.addZoneSelectors(LB.VisualLine, labelWidget, docStore)

    // println(prettyPrintLabelWidget(withIndicators))

    val lwIndex = LabelWidgetIndex.create(docStore, withIndicators)

    lwIndex.debugPrint()

    // val layout: Seq[WidgetPositioning] = lwIndex.layout.positioning

    val uiState = UIState(ByLine, None, List())
    // val gesture = Click(Point(0,0))
    val gesture = Click(Point(10.0d, 3d))

    // activeLabelWidgetIndex.map { lwIndex =>
    //   println(s"got UIRequest ${r}")

    val UIResponse(finalState, changes) = lwIndex.userInteraction(uiState, gesture)
    println(s"uiState = ${finalState}")



  }
}
