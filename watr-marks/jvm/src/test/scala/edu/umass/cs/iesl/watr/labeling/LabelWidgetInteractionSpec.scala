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

  it should "select a zone" in new CleanDocstore {

    add4pg_3x3SampleDoc()
    // TODO visualize document
    // TODO visualize docStore

    val stableId = DocumentID(s"doc#0")
    val docId = docStore.getDocument(stableId).get

    // create a zone that has regions from multiple pages
    val newZone = docStore.createZone(docId)
    val page0Lines = docStore.getPageVisualLines(stableId, PageNum(0)).flatMap(_.regions)
    val page1Lines = docStore.getPageVisualLines(stableId, PageNum(1)).flatMap(_.regions)

    // println(s"page0Lines: ${page0Lines}")

    docStore.setZoneTargetRegions(newZone, page0Lines ++ page1Lines)
    docStore.addZoneLabel(newZone, LB.Authors)

    // visualizeDocStore()
    println(
      visualizeDocument(stableId)
    )

    val labelWidget = col(
      row(pageDivs3(1), pageDivs2(2))
    )

    val withIndicators = LWT.addZoneSelectors(LB.Authors, labelWidget, docStore)
    println(prettyPrintLabelWidget(withIndicators))

    val lwIndex = LabelWidgetIndex.create(docStore, withIndicators)
    // // lwIndex.debugPrint()

    // val uiState = UIState(ByLine, None, List())

    // {
    //   val (UIResponse(finalState, changes), finlw) = lwIndex.userInteraction(uiState, Click(Point(0, 0)))
    //   println(prettyPrintLabelWidget(finlw))
    //   assertResult(finalState.selections) { List(ZoneID(1)) }
    // }
    // {
    //   val (UIResponse(finalState, changes), finlw) = lwIndex.userInteraction(uiState, Click(Point(10, 12)))
    //   assertResult(finalState.selections) { List(ZoneID(2)) }
    // }


  }
}
