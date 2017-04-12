package edu.umass.cs.iesl.watr
package labeling

// import geometry._
// import geometry.zones._

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

    val stableId = DocumentID(s"doc#0")
    val docId = docStore.getDocument(stableId).get

    // create a zone that has regions from multiple pages
    val page0Lines = docStore.getPageVisualLines(stableId, PageNum(0))// .flatMap(_.regions)
    val page1Lines = docStore.getPageVisualLines(stableId, PageNum(1))// .flatMap(_.regions)

    // val newZone = docStore.createZoneTree(zoneIds: Seq[<refinement>[Int, ZoneID]])(docId)
    val lines = page0Lines ++ page1Lines

    // val p0Ztree = page0Lines.map(r => docStore.createZoneTree(r))

    // val p0Block = docStore.addZoneTreeLabel(p0Ztree, LB.TextBlock)
    // docStore
    // val block1 = ZoneTrees.node(
    //   LB.TextBlock,
    //   page0Lines.map(ZoneTrees.leaf(_))
    // )
    // val block2 = ZoneTrees.node(
    //   LB.TextBlock,
    //   page1Lines.map(ZoneTrees.leaf(_))
    // )

    // val para0 = ZoneTree.node(LB.Para, Seq(block1, block2))


    // println(s"page0Lines: ${page0Lines}")

    // docStore.setZoneTargetRegions(newZone, page0Lines ++ page1Lines)
    // docStore.addZoneLabel(newZone, LB.Authors)

    // visualizeDocStore()
    println(
      visualizeDocument(stableId)
    )

    val labelWidget = col(
      row(pageDivs3(PageID(1)), pageDivs2(PageID(2)))
    )

    val withIndicators = LWT.addZoneIndicators(LB.Authors, labelWidget, docStore)
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
