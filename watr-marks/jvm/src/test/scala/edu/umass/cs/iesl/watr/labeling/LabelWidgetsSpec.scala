package edu.umass.cs.iesl.watr
package labeling

// import geometry._
// import textreflow.data._
// import labeling.data._

import TypeTags._
import corpora._
import LabelWidgets._
import LabelWidgetLayoutHelpers._
// import watrmarks.{StandardLabels => LB}
import scalaz._, Scalaz._

class LabelWidgetsSpec extends LabelWidgetTestUtil { // FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "label widgets"

  /**

    I'm not totally sure how to write these tests... So far they are really just
    glorified REPL interactions in testing blocks, so that I can step-debug and println
    my way through the results to verify them


    - Ideas for testing:
      - A small visual notation that expresses the desired layout, is easy to visually understand and check against



    */

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._
  import matryoshka.patterns._

  it should "handle diffs" in new CleanDocstore {
    add4pg_3x3SampleDoc()

    val stableId = DocumentID(s"doc#0")
    val docId = docStore.getDocument(stableId).get

    // println(visualizeDocument(stableId))

    val labelWidget2 = col(
      row(pageDivs3(PageID(1)))
    )

    val labelWidget = col(
      row(pageDivs_1_2(PageID(1)))
    )

    val wer: Fix[Diff[Fix, LabelWidgetF, ?]] = labelWidget.paraMerga(labelWidget2)(diff)

    val treeStr = wer.cata(toTree).drawTree
    println(treeStr)

    /// Use of diffs for labeling:
    // - When a widget is clicked (or selected)
    //   - run the _.cata(..) via interaction to make changes to the label widget
    //   - do a diff of the resulting widget against the original
    //   - reduce the diff to a data structure suitable to send to the UI,
    //     which contains visual updates and client-state changes


    // On click, add selection indicator



  }

  // it should "include labeled targets as overlays" in new CleanDocstore {
  //   val stableId = add4pg_3x3SampleDoc()
  //   val docId = docStore.addDocument(stableId)
  //   val pageId = docStore.addPage(docId, PageNum(0))

  //   val pageRegion = mkTargetRegionDbl(pageId, 5d, 4d, 3d, 2d)
  //   val pageRegion2 = mkTargetRegionDbl(pageId, 5.1d, 4.1d, 3.1d, 2.1d)

  //   val subRegion = mkTargetRegionDbl(pageId, 5.5d, 4.5d, 0.5d, 1.5d)
  //   val subRegion2 = mkTargetRegionDbl(pageId, 5.6d, 3.5d, 0.1d, 2.5d)

  //   val widget0 = col(
  //     targetOverlay(pageRegion, List(labeledTarget(subRegion), labeledTarget(subRegion2))),
  //     targetOverlay(pageRegion2, List(labeledTarget(subRegion), labeledTarget(subRegion2)))
  //   )


  //   val widgetLayout = layoutWidgetPositions(widget0)

  //   widgetLayout.positioning
  //     .foreach{ pos =>
  //       println(s"${pos}")
  //       val clipTo = LTBounds(1d, 0d, 1d, 100d)
  //       val clipped = clipPageRegionFromWidgetSpace(pos, clipTo)
  //       println(s"  clipped: ${clipped}")

  //     }

  // }


  // it should "correctly position overlays" in  {
  // }

  // it should "rewrite widgets to include geometric figure overlays for prior labeling" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()

  //   // Create a zone labeling over a few pages
  //   // val stableId = docStore.getDocuments().head
  //   // val docId = docStore.getDocument(stableId).get


  //   // // Create a zone that spans 2 pages
  //   // val page0Lines = docStore.getPageVisualLines(stableId, PageNum(0))
  //   // val page1Lines = docStore.getPageVisualLines(stableId, PageNum(1))
  //   // val lineZoneIds = (page0Lines ++ page1Lines).map(_.getId())
  //   // val newZone = docStore.createZone(lineZoneIds)
  //   // docStore.addZoneLabel(newZone, LB.Authors)



  //   // // Create a labeling widget
  //   // val layout = col(
  //   //   row(pageDivs3(1), pageDivs2(2))
  //   // )

  //   // val layout1 = LabelWidgetTransforms.addZoneIndicators(LB.Authors, layout, docStore)

  //   // val lwindex = LabelWidgetIndex.create(docStore, layout1)

  //   // // prove that a widget w/ either of those pages includes the labeled overlay
  //   // lwindex.layout.positioning.foreach { pos =>
  //   //   println(s"${pos}")
  //   // }
  // }


  // it should "create columns" in {
  // }

  // it should "create rows" in {
  // }

  // it should "include inserted text (as textbox)" in {
  // }

  // it should "include reflows" in {
  // }

  // it should "include padding" in {
  // }

}
