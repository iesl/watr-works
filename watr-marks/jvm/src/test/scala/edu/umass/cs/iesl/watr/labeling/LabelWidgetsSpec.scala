package edu.umass.cs.iesl.watr
package labeling

import geometry._

import TypeTags._
import corpora._
import LabelWidgets._
// import LabelWidgetF._
// import matryoshka._
// import matryoshka.implicits._

class LabelWidgetsSpec extends LabelWidgetTestUtil { // FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {
  def createEmptyDocumentZoningApi(): DocumentZoningApi = new MemDocZoningApi

  behavior of "label widgets"

  /**

    I'm not totally sure how to write these tests... So far they are really just
    glorified REPL interactions in testing blocks, so that I can step-debug and println
    my way through the results to verify them


    Possible ideas for testing:
      - A small visual notation that expresses the desired layout, is easy to visually understand and check against

    */


  val page1 = PageID(1)
  val page2 = PageID(2)
  val page3 = PageID(3)
  val page4 = PageID(4)

  def assertExpectedText(expected: String, actual: String): Unit = {
    val l1s = actual.split("\n").map(_.trim())
    val l2s = expected.split("\n").map(_.trim())
    val zipped = l1s.zip(l2s)
    zipped.foreach { case (l1, l2) =>
      // println(s"1>$l1")
      // println(s"2>$l2")
      assertResult(l1)(l2)
    }
  }

  behavior of "figures"

  // it should "adjust figure positions" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()
  //   // val bbox2 = LTBounds.Ints(1, 1, 8, 8)
  //   // val layout =
  //   //   row(
  //   //     figure(bbox),
  //   //     figure(bbox2)
  //   //   )

  //   val bbox = LTBounds.Ints(1, 1, 22, 8)
  //   val layout =
  //     col(
  //       row(

  //         targetOverlays(
  //           mkTargetRegion(page1, 0, 0, 3, 3),
  //           figure(bbox)
  //         ),
  //         targetOverlays(
  //           mkTargetRegion(page1, 0, 0, 3, 3),
  //           figure(LTBounds.Ints(0, 10, 10, 15))
  //         )
  //       ),
  //       row(
  //         pad(
  //           targetOverlays(
  //             mkTargetRegion(page1, 0, 0, 3, 3),
  //             figure(bbox)
  //           ),
  //           Padding.Ints(5)
  //         ),
  //         targetOverlays(
  //           mkTargetRegion(page1, 0, 0, 3, 3),
  //           figure(LTBounds.Ints(0, 10, 10, 15))
  //         )
  //       )
  //     )

  //   // val layout =
  //   //   pad(
  //   //     figure(bbox),
  //   //     Padding.Ints(2)
  //   //   )

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)

  //   // println(prettyPrintLabelWidget(layout))
  //   // lwindex.debugPrint()
  //   // lwindex.layout.positioning.foreach { pos =>
  //   //   val sb = pos.strictBounds
  //   //   val f = pos.widget
  //   //   println(s"${sb}: $f")
  //   // }
  // }

  it should "correctly position target regions" in new CleanDocstore  {
    add4pg_3x3SampleDoc()

    val layout = row(
      targetOverlay(mkTargetRegion(page2, 1, 2, 2, 1) , List()),
      targetOverlay(mkTargetRegion(page3, 2, 1, 1, 2) , List())
    )

    val lwindex = LabelWidgetIndex.create(docStore, layout)

    val expectedOutput = {
      """|222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |222222222222222222223333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |                    3333333333
         |""".stripMargin
    }

    val graphPaper = lwindex.toGraphPaper()

    assertExpectedText(
      expectedOutput,
      graphPaper.asMonocolorString()
    )
  }


  it should "correctly position target regions with figure overlays" in new CleanDocstore  {
    add4pg_3x3SampleDoc()

    val layout = row(
      targetOverlay(mkTargetRegion(page2, 1, 2, 2, 1) , List( figure(getRegionBounds(1, 2, 1, 1) ))),
      targetOverlay(mkTargetRegion(page3, 2, 1, 1, 2) , List( figure(getRegionBounds(2, 2, 1, 1) )))
    )

    val lwindex = LabelWidgetIndex.create(docStore, layout)
    val expectedOutput = {
      """|αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |αααααααααα22222222223333333333
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |                    ßßßßßßßßßß
         |""".stripMargin
    }

    val graphPaper = lwindex.toGraphPaper()
    val actual = graphPaper.asMonocolorString()
    // println(actual)
    assertExpectedText(expectedOutput, actual)
  }

  it should "correctly position padded regions" in new CleanDocstore  {
    add4pg_3x3SampleDoc()

    val layout = row(
      pad(
        targetOverlay(mkTargetRegion(page2, 1, 2, 2, 1) , List( figure(getRegionBounds(1, 2, 1, 1) ))),
        Padding.Ints(2)
      ),
      pad(
        targetOverlay(mkTargetRegion(page3, 2, 1, 1, 2) , List( figure(getRegionBounds(2, 2, 1, 1) ))),
        Padding.Ints(3)
      )
    )

    val lwindex = LabelWidgetIndex.create(docStore, layout)
    val expectedOutput = {
      """|
         |
         |  αααααααααα2222222222
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |  αααααααααα2222222222       3333333333
         |                             3333333333
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |                             ßßßßßßßßßß
         |
         |
         |
         |""".stripMargin
    }

    val graphPaper = lwindex.toGraphPaper()
    val actual = graphPaper.asMonocolorString()
    assertExpectedText(expectedOutput, actual)
  }


  // it should "adjust query bounds to correct page bounds for each target region" in new CleanDocstore  {
  // }



  // it should "create columns/rows" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()

  //   val layout = col(
  //     row(pageDivs3(PageID(1)), pageDivs2(PageID(2))),
  //     row(pageDivs2(PageID(3)), pageDivs3(PageID(4)))
  //   )
  //   // println(prettyPrintLabelWidget(layout))

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)

  //   val finalLayout = (
  //     """|111222
  //        |111
  //        |111222
  //        |333444
  //        |   444
  //        |333444
  //        |""")

  //   // lwindex.debugPrint()
  //   // lwindex.layout.positioning
  // }

  // it should "create overlay zstacks" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()
  //   val layout = row(
  //     row(pageDivs3(PageID(1)), pageDivs2(PageID(2))),
  //     pageDivs3Stacked(PageID(3))
  //   )

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)
  //   // lwindex.debugPrint()
  //   // lwindex.layout.positioning.foreach { absPos =>
  //   //   println(s"${absPos}")
  //   // }
  // }


  // it should "compute a diff between 2 label widgets" in new CleanDocstore {
  //   val stableId = add4pg_3x3SampleDoc()
  //   val docId = docStore.getDocument(stableId).get

  //   // println(visualizeDocument(stableId))

  //   val overlayBox = figure(getRegionBounds(1, 1, 1, 2))
  //   val overlayBox2 = figure(getRegionBounds(0, 0, 1, 2))
  //   val overlayBox3 = figure(getRegionBounds(0, 0, 1, 3))

  //   val labelWidget1 = col(
  //     row(pageDiv1(page1, List(overlayBox, overlayBox3)))
  //   )

  //   def visit(lw: LabelWidgetT): LabelWidgetT = lw match {
  //     case l @ RegionOverlay(wid, under, overlays) =>
  //       l.copy(
  //         overlays = List(overlays(0), overlayBox2, overlays(1))
  //       )
  //     case _ => lw
  //   }

  //   val labelWidget2 = labelWidget1.transCata[LabelWidget](visit)

  //   val lwindex1 = LabelWidgetIndex.create(docStore, labelWidget1)
  //   val lwindex2 = LabelWidgetIndex.create(docStore, labelWidget2)

  //   // lwindex2.debugPrint()

  //   val lwDiff = labelWidgetDiff(labelWidget1, labelWidget2)

  //   println(drawLabelWidgetDiff(lwDiff))

  //   val allMods = labelWidgetDiffToMods(lwDiff)

  //   val str = allMods.mkString("\n  ", "\n  ", "\n")
  //   // println(str)

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
  //   // val layout = col(246]

  //   //   row(pageDivs3(1), pageDivs2(2))
  //   // )

  //   // val layout1 = LabelWidgetTransforms.addZoneIndicators(LB.Authors, layout, docStore)

  //   // val lwindex = LabelWidgetIndex.create(docStore, layout1)

  //   // // prove that a widget w/ either of those pages includes the labeled overlay
  //   // lwindex.layout.positioning.foreach { pos =>
  //   //   println(s"${pos}")
  //   // }
  // }

  // it should "include padding" in new CleanDocstore {
  // it should "include inserted text (as textbox)" in new CleanDocstore {
  // it should "include reflows" in new CleanDocstore {
  // it should "include labeled targets as overlays" in new CleanDocstore {

}
