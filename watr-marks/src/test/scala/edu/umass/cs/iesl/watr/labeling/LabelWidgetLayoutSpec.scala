package edu.umass.cs.iesl.watr
package labeling

import geometry._

import TypeTags._
import corpora._
import LabelWidgets._

class LabelWidgetLayoutSpec extends LabelWidgetTestUtil {
  override def xscale = 10.0d
  override def yscale = 10.0d

  def createEmptyDocumentZoningApi(): DocumentZoningApi = new MemDocZoningApi


  /**
    Testing layout functionality:

    */


  behavior of "label widget layout"
  it should "correctly position target regions" in new CleanDocstore  {
    add4pg_3x3SampleDoc()


    val layout = row(
      targetOverlay(mkTargetRegion(page(2), 1, 2, 2, 1) , List()),
      targetOverlay(mkTargetRegion(page(3), 2, 1, 1, 2) , List())
    )

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
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |░░░░░░░░░░░░░░░░░░░░3333333333
         |""".stripMargin
    }

    assertExpectedLayout(layout, expectedOutput)
  }

  it should "correctly position target regions with figure overlays" in new CleanDocstore  {
    add4pg_3x3SampleDoc()

    val layout = row(
      targetOverlay(mkTargetRegion(page(2), 1, 2, 2, 1) , List( figure(getRegionBounds(1, 2, 1, 1) ))),
      targetOverlay(mkTargetRegion(page(3), 2, 1, 1, 2) , List( figure(getRegionBounds(2, 2, 1, 1) )))
    )

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
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß
         |""".stripMargin
    }

    assertExpectedLayout(layout, expectedOutput)
  }

  it should "include properly handle inherited layout (Panel, Labeled, Identified)" in new CleanDocstore {
    add4pg_3x3SampleDoc()

    val layout = row(
      targetOverlay(mkTargetRegion(page(2), 1, 2, 2, 1) , List(
        withId(ZoneID(1),
          figure(getRegionBounds(2, 2, 1, 1))
        )
      )),
      targetOverlay(mkTargetRegion(page(3), 2, 1, 1, 2) , List(
        panel(
          withLabel("key", "value",
            figure(getRegionBounds(2, 2, 1, 1))
          ) ,
          Interaction.InteractNil
        )
      ))
    )

    val expectedOutput = {
      """|2222222222αααααααα│α3333333333
         |2222222222αααααααα│α3333333333
         |2222222222αααααααα│α3333333333
         |2222222222αααααααα│α3333333333
         |2222222222αααααααα│α3333333333
         |2222222222αααααααα│α3333333333
         |2222222222αααααααα│α3333333333
         |2222222222αααααααα│α3333333333
         |2222222222────────iα3333333333
         |2222222222αααααααααα3333333333
         |░░░░░░░░░░░░░░░░░░░░ßßßßßßßß│ß
         |░░░░░░░░░░░░░░░░░░░░ßl┄┄┄┄┄┄┄┄
         |░░░░░░░░░░░░░░░░░░░░ß┊ßßßßßß│ß
         |░░░░░░░░░░░░░░░░░░░░ß┊ßßßßßß│ß
         |░░░░░░░░░░░░░░░░░░░░ß┊ßßßßßß│ß
         |░░░░░░░░░░░░░░░░░░░░ß┊ßßßßßß│ß
         |░░░░░░░░░░░░░░░░░░░░ß┊ßßßßßß│ß
         |░░░░░░░░░░░░░░░░░░░░ß┊ßßßßßß│ß
         |░░░░░░░░░░░░░░░░░░░░─┊──────pß
         |░░░░░░░░░░░░░░░░░░░░ß┊ßßßßßßßß
         |""".stripMargin
    }

    assertExpectedLayout(layout, expectedOutput)
  }



  it should "correctly position padded regions" in new CleanDocstore  {
    add4pg_3x3SampleDoc()

    val layout = row(
      pad(
        targetOverlay(mkTargetRegion(page(2), 1, 2, 2, 1) , List( figure(getRegionBounds(1, 2, 1, 1) ))),
        Padding.Ints(2)
      ),
      pad(
        targetOverlay(mkTargetRegion(page(3), 2, 1, 1, 2) , List( figure(getRegionBounds(2, 2, 1, 1) ))),
        Padding.Ints(3)
      )
    )

    val expectedOutput = {
      """|░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
         |░░αααααααααα2222222222░░░░░░░░░░░░░░░░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░αααααααααα2222222222░░░░░3333333333░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░3333333333░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░ßßßßßßßßßß░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
         |░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
         |""".stripMargin
    }

    assertExpectedLayout(layout, expectedOutput)
  }


  // it should "correctly report the width/height of the final layout" in new CleanDocstore { }
  // it should "create columns/rows" in new CleanDocstore { }
  // it should "create overlay zstacks" in new CleanDocstore { }




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
}
