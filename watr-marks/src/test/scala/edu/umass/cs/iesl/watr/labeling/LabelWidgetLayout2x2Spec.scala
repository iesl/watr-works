
package edu.umass.cs.iesl.watr
package labeling

import corpora._
import LabelWidgets._

class LabelWidgetLayout2x2Spec extends LabelWidgetTestUtil {
  override def xscale = 2.0d
  override def yscale = 2.0d

  def createEmptyDocumentZoningApi(): DocumentZoningApi = new MemDocZoningApi

  behavior of "label widget layout"

  it should "layout rows of overlay regions" in new CleanDocstore  {
    add4pg_3x3SampleDoc()
    val layout = row(
      targetOverlay(mkTargetRegion(page(2), 1, 2, 2, 1) , List()),
      targetOverlay(mkTargetRegion(page(3), 2, 1, 1, 2) , List())
    )

    val expectedOutput = {
      """|222233
         |222233
         |░░░░33
         |░░░░33
         |""".stripMargin
    }

    assertExpectedLayout(layout, expectedOutput)

  }

  it should "layout rows" in new CleanDocstore  {
    add4pg_3x3SampleDoc()
    val layout = row(
      figure(getRegionBounds(1, 1, 1, 1)),
      figure(getRegionBounds(1, 1, 1, 1))
    )

    val expectedOutput = {
      """|░░░░░░░░
         |░░░░░░░░
         |░░αα░░ßß
         |░░αα░░ßß
         |""".stripMargin
    }

    assertExpectedLayout(layout, expectedOutput)

  }
  it should "layout columns" in new CleanDocstore  {
    add4pg_3x3SampleDoc()
    val layout = col(
      figure(getRegionBounds(1, 1, 1, 1)),
      figure(getRegionBounds(1, 1, 1, 1))
    )

    val expectedOutput = {
      """|░░░░
         |░░░░
         |░░αα
         |░░αα
         |░░░░
         |░░░░
         |░░ßß
         |░░ßß
         |""".stripMargin
    }

    assertExpectedLayout(layout, expectedOutput)

  }
}
