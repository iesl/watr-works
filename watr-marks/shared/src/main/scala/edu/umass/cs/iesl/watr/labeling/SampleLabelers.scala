package edu.umass.cs.iesl.watr
package labeling

import textboxing.{TextBoxing => TB}, TB._
import geometry._
import LabelWidgets._
import utils.Colors
import utils.EnrichNumerics._

object TestLabelers {


  def dimensionTest(): LabelingPanel = {
    val sampleText1 = textbox(
      vjoin(center1)(
        "aliqua. Ut enim ad minim veniam, ",
        "incididunt ut labore et dolore magna ",
        "aliqua. Ut enim ad minim veniam, ",
        "Anyconsectetur adipisicing elit, sed do eiusmod tempor"
      )
    )

    val sampleText2 = textbox(
      vjoin(left)(
        "Lorem ipsum dolor sit amet",
        "Anyconsectetur adipisicing elit, sed do eiusmod tempor",
        "aliqua. Ut enim ad minim veniam, "
      )
    )


    val r0 = row(
      pad(sampleText1, Padding(3), Colors.LightBlue),
      pad(sampleText2, Padding(9), Colors.Azure)
    )

    val col0 = col(
      r0,
      pad(r0, Padding(2), Colors.Gray(3.percent))
    )
    val layout = pad(col0, Padding(10), Colors.Red)

    LabelingPanel(
      layout,
      LabelOptions(List())
    )

  }
}
