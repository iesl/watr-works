package edu.umass.cs.iesl.watr
package display

import org.scalatest._

import geometry._
import textreflow._
import textreflow.data._
import display.data._
import utils.EnrichNumerics._
import TypeTags._

// import watrmarks.{StandardLabels => LB}
class LabelWidgetsSpec extends FlatSpec with Matchers with PlainTextReflow with LabelWidgetLayout {

  behavior of "label widgets"

  val bbox = LTBounds(20d, 12d, 10d, 10d)
  val tr = TargetRegion(RegionID(0), DocumentID("doc-id-0"), PageID(23), bbox)

  def stringToReflow(s: String): TextReflow =
    stringToTextReflow(s)(DocumentID("doc-id-0"), PageID(23))

  def reg0: TargetRegion = tr
  def sel0: TargetRegion = tr
  def sel1: TargetRegion = tr

  // it should "specify widget layout" in {

  //   val reflow0 = stringToReflow("lime _{^{ﬂ}a}vor")
  //   def range0: RangeInt = RangeInt(1, 3)

  //   val w1 = LW.targetOverlay(reg0, List(
  //     LW.labeledTarget(sel0),
  //     LW.labeledTarget(sel1)
  //   ))

  //   val w2 = LW.col(
  //     LW.reflow(reflow0),
  //     LW.reflow(reflow0)
  //   )

  //   val body = LW.row(w1, w2)

  //   val controls = LW.row(
  //     LW.button("Clear"),
  //     LW.button("Skip")
  //   )

  //   val panel1 =  LW.panel(
  //     LW.col(
  //       controls,
  //       body
  //     )
  //   )

  //   println("layout")

  //   val test = panel1 // LW.row(LW.col(LW.labeledTarget(sel0)))

  //   println(
  //     prettyPrintLabelWidget(test)
  //   )
  //   println("positioned")
  //   val abs0 = layoutWidgetPositions(test)
  //   println(
  //     prettyPrintPosition(abs0)
  //   )

  //   // // approve all selections within layout regions:
  //   // button(approveSelections(w2))
  //   // button(approveSelections(row1))
  // }



  def padding(n: Double): Padding = {
    Padding(n, n, n, n)
  }

  it should "properly position widgets" in {
    import LW._

    val reflow0 = stringToReflow("Meow, said the cat.")
    val example =  {

      // val col0 = col(
      //   pad(
      //     reflow(reflow0),
      //     Padding(left=1d, right=1d, top=1d, bottom=1d)
      //   )
      // )

      // row(
      //   col0, col0
      // )
      val r0 = row(
        pad(reflow(reflow0), padding(3)),
        reflow(reflow0)
        // pad(reflow(reflow0), padding(3)),
        // reflow(reflow0)
      )

      col(
        r0, r0
      )

    }

    println(prettyPrintLabelWidget(example))
    println("")
    val pos0 = layoutWidgetPositions(example)


    println(
      pos0.map(_.toString)
        .mkString("{\n  ", "\n  ", "\n}")
    )

  }
}
