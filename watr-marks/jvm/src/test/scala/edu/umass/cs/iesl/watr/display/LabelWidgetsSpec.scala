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
    val pos0 = layoutWidgetPositions2(example)

    // println(prettyPrintPosition(pos0))


  }
}














  // def reposition(
  //   oldpositions: List[(LabelWidget, Position)],
  // //   offsetFn: (LTBounds) => PositionVector
  // // ): (LTBounds, List[Position]) = {
  // //   val newpositions = oldpositions
  // //     .foldLeft((zeroLTBounds, List[Position]()))({
  // //       case ((accBounds, accPositions), (widget, oldpos)) =>
  // //         val pvec = offsetFn(accBounds)
  // //         val newpos = oldpos.translate(pvec)
  // //         val newBounds = newpos.totalBounds union accBounds

  // //         (newBounds, newpos :: accPositions)
  // //     })

  // //   (newpositions._1, newpositions._2.reverse)
  // // }

  // def layoutWidgetPositions(lwidget: LabelWidget): Position = {
  //   // val idgen = utils.IdGenerator[RegionID]()

  //   // // Bottom-up evaluator
  //   // def positionAttrs: GAlgebra[(LabelWidget, ?), LabelWidgetF, Position] = fwa => {
  //   //   fwa match {
  //   //     case TargetOverlay(under, overs)  =>
  //   //       val positionVec = under.bbox.toPoint(CDir.NW)
  //   //       val bbox = under.bbox.moveToOrigin
  //   //       // val (totalBounds, repos) = reposition(overs, _.toPoint(CDir.NW))
  //   //       val (totalBounds, repos) = reposition(overs, {_ => -positionVec})

  //   //       Position(
  //   //         targetOverlay(under, overs.map(_._1)),
  //   //         positionVec, bbox, bbox,
  //   //         idgen.nextId,
  //   //         repos
  //   //       )

  //   //     case LabeledTarget(target, label, score)   =>
  //   //       // val positionVec = target.bbox.toPoint(CDir.NW)
  //   //       val bbox = target.bbox
  //   //       Position(labeledTarget(target, label, score),
  //   //         zeroPosVector, bbox, bbox,
  //   //         idgen.nextId,
  //   //         List()
  //   //       )

  //   //     case Col(attrs) =>
  //   //       val (totalBounds, repos) = reposition(attrs, _.toPoint(CDir.SW))
  //   //       Position(col(attrs.map(_._1):_*),
  //   //         zeroPosVector, totalBounds, totalBounds,
  //   //         idgen.nextId,
  //   //         repos
  //   //       )

  //   //     case Row(attrs) =>
  //   //       val (totalBounds, repos) = reposition(attrs, _.toPoint(CDir.NE))

  //   //       Position(row(attrs.map(_._1):_*),
  //   //         zeroPosVector, totalBounds, totalBounds,
  //   //         idgen.nextId,
  //   //         repos
  //   //       )

  //   //     case Pad(p@(a, attr), padding) =>
  //   //       val tbbox = attr.totalBounds

  //   //       val newtbbox = LTBounds(
  //   //         tbbox.left, tbbox.top,
  //   //         tbbox.width + padding.left + padding.right,
  //   //         tbbox.height + padding.top + padding.bottom
  //   //       )

  //   //       val pvec = Point(padding.left, padding.top)

  //   //       val (newwbbox, repos) = reposition(List(p), b => pvec.translate(b.toPoint(CDir.NW)))

  //   //       Position(
  //   //         pad(a, padding),
  //   //         zeroPosVector, newtbbox, newtbbox,
  //   //         idgen.nextId,
  //   //         repos
  //   //       )

  //   //     case RangeSelection(range) =>
  //   //       ???

  //   //     case l @ Reflow(textReflow) =>
  //   //       val target = textReflow.targetRegion
  //   //       val positionVec = target.bbox.toPoint(CDir.NW)
  //   //       val bbox = target.bbox.moveToOrigin

  //   //       Position(
  //   //         reflow(textReflow),
  //   //         zeroPosVector, bbox, bbox,
  //   //         idgen.nextId,
  //   //         List()
  //   //       )

  //   //     case l @ TextBox(box) =>
  //   //       val str = box.toString
  //   //       val lines = str.split("\n")
  //   //       val height = lines.length
  //   //       val maxwidth = lines.map(_.length).max
  //   //       val bbox: LTBounds = LTBounds(0, 0, maxwidth*6d, height*16d)

  //   //       Position(
  //   //         textbox(box),
  //   //         zeroPosVector, bbox, bbox,
  //   //         idgen.nextId,
  //   //         List()
  //   //       )

  //   //     case Button(action) =>
  //   //       val width = (action.length+1) * 6d
  //   //       val height = 18d
  //   //       val bbox: LTBounds = LTBounds(0, 0, width, height)

  //   //       Position(
  //   //         button(action),
  //   //         zeroPosVector, bbox, bbox,
  //   //         idgen.nextId,
  //   //         List()
  //   //       )

  //   //     case l @ Panel(p@(content, attr)) =>
  //   //       val (bbox, repos) = reposition(List(p), _.toPoint(CDir.NW))

  //   //       Position(
  //   //         panel(content),
  //   //         zeroPosVector, bbox, bbox,
  //   //         idgen.nextId,
  //   //         repos
  //   //       )

  //   //     case l @ MouseOverlay(p@(bkplane, attr)) =>
  //   //       val (bbox, repos) = reposition(List(p), _.toPoint(CDir.NW))

  //   //       Position(
  //   //         mouseOverlay(bkplane),
  //   //         zeroPosVector, bbox, bbox,
  //   //         idgen.nextId,
  //   //         repos
  //   //       )
  //   //   }
  //   // }

  //   // val relativePositioned = lwidget
  //   //   .cata(attributePara(positionAttrs))
  //   //   .toPair._1

  //   ???
  // }

// import TB._
// val wname = t.lwidget.unFix.getClass.getName.split("\\$").last
// val node = tbox(s"${wname}: vec: ${t.pVector} w:${t.widgetBounds} / t:${t.totalBounds}")

// if (t.children.isEmpty) node else {
//   node atop indent(2)(
//     vcat(
//       t.children.map(prettyPrintPosition(_))
//     )
//   )
// }
