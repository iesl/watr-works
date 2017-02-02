package edu.umass.cs.iesl.watr
package display

// import scalaz._, Scalaz._
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

// import textreflow._
import textreflow.data._
import utils.{CompassDirection => CDir}
import geometry._
import geometry.syntax._
import LabelWidgetF._
import LabelWidgets._

import textboxing.{TextBoxing => TB}

case class Position(
  lwidget: LabelWidget,
  pVector: PositionVector,
  widgetBounds: LTBounds,
  totalBounds: LTBounds,
  id: Int@@RegionID,
  children: List[Position] // =List()
) {
  def translate(pvec: PositionVector): Position = {
    val newvec = pVector.translate(pvec)
    Position(
      lwidget,
      newvec,
      widgetBounds.translate(newvec),
      totalBounds.translate(newvec),
      id,
      children
    )
  }
}


trait LabelWidgetLayout extends LabelWidgetBasics {

  def prettyPrintPosition(t: Position): TB.Box = {
    import TB._
    val wname = t.lwidget.unFix.getClass.getName.split("\\$").last
    val node = tbox(s"${wname}: vec: ${t.pVector} w:${t.widgetBounds} / t:${t.totalBounds}")

    if (t.children.isEmpty) node else {
      node atop indent(2)(
        vcat(
          t.children.map(prettyPrintPosition(_))
        )
      )
    }
  }


  val zeroLTBounds: LTBounds = LTBounds(0, 0, 0, 0)
  val zeroPosVector: PositionVector = Point(0, 0)

  def reposition(
    oldpositions: List[(LabelWidget, Position)],
    offsetFn: (LTBounds) => PositionVector
  ): (LTBounds, List[Position]) = {
    val newpositions = oldpositions
      .foldLeft((zeroLTBounds, List[Position]()))({
        case ((accBounds, accPositions), (widget, oldpos)) =>
          val pvec = offsetFn(accBounds)
          val newpos = oldpos.translate(pvec)
          val newBounds = newpos.totalBounds union accBounds

          (newBounds, newpos :: accPositions)
      })

    (newpositions._1, newpositions._2.reverse)
  }

  def layoutWidgetPositions(lwidget: LabelWidget): Position = {
    val idgen = utils.IdGenerator[RegionID]()

    // Bottom-up evaluator
    def positionAttrs: GAlgebra[(LabelWidget, ?), LabelWidgetF, Position] = fwa => {
      fwa match {
        case TargetOverlay(under, overs)  =>
          val positionVec = under.bbox.toPoint(CDir.NW)
          val bbox = under.bbox.moveToOrigin
          // val (totalBounds, repos) = reposition(overs, _.toPoint(CDir.NW))
          val (totalBounds, repos) = reposition(overs, {_ => -positionVec})

          Position(
            targetOverlay(under, overs.map(_._1)),
            positionVec, bbox, bbox,
            idgen.nextId,
            repos
          )

        case LabeledTarget(target, label, score)   =>
          // val positionVec = target.bbox.toPoint(CDir.NW)
          val bbox = target.bbox
          Position(labeledTarget(target, label, score),
            zeroPosVector, bbox, bbox,
            idgen.nextId,
            List()
          )

        case Col(attrs) =>
          val (totalBounds, repos) = reposition(attrs, _.toPoint(CDir.SW))
          Position(col(attrs.map(_._1):_*),
            zeroPosVector, totalBounds, totalBounds,
            idgen.nextId,
            repos
          )

        case Row(attrs) =>
          val (totalBounds, repos) = reposition(attrs, _.toPoint(CDir.NE))

          Position(row(attrs.map(_._1):_*),
            zeroPosVector, totalBounds, totalBounds,
            idgen.nextId,
            repos
          )

        case Pad(p@(a, attr), padding) =>
          val tbbox = attr.totalBounds

          val newtbbox = LTBounds(
            tbbox.left, tbbox.top,
            tbbox.width + padding.left + padding.right,
            tbbox.height + padding.top + padding.bottom
          )

          val pvec = Point(padding.left, padding.top)

          val (newwbbox, repos) = reposition(List(p), b => pvec.translate(b.toPoint(CDir.NW)))

          Position(
            pad(a, padding),
            zeroPosVector, newtbbox, newtbbox,
            idgen.nextId,
            repos
          )

        case RangeSelection(range) =>
          ???

        case l @ Reflow(textReflow) =>
          val target = textReflow.targetRegion
          val positionVec = target.bbox.toPoint(CDir.NW)
          val bbox = target.bbox.moveToOrigin

          Position(
            reflow(textReflow),
            zeroPosVector, bbox, bbox,
            idgen.nextId,
            List()
          )

        case l @ TextBox(box) =>
          val str = box.toString
          val lines = str.split("\n")
          val height = lines.length
          val maxwidth = lines.map(_.length).max
          val bbox: LTBounds = LTBounds(0, 0, maxwidth*6d, height*16d)

          Position(
            textbox(box),
            zeroPosVector, bbox, bbox,
            idgen.nextId,
            List()
          )

        case Button(action) =>
          val width = (action.length+1) * 6d
          val height = 18d
          val bbox: LTBounds = LTBounds(0, 0, width, height)

          Position(
            button(action),
            zeroPosVector, bbox, bbox,
            idgen.nextId,
            List()
          )

        case l @ Panel(p@(content, attr)) =>
          val (bbox, repos) = reposition(List(p), _.toPoint(CDir.NW))

          Position(
            panel(content),
            zeroPosVector, bbox, bbox,
            idgen.nextId,
            repos
          )

        case l @ MouseOverlay(p@(bkplane, attr)) =>
          val (bbox, repos) = reposition(List(p), _.toPoint(CDir.NW))

          Position(
            mouseOverlay(bkplane),
            zeroPosVector, bbox, bbox,
            idgen.nextId,
            repos
          )
      }
    }

    val relativePositioned = lwidget
      .cata(attributePara(positionAttrs))
      .toPair._1

    def absolutePosition(pos: Position, vec: PositionVector): Position = {
      pos.translate(vec)

      pos.lwidget.project match {
        case l @ Row(as) =>
        case l @ Col(as) =>

      }
      pos.children.map(absolutePosition)
    }

}
