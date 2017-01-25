package edu.umass.cs.iesl.watr
package display


import scalaz._, Scalaz._
import matryoshka._
// import matryoshka.data._
import matryoshka.implicits._

trait LabelWidgetBasics {

  import utils.ScalazTreeImplicits._
  import textboxing.{TextBoxing => TB}
  import utils.{CompassDirection => CDir}
  import geometry._
  import geometry.syntax._
  import textreflow.data._

  import LabelWidgetF._
  import LabelWidgets._

  def prettyPrintLabelWidget(lwidget: LabelWidget): TB.Box = {
    lwidget.cata(toTree).drawBox
  }

  def absPositionLabelWidget(lwidget: LabelWidget): LabelWidget = {

    def position(lw: LabelWidget): (PositionVector, LTBounds) = lw.unFix match {
      case Positioned(a, pvec, area) => (pvec, area)
      case x => sys.error(s"found non-positioned LabelWidget ${x}")
    }


    val zeroLTBounds: LTBounds = LTBounds(0, 0, 0, 0)
    val zeroPosVector: PositionVector = Point(0, 0)

    def visit(t: LabelWidgetF[LabelWidget]): LabelWidget = t match {
      case l @ TargetImage(tr)  =>
        val positionVec = tr.bbox.toPoint(CDir.NW)
        val bbox = tr.bbox.moveToOrigin
        positioned(fixlw(l), positionVec, bbox)

      case l @ TargetSelection(bkplane, region)  =>
        val (pvec, area) = position(bkplane)
        val a = region.bbox.translate(-pvec)

        positioned(fixlw(l), pvec, a)

      case Col(attrs) =>
        var currBbox: LTBounds = zeroLTBounds

        val pos = attrs.map(t => t.unFix match {
          case Positioned(a, pvec, area) =>
            val newVec = pvec.translate(y=currBbox.bottom)
            val newArea = area.translate(newVec)

            currBbox = currBbox union newArea

            positioned(t, newVec, newArea)

          case x => sys.error(s"found non-positioned LabelWidget ${x}")
        })

        positioned(col(pos:_*), zeroPosVector, currBbox)

      case Row(attrs) =>
        var currBbox: LTBounds = zeroLTBounds

        val pos = attrs.map(t => t.unFix match {
          case Positioned(a, pvec, area) =>
            val newVec = pvec.translate(x=currBbox.right)
            val newArea = area.translate(newVec)

            currBbox = currBbox union newArea

            positioned(t, newVec, newArea)

          case x => sys.error(s"found non-positioned LabelWidget ${x}")
        })

        positioned(row(pos:_*), zeroPosVector, currBbox)

      case RangeSelection(range) =>
        ???

      case l @ Reflow(reflow) =>
        var currBbox: LTBounds = zeroLTBounds

        val reflowAreas = reflow.targetRegions
          .map({tr =>
            // val newVec = pvec.translate(x=currBbox.right)
            val trArea = tr.bbox.moveToOrigin().translate(x=currBbox.right)
            currBbox = currBbox union tr.bbox
            trArea
          })

        positioned(fixlw(l), zeroPosVector, currBbox)

      case Panel(content) =>
        ???
      case MouseOverlay(bkplane) =>
        ???

      case x => sys.error("echoLabeler: TODO")
    }

    lwidget.cata(visit)
  }
}
