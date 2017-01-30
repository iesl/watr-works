package edu.umass.cs.iesl.watr
package display


import scalaz._, Scalaz._
import matryoshka._
import matryoshka.data._
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
    val idgen = utils.IdGenerator[RegionID]()

    def position(lw: LabelWidget): (PositionVector, LTBounds, LTBounds) = lw.unFix match {
      case Positioned(a, pvec, wbbox, tbbox, id) => (pvec, wbbox, tbbox)
      case x => sys.error(s"found non-positioned LabelWidget ${x}")
    }

    val zeroLTBounds: LTBounds = LTBounds(0, 0, 0, 0)
    val zeroPosVector: PositionVector = Point(0, 0)

    def visit(lwidget0: LabelWidgetF[(LabelWidget, LabelWidget)]): LabelWidget = lwidget0 match {

      case TargetOverlay(under, overs)  =>
        val positionVec = under.bbox.toPoint(CDir.NW)
        val bbox = under.bbox.moveToOrigin

        val repositionedOvers = overs.map({case (over, overpos) => overpos.unFix match {
          case Positioned(a, pvec, wbbox, tbbox, id) =>
            val newVec = -positionVec // pvec.translate(-positionVec)
            val newWArea = wbbox.translate(newVec)
            val newTbbox = tbbox.translate(newVec)

            positioned(a, newVec, newWArea, newTbbox, id)

          case x => sys.error(s"found non-positioned LabelWidget ${x}")
        }})

        positioned(targetOverlay(under, repositionedOvers), zeroPosVector, bbox, bbox, idgen.nextId)

      case LabeledTarget(target, label, score)  =>
        val positionVec = target.bbox.toPoint(CDir.NW)
        val bbox = target.bbox // .moveToOrigin

        positioned(labeledTarget(target, label, score), zeroPosVector, bbox, bbox, idgen.nextId)


      case Col(attrs) =>
        var currBbox: LTBounds = zeroLTBounds

        val pos = attrs.map({case (t, attr) => attr.unFix match {
          case Positioned(a, pvec, wbbox, tbbox, id) =>
            val newVec = pvec.translate(y=currBbox.bottom)
            val newWArea = wbbox.translate(newVec)
            val newTbbox = tbbox.translate(newVec)

            currBbox = currBbox union newTbbox

            positioned(a, newVec, newWArea, newWArea, id)

          case x => sys.error(s"found non-positioned LabelWidget ${x}")
        }})

        positioned(col(pos:_*), zeroPosVector, currBbox, currBbox, idgen.nextId)

      case Row(attrs) =>
        var currBbox: LTBounds = zeroLTBounds

        val pos = attrs.map({case (t, attr) => attr.unFix match {
          case Positioned(a, pvec, wbbox, tbbox, id) =>
            val newVec = pvec.translate(x=currBbox.right)
            val newWArea = wbbox.translate(newVec)
            val newTbbox = tbbox.translate(newVec)

            currBbox = currBbox union newTbbox

            positioned(a, newVec, newWArea, newWArea, id)

          case x => sys.error(s"found non-positioned LabelWidget ${x}")
        }})

        positioned(row(pos:_*), zeroPosVector, currBbox, currBbox, idgen.nextId)

      case RangeSelection(range) =>
        ???

      case l @ Reflow(treflow) =>
        var currBbox: LTBounds = LTBounds(0, 0, 1.0, 1.0)

        val reflowAreas = treflow.targetRegions
          .foreach({tr =>
            // val newVec = pvec.translate(x=currBbox.right)
            val trArea = tr.bbox.moveToOrigin().translate(x=currBbox.right)
            currBbox = currBbox union trArea
          })

        positioned(reflow(treflow), zeroPosVector, currBbox, currBbox, idgen.nextId)

      case l @ TextBox(box) =>
        val str = box.toString
        val lines = str.split("\n")
        val height = lines.length
        val maxwidth = lines.map(_.length).max
        val currBbox: LTBounds = LTBounds(0, 0, maxwidth*6d, height*16d)

        positioned(textbox(box), zeroPosVector, currBbox, currBbox, idgen.nextId)

      case l @ Panel((content, attr)) =>
        val (childpvec, childbbox, tbbox) = position(attr)

        positioned(panel(attr), childpvec, childbbox, tbbox, idgen.nextId)

      case l @ MouseOverlay((bkplane, attr)) =>
        val (childpvec, childbbox, tbbox) = position(attr)

        positioned(mouseOverlay(attr), childpvec, childbbox, tbbox, idgen.nextId)

      case x => sys.error(s"absPositionLabelWidget: case ${x}")
    }

    lwidget
      .cata(attributePara(visit))
      .toPair._1


  }
}
