package edu.umass.cs.iesl.watr
package labeling

import scalaz.{@@ => _, _}, Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

import textreflow.data._
import utils.{CompassDirection => CDir}
import geometry._
import geometry.syntax._
import LabelWidgetF._

import utils.ScalazTreeImplicits._
import TypeTags._
import PageComponentImplicits._
import GeometricFigure._


// Position transform:
//  capture the absolute positioning of a widget along with the matrix transform
//    used to get it there. The inverse of the transform matrix is used to transform
//    selection boxes and clicked points back into the correct geometry to determine
//    what characters within a document are selected
case class WidgetPositioning(
  widget: LabelWidgetF[Unit],
  widgetBounds: LTBounds,
  translation: PositionVector,
  scaling: Double = 1.0d,
  id: Int@@WidgetID
)


case class WidgetLayout(
  positioning: Seq[WidgetPositioning],
  layoutBounds: LTBounds
)

// Accumulator for calculating layout positioning transforms
case class PosAttr(
  widget: LabelWidgetF[Unit],
  widgetBounds: LTBounds,
  id: Int@@WidgetID,
  selfOffset: PositionVector,
  childOffsets: List[PositionVector] = List(),
  scaling: Double = 1.0d
) {

  def toStackString = {
    val soff = selfOffset.prettyPrint
    val ch = childOffsets.map(_.prettyPrint).mkString(", ")
    s"curVec:${soff}   st:[ ${ch} ]"
  }

  override def toString = {
    val wpp = widgetBounds.prettyPrint
    val sstr = toStackString
    val cn = widget.getClass().getName.split("\\$").last
    s"${cn}#${id} bb:${wpp} ${sstr} ]"
  }

  def translate(pvec: PositionVector): PosAttr = {
    PosAttr(
      widget,
      widgetBounds.translate(pvec),
      id,
      selfOffset.translate(pvec),
      childOffsets.map(_.translate(pvec))
    )
  }
}

object LabelWidgetLayoutHelpers {
  def cofreeLWAttrToTree[A](c: Cofree[LabelWidgetF, A]): Tree[A] = {
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeLWAttrToTree(_))
    )
  }

  import textboxing.{TextBoxing => TB}

  def printTree(pos: Cofree[LabelWidgetF, PosAttr]): TB.Box = {
    cofreeLWAttrToTree(
      pos.map(posAttr => posAttr.toString)
    ).drawBox
  }

  def widgetRegionToPageRegion(wpos: WidgetPositioning, widgetSpaceRegion: LTBounds): LTBounds = {
    widgetSpaceRegion.translate(wpos.translation)
  }

  def clipPageRegionFromWidgetSpace(wpos: WidgetPositioning, widgetSpaceRegion: LTBounds): Option[PageRegion] = {
    val pageRegion = widgetRegionToPageRegion(wpos, widgetSpaceRegion)
    wpos.widget match {
      case l @ RegionOverlay(under, overs) =>
        under.intersection(pageRegion)
      case l @ LabeledTarget(target, label, score) =>
        target.intersection(pageRegion)
      case _ => None
    }
  }

}


trait LabelWidgetLayout extends LabelWidgetBasics {


  val zeroLTBounds: LTBounds = LTBounds(0, 0, 0, 0)
  val zeroPosVector: PositionVector = Point(0, 0)

  // Natural Transformation  from EnvT ~> LabelWidget
  def stripLWEnv = new (EnvT[PosAttr, LabelWidgetF, ?] ~> LabelWidgetF[?]) {
    def apply[A](env: EnvT[PosAttr, LabelWidgetF, A]): LabelWidgetF[A] = {
      env.lower
    }
  }

  // calc child offset vectors and total bounding box
  def computeOffsets(
    relativePositions: List[(LabelWidget, PosAttr)],
    offsetFn: (LTBounds, PosAttr) => PositionVector
  ): (LTBounds, List[PositionVector]) = {
    val newpositions = relativePositions
      .foldLeft((zeroLTBounds, List[PositionVector]()))({
        case (acc@(currBounds, childVecs), (_, oldpos)) =>
          val newChildVec = offsetFn(currBounds, oldpos)
          val newpos = oldpos.translate(newChildVec)
          val newBounds = currBounds union newpos.widgetBounds

          (newBounds, newChildVec :: childVecs)
      })

    (newpositions._1, newpositions._2.reverse)
  }

  def layoutWidgetPositions(lwidget: LabelWidget): WidgetLayout = {
    val idgen = utils.IdGenerator[WidgetID]()

    val F = LabelWidgetFunctor
    // Bottom-up first pass evaluator
    def positionAttrs: GAlgebra[(LabelWidget, ?), LabelWidgetF, PosAttr] = fwa => {
      fwa match {
        case flw @ RegionOverlay(under, overs)  =>
          val selfPosition = under.bbox.toPoint(CDir.NW)
          val bbox = under.bbox.moveToOrigin
          val (_, childAdjustVecs) = computeOffsets(overs,
            {(cbbox, overpos) =>
              val childPosition = overpos.selfOffset
              childPosition - selfPosition
            })

          // PosAttr(F.void(flw), bbox, idgen.nextId, zeroPosVector, childAdjustVecs)
          PosAttr(F.void(flw), bbox, idgen.nextId, selfPosition, childAdjustVecs)

        case flw @ LabeledTarget(target, label, score)   =>
          val bbox = target.bbox.moveToOrigin()
          val positionVec = target.bbox.toPoint(CDir.NW)
          PosAttr(F.void(flw), bbox, idgen.nextId, positionVec)

        case flw @ Col(attrs) =>
          val (bbox, childAdjustVecs) = computeOffsets(attrs, {(bbox, childPos)=> bbox.toPoint(CDir.SW)  })
          PosAttr(F.void(flw), bbox, idgen.nextId, zeroPosVector, childAdjustVecs)

        case flw @ Row(attrs) =>
          val (bbox, childAdjustVecs) = computeOffsets(attrs, {(bbox, childPos)=> bbox.toPoint(CDir.NE)  })
          PosAttr(F.void(flw), bbox, idgen.nextId, zeroPosVector, childAdjustVecs)

        case flw @ Pad(p@(a, attr), padding, color) =>
          val ulOffset = Point(padding.left, padding.top)

          val (chbbox, childAdjustVecs) = computeOffsets(List(p), {(bbox, childPos)=> ulOffset})

          val bbox = LTBounds(
            chbbox.left, chbbox.top,
            chbbox.width + padding.left + padding.right,
            chbbox.height + padding.top + padding.bottom
          )

          PosAttr(F.void(flw), bbox, idgen.nextId, zeroPosVector, childAdjustVecs)

        case flw @ Reflow(textReflow) =>
          val bounds = textReflow.bounds()
          val bbox = bounds.moveToOrigin

          PosAttr(F.void(flw), bbox, idgen.nextId, zeroPosVector, List())

        case flw @ TextBox(box) =>
          val str = box.toString
          val lines = str.split("\n")
          val height = lines.length
          val maxwidth = lines.map(_.length).max
          val bbox: LTBounds = LTBounds(0, 0, maxwidth*6d, height*16d)

          PosAttr(F.void(flw), bbox, idgen.nextId, zeroPosVector, List())

        case flw @ Figure(figure) =>
          val bbox = totalBounds(figure)

          PosAttr(F.void(flw), bbox, idgen.nextId, zeroPosVector, List())

      }
    }


    def putStrLn[S](str: => String): State[S, Unit] =
      State.state[S, Unit]( println(str) )

    def adjustPositions(
      selfAttr: PosAttr, ft:LabelWidgetF[_]
    ): State[PosAttr, PosAttr] = {

      for {
        // Initial state passed from parent
        init       <- State.get[PosAttr]
        // Initial stack passed from parent
        initStack   = init.childOffsets

        // Current offset vector is the top of initial stack
        headOffsetVec  = initStack.head
        tailOffsetVecs = initStack.tail

        // Relative offset vectors for children of the current node
        selfOffsetVecs   = selfAttr.childOffsets

        // Translated current-node offset vectors from Relative -> Absolute positioning
        selfAbsOffsetVecs = selfOffsetVecs.map(_.translate(headOffsetVec))

        // Adjusted current-node bounding box to Absolute positioning
        newSelf = selfAttr.copy(
          widgetBounds = selfAttr.widgetBounds.translate(headOffsetVec),
          selfOffset   = selfAttr.selfOffset.translate(-headOffsetVec)
        )

        // Update State monad to include repositioned offset vectors for current-node's children
        newState = init.copy(
          childOffsets = selfAbsOffsetVecs ++ tailOffsetVecs
        )

        _      <- State.modify[PosAttr](_ => newState)
      } yield {
        newSelf
      }
    }


    val relativePositioned: Cofree[LabelWidgetF, PosAttr] =
      lwidget.cata(attributePara(positionAttrs))

    val zero = PosAttr(TextBox("dummy"), zeroLTBounds, WidgetID(0), Point(0, 0), List(Point(0, 0)))

    val adjusted: Cofree[LabelWidgetF, PosAttr] = relativePositioned
      .attributeTopDownM[State[PosAttr, ?], PosAttr](zero)({
        case e => adjustPositions(e._2.ask, e._2.lower)
      })
      .eval(zero)
      .mapBranching(stripLWEnv)


    val positions = adjusted.universe
        .map(_.head)
        .map(w => WidgetPositioning(w.widget, w.widgetBounds, w.selfOffset, w.scaling, w.id))

    val root = positions.head

    WidgetLayout(positions, root.widgetBounds)

  }

}
