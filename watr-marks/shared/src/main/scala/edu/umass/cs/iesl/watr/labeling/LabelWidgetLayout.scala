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


// Position transform:
//  capture the absolute positioning of a widget along with the matrix transform
//    used to get it there. The inverse of the transform matrix is used to transform
//    selection boxes and clicked points back into the correct geometry to determine
//    what characters within a document are selected
case class WidgetPositioning(
  widget: LabelWidgetF[Unit],
  strictBounds: LTBounds,
  bleedBounds: LTBounds,
  translation: PositionVector,
  scaling: Double = 1.0d,
  id: Int@@WidgetID
)


case class WidgetLayout(
  positioning: Seq[WidgetPositioning],
  strictBounds: LTBounds,
  bleedBounds: LTBounds,
  labelWidget: LabelWidget
)

// Accumulator for calculating layout positioning transforms
case class PosAttr(
  widget: LabelWidgetF[Unit],
  strictBounds: LTBounds,
  bleedBounds: LTBounds,
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
    val wpp = strictBounds.prettyPrint
    val sstr = toStackString
    val cn = widget.getClass().getName.split("\\$").last
    s"${cn}#${id} bb:${wpp} ${sstr} ]"
  }

  def translate(pvec: PositionVector): PosAttr = {
    PosAttr(
      widget,
      strictBounds.translate(pvec),
      bleedBounds.translate(pvec),
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

  // def clipPageRegionFromWidgetSpace(wpos: WidgetPositioning, widgetSpaceRegion: LTBounds): Option[TargetRegion] = {
  //   val pageRegion = widgetRegionToPageRegion(wpos, widgetSpaceRegion)
  //   wpos.widget match {
  //     case l @ RegionOverlay(under, overs) =>
  //       under.intersection(pageRegion)
  //     case l @ LabeledTarget(target, label, score) =>
  //       target.intersection(pageRegion)
  //     case _ => None
  //   }
  // }

}


trait LabelWidgetLayout extends LabelWidgetBasics {
  import utils.IdGenerator


  val zeroLTBounds: LTBounds = LTBounds(0, 0, 0, 0)
  val zeroPosVector: PositionVector = Point(0, 0)

  // Natural Transformation  from EnvT ~> LabelWidget
  def stripLWEnv = new (EnvT[PosAttr, LabelWidgetF, ?] ~> LabelWidgetF[?]) {
    def apply[A](env: EnvT[PosAttr, LabelWidgetF, A]): LabelWidgetF[A] = {
      env.lower
    }
  }


  def repositionChildren(
    children: List[PosAttr],
    offsetFn: (LTBounds, PosAttr) => PositionVector
  ): (LTBounds, LTBounds, List[PositionVector]) = {
    val newpositions =
      (children foldLeft (zeroLTBounds, zeroLTBounds, List[PositionVector]())) {
        case ((currStrictBounds, currBleedBounds, childVecs), oldpos) =>
          val newChildVec = offsetFn(currStrictBounds, oldpos)
          val newpos = oldpos.translate(newChildVec)
          val newStrictBounds = currStrictBounds union newpos.strictBounds
          val newBleedBounds = (currBleedBounds union newpos.bleedBounds) union newStrictBounds
          println(s"repositionChildren:  ${oldpos.widget}")
          println(s"    : str       ${currStrictBounds}")
          println(s"    : bleed     ${currBleedBounds}")
          println(s"    : new str   ${newStrictBounds}")
          println(s"    : new bleed ${newBleedBounds}")
          println()

          (newStrictBounds, newBleedBounds, newChildVec :: childVecs)
      }

    (newpositions._1, newpositions._2, newpositions._3.reverse)
  }


  // TODO: I don't think I need to run repositionChildren here, just propagate child pos info
  def inheritChildLayout(fv: LabelWidgetF[Unit], childPos: PosAttr, idgen: IdGenerator[WidgetID]): PosAttr = {
    val (childBBox, chBleed, childAdjustVecs) = repositionChildren(
      List(childPos),
      { (childrenBbox, childPos) => childrenBbox.toPointUpLeft() }
    )
    val bbox = childPos.strictBounds
    PosAttr(fv, bbox, chBleed, idgen.nextId, zeroPosVector, childAdjustVecs)
  }


  def layoutWidgetPositions(lwidget: LabelWidget): WidgetLayout = {
    val idgen = IdGenerator[WidgetID]()

    val F = LabelWidgetFunctor
    // Bottom-up first pass evaluator
    def positionAttrs: GAlgebra[(LabelWidget, ?), LabelWidgetF, PosAttr] = fwa => {
      fwa match {

        case flw @ RegionOverlay(pageId, pGeom, clipTo, overlays) =>
          val clipBox = clipTo.getOrElse { pGeom }
          val bbox = clipBox.moveToOrigin
          val selfPosition = clipBox.toPoint(CDir.NW)

          val (childBbox, chBleed, childAdjustVecs) =
            repositionChildren(
              overlays.map(_._2), {
                (totalChildsBbox, childPos) =>
                val childPosition = childPos.selfOffset
                childPosition - selfPosition
              })

          val totalBleed = bbox union chBleed

          PosAttr(F.void(flw), bbox, totalBleed, idgen.nextId, selfPosition, childAdjustVecs)


        case flw @ Col(attrs) =>
          val (bbox, chBleed, childAdjustVecs) = repositionChildren(
            attrs.map(_._2),
            {(totalChildsBbox, childPos)=> totalChildsBbox.toPoint(CDir.SW) }
          )
          PosAttr(F.void(flw), bbox, bbox, idgen.nextId, zeroPosVector, childAdjustVecs)

        case flw @ Panel(p@(a, attr), action) =>
          inheritChildLayout(F.void(flw), attr, idgen)

        case flw @ Identified(p@(a, attr), id, cls) =>
          inheritChildLayout(F.void(flw), attr, idgen)

        case flw @ Row(attrs) =>
          val (bbox, chBleed, childAdjustVecs) = repositionChildren(attrs.map(_._2), {(totalChildsBbox, childPos)=> totalChildsBbox.toPoint(CDir.NE)  })
          PosAttr(F.void(flw), bbox, chBleed, idgen.nextId, zeroPosVector, childAdjustVecs)

        case flw @ Pad(p@(a, attr), padding, color) =>
          val ulOffset = Point(padding.left, padding.top)

          val (childBbox, childBleed, childAdjustVecs) = repositionChildren(List(attr), {(totalChildsBbox, childPos)=> ulOffset})

          val bbox = LTBounds(
            childBbox.left, childBbox.top,
            childBbox.width + padding.left + padding.right,
            childBbox.height + padding.top + padding.bottom
          )

          PosAttr(F.void(flw), bbox, childBleed, idgen.nextId, zeroPosVector, childAdjustVecs)

        case flw @ Reflow(textReflow) =>
          val bounds = textReflow.bounds()
          val bbox = bounds.moveToOrigin

          PosAttr(F.void(flw), bbox, bbox, idgen.nextId, zeroPosVector)

        case flw @ TextBox(box) =>
          val str = box.toString
          val lines = str.split("\n")
          val height = lines.length
          val maxwidth = lines.map(_.length).max
          val bbox: LTBounds = LTBounds(0, 0, maxwidth*6d, height*16d)

          PosAttr(F.void(flw), bbox, bbox, idgen.nextId, zeroPosVector)

        case flw @ Figure(figure) =>
          val bbox = totalBounds(figure)
          PosAttr(F.void(flw), bbox, bbox, idgen.nextId, zeroPosVector)

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
          strictBounds = selfAttr.strictBounds.translate(headOffsetVec),
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

    val zero = PosAttr(TextBox("dummy"), zeroLTBounds, zeroLTBounds, WidgetID(0), Point(0, 0), List(Point(0, 0)))

    val adjusted: Cofree[LabelWidgetF, PosAttr] = relativePositioned
      .attributeTopDownM[State[PosAttr, ?], PosAttr](zero)({
        case e => adjustPositions(e._2.ask, e._2.lower)
      })
      .eval(zero)
      .mapBranching(stripLWEnv)


    val positions = adjusted.universe
      .map(_.head)
      .map(w => WidgetPositioning(w.widget, w.strictBounds, w.bleedBounds, w.selfOffset, w.scaling, w.id))
      .toList

    val root = positions.head

    WidgetLayout(positions, root.strictBounds, root.bleedBounds, lwidget)

  }

}
