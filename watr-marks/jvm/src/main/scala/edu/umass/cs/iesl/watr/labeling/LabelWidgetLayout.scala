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
import textboxing.{TextBoxing => TB}


object LabelWidgetLayoutHelpers {
  def cofreeLWAttrToTree[A](c: Cofree[LabelWidgetF, A]): Tree[A] = {
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeLWAttrToTree(_))
    )
  }



  def printTree(pos: Cofree[LabelWidgetF, PosAttr]): TB.Box = {
    cofreeLWAttrToTree(
      pos.map(posAttr => posAttr.toString)
    ).drawBox
  }

  def widgetRegionToPageRegion(wpos: AbsPosWidget, widgetSpaceRegion: LTBounds): LTBounds = {
    widgetSpaceRegion.translate(wpos.translation)
  }


}


trait LabelWidgetLayout extends LabelWidgetBasics {

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
      (children foldLeft (LTBounds.empty, LTBounds.empty, List[PositionVector]())) {
        case ((currStrictBounds, currBleedBounds, childVecs), oldpos) =>
          val newChildVec = offsetFn(currStrictBounds, oldpos)
          val newpos = oldpos.translate(newChildVec)
          val newStrictBounds = currStrictBounds union newpos.strictBounds
          val newBleedBounds = (currBleedBounds union newpos.bleedBounds) union newStrictBounds
          // println(s"repositionChildren:  ${oldpos.widget}")
          // println(s"    : str       ${currStrictBounds}")
          // println(s"    : bleed     ${currBleedBounds}")
          // println(s"    : new str   ${newStrictBounds}")
          // println(s"    : new bleed ${newBleedBounds}")
          // println()
          // DebugLayout.debugPrint(
          //   newStrictBounds,
          //   newBleedBounds,
          //   List(newpos)
          // )

          (newStrictBounds, newBleedBounds, newChildVec :: childVecs)
      }

    (newpositions._1, newpositions._2, newpositions._3.reverse)
  }

  // TODO: I don't think I need to run repositionChildren here, just propagate child pos info
  def inheritChildLayout(fv: LabelWidgetF[Unit], childPos: PosAttr, zOrder:Option[Int]=None): PosAttr = {
    val (childBBox, chBleed, childAdjustVecs) = repositionChildren(
      List(childPos),
      { (childrenBbox, childPos) => childrenBbox.toPointUpLeft() }
    )
    val bbox = childPos.strictBounds
    PosAttr(fv, bbox, chBleed, zOrder.getOrElse(0), Point.zero,  childAdjustVecs)
  }

  def childMaxZIndex(posAttrs: List[PosAttr]): Int = {
    (0 :: posAttrs.map(_.zOrder)).max
  }



  def layoutWidgetPositions(lwidget: LabelWidget): WidgetLayout = {

    val F = LabelWidgetFunctor
    // Bottom-up first pass evaluator
    def positionAttrs: GAlgebra[(LabelWidget, ?), LabelWidgetF, PosAttr] = fwa => {
      fwa match {

        case flw @ RegionOverlay(wid, under, overlays)  =>
          val selfPosition = under.bbox.toPoint(CDir.NW)
          val bbox = under.bbox.moveToOrigin

          val childMaxZ = childMaxZIndex(overlays.map(_._2))

          val (childBbox, chBleed, childAdjustVecs) =
            repositionChildren(
              overlays.map(_._2), {
                (totalChildsBbox, childPos) =>
                val childPosition = childPos.selfOffset
                childPosition - selfPosition
              })

          val totalBleed = bbox union chBleed

          PosAttr(F.void(flw), bbox, totalBleed, childMaxZ+1, selfPosition, childAdjustVecs)

        case flw @ Row(wid, attrs) =>
          val (bbox, chBleed, childAdjustVecs) = repositionChildren(
            attrs.map(_._2),
            {(totalChildsBbox, childPos)=> totalChildsBbox.toPoint(CDir.NE) }
          )
          val childMaxZ = childMaxZIndex(attrs.map(_._2))
          PosAttr(F.void(flw), bbox, chBleed, childMaxZ+1, Point.zero, childAdjustVecs)

        case flw @ Col(wid, attrs) =>
          val (bbox, chBleed, childAdjustVecs) = repositionChildren(
            attrs.map(_._2),
            {(totalChildsBbox, childPos)=> totalChildsBbox.toPoint(CDir.SW) }
          )
          val childMaxZ = childMaxZIndex(attrs.map(_._2))

          PosAttr(F.void(flw), bbox, bbox, childMaxZ+1, Point.zero, childAdjustVecs)

        case flw @ ZStack(wid, attrs) =>
          val (bbox, chBleed, childAdjustVecs) = repositionChildren(
            attrs.map(_._2),
            {(totalChildsBbox, childPos) => Point.zero }
          )
          val childMaxZ = childMaxZIndex(attrs.map(_._2))

          PosAttr(F.void(flw), bbox, bbox, childMaxZ+1, Point.zero, childAdjustVecs)

        case flw @ Panel(wid, p@(a, attr), action) =>
          inheritChildLayout(F.void(flw), attr, Some(-attr.zOrder))

        case flw @ Labeled(wid, p@(a, attr), k, v) =>
          inheritChildLayout(F.void(flw), attr)

        case flw @ Identified(wid, p@(a, attr), id, cls) =>
          inheritChildLayout(F.void(flw), attr)


        case flw @ Pad(wid, p@(a, attr), padding, color) =>
          val ulOffset = Point(padding.left, padding.top)

          val (childBbox, childBleed, childAdjustVecs) = repositionChildren(List(attr), {(totalChildsBbox, childPos)=> ulOffset})

          val bbox = LTBounds(
            childBbox.left, childBbox.top,
            childBbox.width + padding.left + padding.right,
            childBbox.height + padding.top + padding.bottom
          )

          PosAttr(F.void(flw), bbox, childBleed, attr.zOrder+1, Point.zero, childAdjustVecs)

        case flw @ Reflow(wid, textReflow) =>
          val bounds = textReflow.bounds()
          val bbox = bounds.moveToOrigin

          PosAttr(F.void(flw), bbox, bbox, 0, Point.zero)

        case flw @ TextBox(wid, box) =>
          val str = box.toString
          val lines = str.split("\n")
          val height = lines.length
          val maxwidth = lines.map(_.length).max
          val bbox: LTBounds = LTBounds.Doubles(0, 0, maxwidth*6d, height*16d)

          PosAttr(F.void(flw), bbox, bbox, 0, Point.zero)

        case flw @ Figure(wid, figure) =>
          val bbox = totalBounds(figure)
          PosAttr(F.void(flw), bbox, bbox, 10, Point.zero)

        case flw @ Terminal =>
          val bbox: LTBounds = LTBounds.zero

          PosAttr(F.void(flw), bbox, bbox, 0, Point.zero)
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
          selfOffset   = selfAttr.selfOffset.translate(-headOffsetVec),
          zOrder       = selfAttr.zOrder+init.zOrder
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

    val zero = PosAttr(TextBox(WidgetID(0), "dummy"), LTBounds.empty, LTBounds.empty, 0, Point.zero, List(Point.zero))

    val adjusted: Cofree[LabelWidgetF, PosAttr] = relativePositioned
      .attributeTopDownM[State[PosAttr, ?], PosAttr](zero)({
        case e => adjustPositions(e._2.ask, e._2.lower)
      })
      .eval(zero)
      .mapBranching(stripLWEnv)


    val positions = adjusted.universe
      .map(_.head)
      .map(w => AbsPosWidget(w.widget, w.strictBounds, w.bleedBounds, w.selfOffset, w.zOrder, w.scaling))
      .toList


    val (tops, bottoms) = positions.partition { _.widget match {
      case l: Labeled[Unit]       => true

      case t: RegionOverlay[Unit] => false
      case t: Col[Unit]           => false
      case t: ZStack[Unit]        => false
      case t: Panel[Unit]         => false
      case t: Identified[Unit]    => false
      case t: Row[Unit]           => false
      case t: Pad[Unit]           => false
      case t: Reflow              => false
      case t: TextBox             => false
      case t: Figure              => false
      case Terminal               => false
    }}

    val root = positions.head

    WidgetLayout(bottoms++tops, root.strictBounds, root.bleedBounds, lwidget)

  }

}
