package edu.umass.cs.iesl.watr
package labeling

import scalaz.{@@ => _, _}, Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

import textreflow.data._
import utils.{RelativeDirection => Dir}
import utils.ExactFloats._
import geometry._
import geometry.syntax._
import LabelWidgetF._

import utils.ScalazTreeImplicits._
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
    parent: LabelWidgetF[Unit],
    initBounds: LTBounds,
    children: List[PosAttr],
    offsetFn: (LTBounds, PosAttr) => PositionVector
  ): (LTBounds, LTBounds, List[PositionVector]) = {
    val newpositions =
      (children foldLeft ( (initBounds, initBounds, List[PositionVector]())) )  {
        case ((currStrictBounds, currBleedBounds, childVecs), child) =>
          val newChildVec = offsetFn(currStrictBounds, child)
          val newpos = child.translate(newChildVec)
          val newStrictBounds = newpos.strictBounds union currStrictBounds
          val newBleedBounds = newpos.bleedBounds union newStrictBounds
          // println(s"repositionChildren of ${parent}")
          // println(s"   child:  ${child.widget}")
          // println(s"   : str       ${currStrictBounds}")
          // println(s"   :  ->       ${newStrictBounds}")
          // println(s"   : bleed     ${currBleedBounds}")
          // println(s"   :   ->      ${newBleedBounds}")
          // println()

          (newStrictBounds, newBleedBounds, newChildVec :: childVecs)
      }

    (newpositions._1, newpositions._2, newpositions._3.reverse)
  }

  def inheritChildLayout(fv: LabelWidgetF[Unit], childPos: PosAttr, zOrder:Option[Int]=None): PosAttr = {
    PosAttr(fv, childPos.strictBounds, childPos.bleedBounds, zOrder.getOrElse(0), Point.zero,  List(Point.zero))
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
          val fv = F.void(flw)
          val selfTranslation = -under.bbox.toPoint(Dir.TopLeft)
          val selfStrictBounds = under.bbox.moveToOrigin

          val childMaxZ = childMaxZIndex(overlays.map(_._2))

          val (_, chBleed, childTranslations) =
            repositionChildren(
              fv,
              selfStrictBounds,
              overlays.map(_._2), {
                (totalChildsBbox, childNode) =>
                selfTranslation //  + childNode.selfTranslation
              })

          val totalBleed = selfStrictBounds union chBleed

          PosAttr(F.void(flw), selfStrictBounds, totalBleed, childMaxZ+1, selfTranslation, childTranslations)

        case flw @ Row(wid, attrs) =>
          val fv = F.void(flw)
          val (selfStrictBounds, chBleed, childTranslations) =
            repositionChildren(
              fv,
              LTBounds.empty,
              attrs.map(_._2), {
                (totalChildsBbox, childNode) =>
                totalChildsBbox.toPoint(Dir.TopRight).copy(y=0.toFloatExact)
              })

          val childMaxZ = childMaxZIndex(attrs.map(_._2))

          PosAttr(fv, selfStrictBounds, chBleed, childMaxZ+1, Point.zero, childTranslations)

        case flw @ Col(wid, attrs) =>
          val fv = F.void(flw)
          val (selfStrictBounds, _, childTranslations) =
            repositionChildren(
              fv,
              LTBounds.empty,
              attrs.map(_._2), {
                (totalChildsBbox, childNode)=>
                totalChildsBbox.toPoint(Dir.BottomLeft).copy(x=0.toFloatExact)
              })

          val childMaxZ = childMaxZIndex(attrs.map(_._2))

          PosAttr(F.void(flw), selfStrictBounds, selfStrictBounds, childMaxZ+1, Point.zero, childTranslations)

        case flw @ ZStack(wid, attrs) =>
          val fv = F.void(flw)
          val (selfStrictBounds, _, childTranslations) =
            repositionChildren(
              fv,
              LTBounds.empty,
              attrs.map(_._2),
              {(totalChildsBbox, childNode) => Point.zero }
            )
          val childMaxZ = childMaxZIndex(attrs.map(_._2))

          PosAttr(F.void(flw), selfStrictBounds, selfStrictBounds, childMaxZ+1, Point.zero, childTranslations)

        case flw @ Panel(wid, p@(a, attr), action) =>
          inheritChildLayout(F.void(flw), attr, Some(-attr.zOrder))

        case flw @ Labeled(wid, p@(a, attr), k, v) =>
          inheritChildLayout(F.void(flw), attr)

        case flw @ Identified(wid, p@(a, attr), id, cls) =>
          inheritChildLayout(F.void(flw), attr)


        case flw @ Pad(wid, p@(a, attr), padding, color) =>
          val fv = F.void(flw)
          val selfTranslation = Point(padding.left, padding.top)
          val startBounds = LTBounds.empty.translate(selfTranslation)

          val (childBbox, childBleed, childTranslations) =
            repositionChildren(
              fv,
              startBounds,
              List(attr), {
                (totalChildsBbox, childNode)=>
                selfTranslation //  + childNode.selfTranslation
              })

          val selfStrictBounds = LTBounds(
            0.toFloatExact, 0.toFloatExact,
            childBbox.width + padding.left + padding.right,
            childBbox.height + padding.top + padding.bottom
          )

          PosAttr(F.void(flw), selfStrictBounds, childBleed, attr.zOrder+1, Point.zero, childTranslations)

        case flw @ Reflow(wid, textReflow) =>
          val bounds = textReflow.bounds()
          val selfStrictBounds = bounds.moveToOrigin

          PosAttr(F.void(flw), selfStrictBounds, selfStrictBounds, 0, Point.zero)

        case flw @ TextBox(wid, box) =>
          val str = box.toString
          val lines = str.split("\n")
          val height = lines.length
          val maxwidth = lines.map(_.length).max
          val selfStrictBounds: LTBounds = LTBounds.Doubles(0, 0, maxwidth*6d, height*16d)

          PosAttr(F.void(flw), selfStrictBounds, selfStrictBounds, 0, Point.zero)

        case flw @ Figure(wid, figure) =>
          val selfStrictBounds = minBoundingRect(figure)

          PosAttr(F.void(flw), selfStrictBounds, selfStrictBounds, 10, Point.zero)

        case flw @ Terminal =>
          val selfStrictBounds: LTBounds = LTBounds.zero

          PosAttr(F.void(flw), selfStrictBounds, selfStrictBounds, 0, Point.zero)
      }
    }

    def putStrLn[S](str: => String): State[S, Unit] =
      State.state[S, Unit]( () )
      // State.state[S, Unit]( println(str) )


    def adjustPositions(
      selfAttr: PosAttr, ft:LabelWidgetF[_]
    ): State[PosAttr, PosAttr] = {

      for {
        // Initial state passed from parent
        init       <- State.get[PosAttr]
        // Initial stack passed from parent
        initStack   = init.childTranslations

        lpad = "   " * initStack.length
        _ <- putStrLn(s"${lpad}>${selfAttr.widget}")

        // Current offset vector is the top of initial stack
        currOffsetVec  = initStack.head
        tailOffsetVecs = initStack.tail



        // Relative offset vectors for children of the current node
        // childTranslations   = selfAttr.childTranslations

        // Translated current-node offset vectors from Relative -> Absolute positioning
        childAbsTranslations = selfAttr.childTranslations.map(_ + currOffsetVec)

        selfAbsOffsetVec  = selfAttr.selfTranslation + currOffsetVec

        // Adjusted current-node bounding box to Absolute positioning
        newSelf = selfAttr.copy(
          strictBounds      = selfAttr.strictBounds.translate(currOffsetVec), // .translate(selfAbsOffsetVec),
          bleedBounds       = selfAttr.bleedBounds.translate(currOffsetVec), // .translate(selfAbsOffsetVec),
          selfTranslation   = selfAbsOffsetVec,
          zOrder            = selfAttr.zOrder+init.zOrder
        )


        // Update State monad to include repositioned offset vectors for current-node's children
        newState = init.copy(
          childTranslations = childAbsTranslations ++ tailOffsetVecs
        )

        _ <- putStrLn(s"${lpad}   initOffsetVec: ${currOffsetVec}")
        _ <- putStrLn(s"${lpad}   self adj  Vec: ${selfAbsOffsetVec}")
        _ <- putStrLn(s"${lpad}   init stack   : ${tailOffsetVecs}")
        _ <- putStrLn(s"${lpad}   strictBounds : ${selfAttr.strictBounds}")
        _ <- putStrLn(s"${lpad}     to ->      : ${newSelf.strictBounds}")

        _      <- State.modify[PosAttr](_ => newState)
      } yield {
        // println(s"adjustPositions: ")
        // println(s"    selfNode: ${selfAttr} ")
        // println(s" -> newSelf : ${newSelf} ")
        // println(s"    initSt  : ${init} ")
        // println(s" -> newSt   : ${newState} ")
        newSelf
      }
    }


    val relativePositioned: Cofree[LabelWidgetF, PosAttr] =
      lwidget.cata(attributePara(positionAttrs))


    // println("relative")
    // println(printTree(relativePositioned))

    val zero = PosAttr(Terminal, LTBounds.empty, LTBounds.empty, 0, Point.zero, List(Point.zero))


    val adjusted: Cofree[LabelWidgetF, PosAttr] =
      relativePositioned
        .attributeTopDownM[State[PosAttr, ?], Cofree[EnvT[PosAttr, LabelWidgetF, ?], PosAttr], PosAttr](zero)({
          case e => adjustPositions(e._2.ask, e._2.lower)
        })
        .eval(zero)
        .mapBranching(stripLWEnv)

    // println("absolute")
    // println(printTree(adjusted))

    val positions = adjusted.elgotPara(universe)
      .map(_.head)
      .map(w => AbsPosWidget(w.widget, w.strictBounds, w.bleedBounds, w.selfTranslation, w.zOrder, w.scaling))
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
    val strictBounds = LTBounds.empty union root.strictBounds
    val bleedBounds = LTBounds.empty union root.bleedBounds

    // println(s"Root: ${root}")
    // println(s"strictBounds: ${strictBounds}")
    // println(s"bleedBounds: ${bleedBounds}")

    WidgetLayout(bottoms++tops, strictBounds, bleedBounds, lwidget)

  }

}
