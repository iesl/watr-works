package edu.umass.cs.iesl.watr
package display

import scalaz.{@@ => _, _}, Scalaz._

// import scalaz.Cofree
// import scalaz.State
// import scalaz.Tree
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
import utils.ScalazTreeImplicits._
import TypeTags._


case class PosAttr(
  widgetBounds: LTBounds,
  id: Int@@RegionID,
  selfOffset: PositionVector = Point(0, 0),
  childOffsets: List[PositionVector] = List()
) {
  def toStackString = {
    val soff = selfOffset.prettyPrint
    val ch = childOffsets.map(_.prettyPrint).mkString(", ")
    s"curVec:${soff}   st:[ ${ch} ]"

  }
  override def toString = {
    val wpp = widgetBounds.prettyPrint
    val soff = selfOffset.prettyPrint
    val ch = childOffsets.map(_.prettyPrint).mkString(", ")
    s"#${id} vec:$soff bb:${wpp} [ ${ch} ]"

  }
  def translate(pvec: PositionVector): PosAttr = {
    PosAttr(
      widgetBounds.translate(pvec),
      id,
      selfOffset.translate(pvec),
      childOffsets.map(_.translate(pvec))
    )
  }
}


trait LabelWidgetLayout extends LabelWidgetBasics {

  def layoutWidgetPositions(lwidget: LabelWidget): Position = ???

  def prettyPrintPosition(t: Position): TB.Box = {
    ???
  }

  val zeroLTBounds: LTBounds = LTBounds(0, 0, 0, 0)
  val zeroPosVector: PositionVector = Point(0, 0)


  import matryoshka.patterns.EnvT

  // Natural Transformation  from EnvT ~> LabelWidget
  def stripLWEnv = new (EnvT[PosAttr, LabelWidgetF, ?] ~> LabelWidgetF[?]) {
    def apply[A](env: EnvT[PosAttr, LabelWidgetF, A]): LabelWidgetF[A] = {
      env.lower
    }
  }
  def cofreeLWAttrToTree[A](c: Cofree[LabelWidgetF, A]): Tree[A] = {
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeLWAttrToTree(_))
    )
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
  def layoutWidgetPositions2(lwidget: LabelWidget): Cofree[(LabelWidget, ?), PosAttr] = {
    val idgen = utils.IdGenerator[RegionID]()

    // Bottom-up first pass evaluator
    def positionAttrs: GAlgebra[(LabelWidget, ?), LabelWidgetF, PosAttr] = fwa => {
      fwa match {
        case TargetOverlay(under, overs)  =>
          val positionVec = under.bbox.toPoint(CDir.NW)
          val bbox = under.bbox.moveToOrigin
          val (_, childAdjustVecs) = computeOffsets(overs, {(_, _)=> ??? })

          PosAttr(bbox, idgen.nextId, positionVec, childAdjustVecs)

        case LabeledTarget(target, label, score)   =>
          val bbox = target.bbox
          PosAttr(bbox, idgen.nextId)

        case Col(attrs) =>
          val (bbox, childAdjustVecs) = computeOffsets(attrs, {(bbox, childPos)=> bbox.toPoint(CDir.SW)  })
          PosAttr(bbox, idgen.nextId, zeroPosVector, childAdjustVecs)


        case Row(attrs) =>
          val (bbox, childAdjustVecs) = computeOffsets(attrs, {(bbox, childPos)=> bbox.toPoint(CDir.NE)  })
          PosAttr(bbox, idgen.nextId, zeroPosVector, childAdjustVecs)

        case Pad(p@(a, attr), padding) =>
          val (chbbox, childAdjustVecs) = computeOffsets(List(p), {(bbox, childPos)=> bbox.toPoint(CDir.NE)  })

          val bbox = LTBounds(
            chbbox.left, chbbox.top,
            chbbox.width + padding.left + padding.right,
            chbbox.height + padding.top + padding.bottom
          )

          PosAttr(bbox, idgen.nextId, zeroPosVector, childAdjustVecs)

        // // case RangeSelection(range) =>
        // //   ???

        case l @ Reflow(textReflow) =>
          val target = textReflow.targetRegion
          val bbox = target.bbox.moveToOrigin

          PosAttr(bbox, idgen.nextId, zeroPosVector, List())

        // case l @ TextBox(box) =>
        //   val str = box.toString
        //   val lines = str.split("\n")
        //   val height = lines.length
        //   val maxwidth = lines.map(_.length).max
        //   val bbox: LTBounds = LTBounds(0, 0, maxwidth*6d, height*16d)

        //   PosAttr(bbox, idgen.nextId)

        // case Button(action) =>
        //   val width = (action.length+1) * 6d
        //   val height = 18d
        //   val bbox: LTBounds = LTBounds(0, 0, width, height)

        //   PosAttr(bbox, idgen.nextId)

        // case l @ Panel(p@(content, attr)) =>
        //   val (bbox, repos) = reposition(List(p), _.toPoint(CDir.NW))

        //   PosAttr(bbox, idgen.nextId)

        // case l @ MouseOverlay(p@(bkplane, attr)) =>
        //   val (bbox, repos) = reposition(List(p), _.toPoint(CDir.NW))
        //   PosAttr(bbox, idgen.nextId)
      }
    }

    val relativePositioned: Cofree[LabelWidgetF, PosAttr] =
      lwidget.cata(attributePara(positionAttrs))



    val withOffs = cofreeLWAttrToTree(
      relativePositioned.map(posAttr => posAttr.toString)
    ).drawBox
    println("withOffs")
    println(withOffs)

    def putStrLn[S](str: => String): State[S, Unit] =
      State.state[S, Unit]( println(str) )


    def adjustPositions(
      selfAttr: PosAttr, ft:LabelWidgetF[_]
    ): State[PosAttr, PosAttr] = {

      for {
        _           <- putStrLn(s"@self: ${selfAttr}")

        init       <- State.get[PosAttr]
        initStack   = init.childOffsets
        currVec     = init.selfOffset

        selfStack   = selfAttr.childOffsets

        adjustedSelfStack = selfStack.map(_.translate(currVec))
        totalStack = adjustedSelfStack ++ initStack
        newCurrVec = totalStack.head
        newStack   = totalStack.tail

        // offStack    = initChOffs ++ selfChOffs
        // offStackStr = childOffsets.map(_.prettyPrint).mkString(", ")

        _           <- putStrLn(s"   init : ${init.toStackString}")

        newState = {
          init.copy(
            selfOffset = newCurrVec,
            childOffsets = newStack
          )

          // initChOffs match {
          //   case Nil =>
          //     init.copy(
          //       childOffsets = selfChOffs
          //     )

          //   case offs :: tail =>
          //     init.copy(
          //       selfOffset = offs,
          //       childOffsets = selfChOffs ++ tail
          //     )
          // }
        }

        newSelf = selfAttr.copy(
          widgetBounds=selfAttr.widgetBounds.translate(newCurrVec)
        )

        _      <- putStrLn(s"   mods: ${newState.toStackString}")
        _      <- putStrLn(s"   <-  : ${newSelf}")

        _      <- State.modify[PosAttr](_ => newState)
      } yield {
        newSelf
      }
    }

    val zero = PosAttr(zeroLTBounds, RegionID(0), Point(0, 0), List(Point(0, 0)))
    // val zero = PosAttr(zeroLTBounds, RegionID(0), Point(0,0), List())

    val adjusted: Cofree[LabelWidgetF, PosAttr] = relativePositioned
      .attributeTopDownM[State[PosAttr, ?], PosAttr](zero)({
        case e => adjustPositions(e._2.ask, e._2.lower)
      })
      .eval(zero)
      .mapBranching(stripLWEnv)

    val adjOffs = cofreeLWAttrToTree(
      adjusted.map(posAttr => posAttr.toString)
    ).drawBox

    println("adjOffs")
    println(adjOffs)

    ???
  }
}











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
