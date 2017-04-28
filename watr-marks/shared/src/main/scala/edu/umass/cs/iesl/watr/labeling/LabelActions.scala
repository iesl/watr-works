package edu.umass.cs.iesl.watr
package labeling

import scalaz.Free
// import scalaz.~>

import geometry._
import upickle.default._, Aliases._


sealed trait Constraint

object Constraint {

  implicit val Constraint_RW: RW[Constraint] = macroRW[Constraint]
    // macroRW[ByLine]
    //   .merge(macroRW[ByChar])
    //   .merge(macroRW[ByRegion])
}

case object ByLine extends Constraint
case object ByChar extends Constraint
case object ByRegion extends Constraint

sealed trait Interaction

object Interaction {
  case class InteractProg[A](
    a: Free[LabelAction, A]
  ) extends Interaction

  case object InteractNil extends Interaction


  import upickle.Js

  implicit def Interaction_RW: RW[Interaction] = RW[Interaction](
    {value => Js.Null},
    {case Js.Null => InteractNil}
  )

}

sealed trait LabelAction[A]

object LabelAction {
  def lift[A](a: LabelAction[A]) = Free.liftF{ a }

  case class SelectZone(g: Int@@ZoneID)            extends LabelAction[Unit]
  case class ToggleZoneSelection(g: Int@@ZoneID)   extends LabelAction[Unit]
  case class DeleteZone(z: Int@@ZoneID)            extends LabelAction[Unit]
  case class MergeZones(zs: Seq[Int@@ZoneID])      extends LabelAction[Unit]

  // TODO nav should cover all labeler types/ids/pages
  case class NavigateTo(pageNum: Int)              extends LabelAction[Unit]

  def selectZone(g: Int@@ZoneID)            = Free.liftF{ SelectZone(g) }
  def toggleZoneSelection(g: Int@@ZoneID)   = Interaction.InteractProg(Free.liftF{ ToggleZoneSelection(g) })
  def deleteZone(g: Int@@ZoneID)            = Free.liftF{ DeleteZone(g) }
  def mergeZones(zs: Seq[Int@@ZoneID])      = Free.liftF{ MergeZones(zs) }

  def clickToSelectZone(zoneId: Int@@ZoneID): Interaction = Interaction.InteractProg(
    for {
      _ <- selectZone(zoneId)
    } yield ()
  )

  import TypeTagPicklers._

  implicit val LabelAction_RW: RW[LabelAction[Unit]] =
    macroRW[SelectZone]
      .merge(macroRW[ToggleZoneSelection])
      .merge(macroRW[DeleteZone])
      .merge(macroRW[MergeZones])
      .merge(macroRW[NavigateTo])

  // macroRW[LabelAction[Unit]]


}

sealed trait Gesture
case class SelectRegion(bbox: LTBounds) extends Gesture
case class Click(point: Point) extends Gesture
case class DblClick(point: Point) extends Gesture
case class MenuAction(action: LabelAction[Unit]) extends Gesture

object Gesture {
  import TypeTagPicklers._

  implicit val Gesture_RW: RW[Gesture] =
    macroRW[SelectRegion]
      .merge(macroRW[Click])
      .merge(macroRW[DblClick])
      .merge(macroRW[MenuAction])
}
