package edu.umass.cs.iesl.watr
package labeling

import scalaz.Free
import scalaz.~>

import geometry._
import watrmarks.Label

sealed trait Gesture
case class SelectRegion(bbox: LTBounds) extends Gesture
case class Click(point: Point) extends Gesture
case class DblClick(point: Point) extends Gesture

sealed trait Constraint

case object ByLine extends Constraint
case object ByChar extends Constraint
case object ByRegion extends Constraint

sealed trait Interaction

case class InteractProg[A](
  a: Free[LabelAction, A]
) extends Interaction

case object InteractNil extends Interaction

sealed trait LabelAction[A]

object LabelAction {

  case class SelectRegion(g: TargetRegion)     extends LabelAction[Unit]
  case class UnselectRegion(g: TargetRegion)   extends LabelAction[Unit]
  case class SelectZone(g: Int@@ZoneID)           extends LabelAction[Int@@ZoneID]
  case class UnselectZone(g: Int@@ZoneID)         extends LabelAction[Unit]
  case class CreateZone(gs: Seq[TargetRegion]) extends LabelAction[Unit]
  case class DeleteZone(z: Int@@ZoneID)           extends LabelAction[Unit]
  case class LabelZone(z: Int@@ZoneID, l: Label)  extends LabelAction[Unit]

  // case object GetSelectedRegions                  extends LabelAction[Seq[TargetRegion]]
  // case class CreateZone(gs: Seq[TargetRegion]) extends LabelAction[Int@@ZoneID]
  // case object GetSelectedZones                    extends LabelAction[Seq[Int@@ZoneID]]

  case class CreateFigure(
    figure: GeometricFigure,
    targetPage: RecordedPageID
  ) extends LabelAction[TargetRegion]

  case class QueryForRegions(
    queryRegion: LTBounds,
    constraint: Constraint,
    targetPage: RecordedPageID
  ) extends LabelAction[Seq[TargetRegion]]

  case class QueryForZones(
    queryRegion: LTBounds,
    targetPage: RecordedPageID
  ) extends LabelAction[Seq[Int@@ZoneID]]


  def createFigure(f: GeometricFigure, p: RecordedPageID)               = Free.liftF{ CreateFigure(f, p) }
  def queryForRegions(q: LTBounds, c: Constraint, t: RecordedPageID)    = Free.liftF{ QueryForRegions(q, c, t) }
  def selectZone(g: Int@@ZoneID)                                        = Free.liftF{ SelectZone(g) }
  def unselectZone(g: Int@@ZoneID)                                      = Free.liftF{ UnselectZone(g) }


  def clickToSelectZone(zoneId: Int@@ZoneID): Interaction = InteractProg(
    for {
      _ <- selectZone(zoneId)
    } yield ()
  )


  val interpLabelAction: LabelAction ~> LabelAction =
    new (LabelAction ~> LabelAction) {
      def apply[A](fa: LabelAction[A]) =
        fa match {
          case SelectZone(zoneId) =>
            fa
          case _ => fa
        }
    }
}
