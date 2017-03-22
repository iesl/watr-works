package edu.umass.cs.iesl.watr
package labeling

import scalaz.Free
// import scalaz.~>
// import scalaz.std.function._
// import scalaz.syntax.monad._

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
case class ByLabel(l: Label) extends Constraint

sealed trait Interaction

case class InteractProg(
  a: Free[LabelAction, Unit]
) extends Interaction

case class InteractList(
  as: Seq[LabelAction[Unit]]
) extends Interaction

case object InteractNil extends Interaction

sealed trait LabelAction[A]

object LabelAction {

  case class  SelectRegion(g: GeometricRegion)  extends LabelAction[Unit]
  case class  UnselectRegion(g: GeometricRegion)  extends LabelAction[Unit]
  case object GetSelectedRegions extends LabelAction[Seq[GeometricRegion]]

  case class  SelectZone(g: Int@@ZoneID)  extends LabelAction[Unit]
  case class  UnselectZone(g: Int@@ZoneID)  extends LabelAction[Unit]
  case object GetSelectedZones extends LabelAction[Seq[Int@@ZoneID]]

  case class CreateZone(gs: Seq[GeometricRegion]) extends LabelAction[Int@@ZoneID]
  case class DeleteZone(z: Int@@ZoneID) extends LabelAction[Unit]
  case class LabelZone(z: Int@@ZoneID, l: Label) extends LabelAction[Unit]

  case class CreateFigure(
    figure: GeometricFigure,
    targetPage: PageIdentifier
  ) extends LabelAction[GeometricRegion]

  case class QueryForRegions(
    queryRegion: LTBounds,
    constraint: Constraint,
    targetPage: PageIdentifier
  ) extends LabelAction[Seq[GeometricRegion]]

  case class QueryForZones(
    queryRegion: LTBounds,
    targetPage: PageIdentifier
  ) extends LabelAction[Seq[Int@@ZoneID]]


  def createFigure(f: GeometricFigure, p: PageIdentifier)            = Free.liftF{ CreateFigure(f, p) }
  def queryForRegions(q: LTBounds, c: Constraint, t: PageIdentifier) = Free.liftF{ QueryForRegions(q, c, t) }
  def selectZone(g: Int@@ZoneID)                                            = Free.liftF{ SelectZone(g) }
  def unselectZone(g: Int@@ZoneID)                                          = Free.liftF{ UnselectZone(g) }

  // def q0: LTBounds = ???
  // def p0: PageIdentifier = ???
  // def regionAtPoint(p: Point) =
  //   for {
  //     q <- queryForRegions(q0, ByLine, p0)
  //   } yield ()



  def clickToSelectZone(zoneId: Int@@ZoneID): Interaction = InteractProg(
    for {
      _ <- selectZone(zoneId)
    } yield ()
  )

}


// type RandomReader[A] = Random => A
// val toState: RngOp ~> RandomReader =
//   new (RngOp ~> RandomReader) {
//     def apply[A](fa: RngOp[A]) =
//       fa match {
