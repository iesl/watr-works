package edu.umass.cs.iesl.watr
package labeling

import geometry._
import watrmarks._

sealed trait Constraint
object Constraint {
  case object ByLine extends Constraint
  case object ByChar extends Constraint
  case object ByRegion extends Constraint
}


sealed trait UIAction

case object Create extends UIAction
case object Delete extends UIAction

final case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  action: UIAction
)

sealed trait Gesture
case class SelectRegion(bbox: LTBounds) extends Gesture
case class Click(point: Point) extends Gesture

final case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

final case class UIResponse(
  changes: List[LTBounds]
)
