package edu.umass.cs.iesl.watr
package labeling

import geometry._
import watrmarks._

sealed trait Constraint
case object ByLine extends Constraint
case object ByChar extends Constraint
case object ByRegion extends Constraint

sealed trait Gesture
case class SelectRegion(bbox: LTBounds) extends Gesture
case class Click(point: Point) extends Gesture

sealed trait UIAction

case object Create extends UIAction
case object Delete extends UIAction

case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  action: UIAction
)

case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

case class UIResponse(
  changes: Seq[(UIAction, LTBounds)]
)
