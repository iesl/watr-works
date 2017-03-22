package edu.umass.cs.iesl.watr
package labeling

import watrmarks._

sealed trait UIChange

case class UIAdd(widget: WidgetPositioning) extends UIChange
case class UIDel(widget: WidgetPositioning) extends UIChange

case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  selections: Seq[Int@@ZoneID]
)

case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

case class UIResponse(
  uiState: UIState,
  changes: List[UIChange]
)
