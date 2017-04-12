package edu.umass.cs.iesl.watr
package labeling

import watrmarks._

sealed trait UIChange

case class UIAdd(widget: WidgetPositioning) extends UIChange
case class UIDel(widget: WidgetPositioning) extends UIChange

sealed trait Mods
final case class AddLw(id: Int@@WidgetID) extends Mods
final case class RmLw(id: Int@@WidgetID) extends Mods

case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  selections: Seq[Int@@ZoneID] // TODO generalize Id type (has to be able to serialize through strings)
)

case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

case class UIResponse(
  uiState: UIState,
  changes: List[UIChange]
)
