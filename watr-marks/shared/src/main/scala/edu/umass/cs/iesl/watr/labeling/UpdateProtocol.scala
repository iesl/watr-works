package edu.umass.cs.iesl.watr
package labeling

import watrmarks._

sealed trait WidgetMod
final case class AddLw(id: Int@@WidgetID, abs: Option[AbsPosWidget]=None) extends WidgetMod
final case class RmLw(id: Int@@WidgetID, abs: Option[AbsPosWidget]=None) extends WidgetMod

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
  changes: List[WidgetMod]
)

sealed trait LabelerRequest

case class DocumentLabelerRequest(
  stableId: String@@DocumentID,
  labelerType: String,
  pagination: Int
) extends LabelerRequest

// TODO request: pre-created labeler, add user id

case class LabelerResponse(
  absPosWidgets: Seq[AbsPosWidget],
  labelOption: LabelerOptions
)
