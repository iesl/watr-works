package edu.umass.cs.iesl.watr
package labeling

import watrmarks._

import utils.Color
import utils.Colors

sealed trait WidgetMod
final case class AddLw(id: Int@@WidgetID, abs: Option[AbsPosWidget]=None) extends WidgetMod
final case class RmLw(id: Int@@WidgetID, abs: Option[AbsPosWidget]=None) extends WidgetMod
final case object ClearAllLw extends WidgetMod

case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  selections: Seq[Int@@ZoneID], // TODO generalize Id type (has to be able to serialize through strings)
  currentLabeler: LabelerIdentifier,
  pagination: Pagination
)

case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

case class UIResponse(
  uiState: UIState,
  changes: List[WidgetMod]
)

case class PaginationInfo(
  pageNum: Int@@PageNum,
  labelCount: Int
)

case class Pagination(
  pageCount: Int,
  currentPage: Int@@PageNum,
  pageInfo: Option[Seq[PaginationInfo]]
)
sealed trait LabelerIdentifier

case class DocumentLabelerIdentifier(
  stableId: String@@DocumentID,
  labelerType: String,
  pagination: Pagination
) extends LabelerIdentifier

// TODO request: pre-created labeler, add user id
case class LabelerOptions(
  pagination: Pagination,
  colorMap: Map[Label, Color] = Map().withDefaultValue(Colors.Black)
)

case class LabelingPanel(
  labelWidget: LabelWidgetF.LabelWidget,
  labelerOptions: LabelerOptions
)

case class LabelerResponse(
  absPosWidgets: Seq[AbsPosWidget],
  labelerOptions: LabelerOptions
)



