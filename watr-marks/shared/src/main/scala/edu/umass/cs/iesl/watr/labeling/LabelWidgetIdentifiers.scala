package edu.umass.cs.iesl.watr
package labeling

import watrmarks._
import data._

import utils.Color
import utils.Colors

sealed trait WidgetMod {
  def id: Int@@WidgetID
}

object WidgetMod {

  case class Added(id: Int@@WidgetID, abs: Option[AbsPosWidget]=None) extends WidgetMod
  case class Removed(id: Int@@WidgetID) extends WidgetMod
  case class Unmodified(id: Int@@WidgetID) extends WidgetMod

}

case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  selections: Seq[Int@@ZoneID],
  currentLabeler: LabelerIdentifier
)

case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

case class UIResponse(
  uiState: UIState,
  changes: Option[List[WidgetMod]]
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


sealed trait LabelerIdentifier {
  def labelColors: Map[Label, Color]
}

case object NilLabelerIdentifier extends LabelerIdentifier {
  // TODO move label color map into UIState
  def labelColors: Map[Label, Color] = Map().withDefaultValue(Colors.Black)
}

case class DocumentLabelerIdentifier(
  stableId: String@@DocumentID,
  labelerType: String,
  pagination: Pagination,
  // TODO move label color map into UIState
  labelColors: Map[Label, Color] = Map().withDefaultValue(Colors.Black)
) extends LabelerIdentifier

case class WorkflowLabelerIdentifier(
  workflowId: String@@WorkflowID,
  // TODO move label color map into UIState
  labelColors: Map[Label, Color] = Map().withDefaultValue(Colors.Black)
) extends LabelerIdentifier

case class LabelWidgetConfig(
  workflowId: String@@WorkflowID,
  labelWidget: LabelWidget
)

trait LabelerBuilder {
  def createLabeler(): LabelWidgetConfig
}
