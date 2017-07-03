package edu.umass.cs.iesl.watr
package labeling

import watrmarks._
import data._

import utils.Color
import TypeTags._

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
  currentLabeler: LabelerIdentifier,
  labelColors: Map[Label, Color] //  = Map().withDefaultValue(Colors.Black)
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
  def pagination: Pagination = {
    Pagination(0, PageNum(0), None)
  }
  def setPagination(p: Pagination): LabelerIdentifier = {
    this
  }
}

case object NilLabelerIdentifier extends LabelerIdentifier

case class DocumentLabelerIdentifier(
  stableId: String@@DocumentID,
  labelerType: String,
  override val pagination: Pagination
) extends LabelerIdentifier {
  override def setPagination(p: Pagination): LabelerIdentifier = {
    copy(pagination = p)
  }

}

case class WorkflowLabelerIdentifier(
  workflowId: String@@WorkflowID
) extends LabelerIdentifier

case class LabelWidgetConfig(
  workflowId: String@@WorkflowID,
  labelWidget: LabelWidget
)

trait LabelerBuilder {
  def createLabeler(userId: Int@@UserID): LabelWidgetConfig
  def targetLabels(): Seq[(Label, Color)]
}
