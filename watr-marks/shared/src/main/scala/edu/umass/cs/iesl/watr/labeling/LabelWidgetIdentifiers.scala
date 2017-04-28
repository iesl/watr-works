package edu.umass.cs.iesl.watr
package labeling

import watrmarks._

import utils.Color
import utils.Colors

sealed trait WidgetMod

object WidgetMod {
  import upickle.default._, Aliases._
  import AbsPosWidget._

  case class AddLw(id: Int@@WidgetID, abs: Option[AbsPosWidget]=None) extends WidgetMod
  case class RmLw(id: Int@@WidgetID, abs: Option[AbsPosWidget]=None) extends WidgetMod
  case class ClearAllLw() extends WidgetMod

  implicit val Int_WidgetID_Pickler: RW[Int@@WidgetID] = TypeTagPicklers.Int_WidgetID_Pickler


  implicit val WidgetMod_RW: RW[WidgetMod] =
    macroRW[AddLw]
      .merge(macroRW[RmLw])
      .merge(macroRW[ClearAllLw])

}


case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  selections: Seq[Int@@ZoneID], // TODO generalize Id type (has to be able to serialize through strings)
  currentLabeler: LabelerIdentifier
)

object UIState {
  import upickle.default._, Aliases._
  import TypeTagPicklers._
  implicit val UIState_RW: RW[UIState] = macroRW[UIState]

}
case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

object UIRequest {
  import upickle.default._, Aliases._
  import TypeTagPicklers._

  // implicit val XX_RW: RW[XX] = macroRW[XX]
  implicit val UIRequest_RW: RW[UIRequest] = macroRW[UIRequest]

}

case class UIResponse(
  uiState: UIState,
  changes: List[WidgetMod]
)
object UIResponse {
  import upickle.default._, Aliases._
  import TypeTagPicklers._
  import UIRequest._
  import UIState._
  import WidgetMod._

  implicit val UIResponse_RW: RW[UIResponse] = macroRW[UIResponse]

}

case class PaginationInfo(
  pageNum: Int@@PageNum,
  labelCount: Int
)
object PaginationInfo {

  import upickle.default._, Aliases._
  import TypeTagPicklers._
  // implicit val XX_RW: RW[XX] = macroRW[XX]

  implicit val PaginationInfo_RW: RW[PaginationInfo] = macroRW[PaginationInfo]
}

case class Pagination(
  pageCount: Int,
  currentPage: Int@@PageNum,
  pageInfo: Option[Seq[PaginationInfo]]
)

object Pagination {
  import upickle.default._, Aliases._
  import TypeTagPicklers._
  // implicit val XX_RW: RW[XX] = macroRW[XX]

  implicit val Pagination_RW: RW[Pagination] = macroRW[Pagination]

}
sealed trait LabelerIdentifier {
  def labelColors: Map[Label, Color]
}

case object NilLabelerIdentifier extends LabelerIdentifier {
  def labelColors: Map[Label, Color] = Map().withDefaultValue(Colors.Black)
}

case class DocumentLabelerIdentifier(
  stableId: String@@DocumentID,
  labelerType: String,
  pagination: Pagination,
  labelColors: Map[Label, Color] = Map().withDefaultValue(Colors.Black)
) extends LabelerIdentifier


object LabelerIdentifier {
  import upickle.default._, Aliases._
  import TypeTagPicklers._

  implicit val LabelerIdentifier_RW: RW[LabelerIdentifier] =
    macroRW[DocumentLabelerIdentifier]
      .merge(macroRW[NilLabelerIdentifier.type])

  // implicit val XX_RW: RW[XX] = macroRW[XX]

}



// case class LabelerOptions(
//   pagination: Pagination,
//   colorMap: Map[Label, Color] = Map().withDefaultValue(Colors.Black)
// )

// case class LabelingPanel(
//   labelWidget: LabelWidgetF.LabelWidget,
//   labelerOptions: LabelerOptions
// )
