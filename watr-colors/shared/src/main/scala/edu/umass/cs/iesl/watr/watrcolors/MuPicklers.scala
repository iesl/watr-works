package edu.umass.cs.iesl.watr
package watrcolors

import TypeTags._

import watrmarks._
import labeling._
import geometry._

import upickle.{default => UPickle}
import upickle.Js
import UPickle._
import Aliases._

sealed trait Gesture
case class SelectRegion(bbox: LTBounds) extends Gesture
case class Click(point: Point) extends Gesture
case class DblClick(point: Point) extends Gesture

object Gesture {
  implicit val readWriter: RW[Gesture] =
    macroRW[SelectRegion].merge(macroRW[Click])
}

final case class UIState(
  selectionConstraint: Constraint,
  selectedLabel: Option[Label],
  action: UIAction
)

case class UIRequest(
  uiState: UIState,
  gesture: Gesture
)

case class UIResponse(
  changes: List[UIChange]
)

object TypeTagPicklers {

  implicit val GeometricGroup_RW: RW[GeometricGroup] =
    macroRW[GeometricGroup]

  implicit val UIRequest_RW: RW[UIRequest] =
    macroRW[UIRequest]

  implicit val UIResponse_RW: RW[UIResponse] =
    macroRW[UIResponse]

  implicit val Int_RegionID_Pickler: RW[Int @@ RegionID] = RW[Int @@ RegionID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => RegionID(s.toInt)}
  )

  implicit val Int_CharID_Pickler: RW[Int @@ CharID] = RW[Int @@ CharID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => CharID(s.toInt)}
  )
  implicit val Int_PageID_Pickler: RW[Int @@ PageID] = RW[Int @@ PageID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => PageID(s.toInt)}
  )

  implicit val Int_PageNum_Pickler: RW[Int @@ PageNum] = RW[Int @@ PageNum](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => PageNum(s.toInt)}
  )
  implicit val Int_LabelID_Pickler: RW[Int @@ LabelID] = RW[Int @@ LabelID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => LabelID(s.toInt)}
  )
  implicit val Int_DocumentID_Pickler: RW[String @@ DocumentID] = RW[String @@ DocumentID](
    {t => Js.Str(t.unwrap)},
    {case Js.Str(s) => DocumentID(s)}
  )
  implicit val Int_WidgetID_Pickler: RW[Int @@ WidgetID] = RW[Int @@ WidgetID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => WidgetID(s.toInt)}
  )

}
