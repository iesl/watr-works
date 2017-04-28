package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import utils._
import labeling._
import labeling.data._
import watrmarks._
import textreflow._

object UPicklers {
  import upickle.default._, Aliases._
  import TypeTagPicklers._
  import TextReflowF._
  import LabelWidgetF._
  import WidgetMod._
  import Interaction._
  import LabelAction._


  implicit val Label_RW: RW[Label] = macroRW[Label]
  implicit val StablePageID_RW: RW[StablePageID] = macroRW[StablePageID]
  implicit val RecordedPageID_RW: RW[RecordedPageID] = macroRW[RecordedPageID]
  implicit val PageRegion_RW: RW[PageRegion] = macroRW[PageRegion]
  implicit val TargetRegion_RW: RW[TargetRegion] = macroRW[TargetRegion]
  implicit val PageGeometry_RW: RW[PageGeometry] = macroRW[PageGeometry]
  implicit val CharAtom_RW: RW[CharAtom] = macroRW[CharAtom]
  implicit val Zone_RW: RW[Zone] = macroRW[Zone]

  implicit val GeometricFigure_RW: RW[GeometricFigure] =
    macroRW[LTBounds]
      .merge(macroRW[LBBounds])
      .merge(macroRW[Point])
      .merge(macroRW[Line])
      .merge(macroRW[GeometricGroup])
      .merge(macroRW[Colorized])

  implicit val Padding_RW: RW[Padding] =
    macroRW[Padding]


  // implicit val Int_WidgetID_Pickler: RW[Int@@WidgetID] = TypeTagPicklers.Int_WidgetID_Pickler


  implicit val WidgetMod_RW: RW[WidgetMod] =
    macroRW[AddLw]
      .merge(macroRW[RmLw])
      .merge(macroRW[ClearAllLw])

  implicit val UIState_RW: RW[UIState] = macroRW[UIState]
  implicit val UIRequest_RW: RW[UIRequest] = macroRW[UIRequest]
  implicit val UIResponse_RW: RW[UIResponse] = macroRW[UIResponse]
  implicit val PaginationInfo_RW: RW[PaginationInfo] = macroRW[PaginationInfo]
  implicit val Pagination_RW: RW[Pagination] = macroRW[Pagination]

  implicit val LabelerIdentifier_RW: RW[LabelerIdentifier] =
    macroRW[DocumentLabelerIdentifier]
      .merge(macroRW[NilLabelerIdentifier.type])

  implicit val TextReflowFU_RW: RW[TextReflowF[Unit]] =
    macroRW[Atom]
      .merge(macroRW[Insert])
      .merge(macroRW[Rewrite[Unit]])
      .merge(macroRW[Bracket[Unit]])
      .merge(macroRW[Flow[Unit]])
      .merge(macroRW[Labeled[Unit]])

  // implicit val LabelWidgetFN_RW: RW[LabelWidgetF[Nothing]] =
  //   macroRW[LabelWidgetF[Nothing]]
  //     .merge(macroRW[LBBounds])

  // implicit val InteractionImplicit = Interaction.Interaction_RW

  implicit val LabelWidgetFU_RW: RW[LabelWidgetF[Unit]] =
    macroRW[RegionOverlay[Unit]]
      .merge(macroRW[Row[Unit]])
      .merge(macroRW[Col[Unit]])
      .merge(macroRW[Pad[Unit]])
      .merge(macroRW[TextBox])
      .merge(macroRW[Reflow])
      .merge(macroRW[Figure])
      .merge(macroRW[Identified[Unit]])
      .merge(macroRW[Panel[Unit]])
      .merge(macroRW[Terminal.type])


  import upickle.Js

  implicit val Constraint_RW: RW[Constraint] = macroRW[Constraint]


  implicit def Interaction_RW: RW[Interaction] = RW[Interaction](
    {value => Js.Null},
    {case Js.Null => InteractNil}
  )

  implicit val LabelAction_RW: RW[LabelAction[Unit]] =
    macroRW[SelectZone]
      .merge(macroRW[ToggleZoneSelection])
      .merge(macroRW[DeleteZone])
      .merge(macroRW[MergeZones])
      .merge(macroRW[NavigateTo])


  implicit val Gesture_RW: RW[Gesture] =
    macroRW[SelectRegion]
      .merge(macroRW[Click])
      .merge(macroRW[DblClick])
      .merge(macroRW[MenuAction])


  implicit val Color_RW: RW[Color] =
    macroRW[RGBColor ]
      .merge(macroRW[CMYKColor])
      .merge(macroRW[HSVColor ])
      .merge(macroRW[HSLColor ])
      .merge(macroRW[Grayscale])

  implicit val AbsPosWidget_RW: RW[AbsPosWidget] = macroRW[AbsPosWidget]

  implicit val LabelerEntry_RW: RW[LabelerEntry] = macroRW[LabelerEntry]
  implicit val DocumentEntry_RW: RW[DocumentEntry] = macroRW[DocumentEntry]
  implicit val RemoteCall_RW: RW[RemoteCall] = macroRW[RemoteCall]
}

