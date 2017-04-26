package edu.umass.cs.iesl.watr
package watrcolors

import TypeTags._

import labeling._

import upickle.{default => UPickle}
import upickle.Js
import UPickle._
import Aliases._


object TypeTagPicklers {

  implicit val RemoteCall_RW: RW[RemoteCall] = macroRW[RemoteCall]

  implicit val Gesture_RW: RW[Gesture] = macroRW[SelectRegion]
      .merge(macroRW[Click])
      .merge(macroRW[DblClick])
      .merge(macroRW[MenuAction])

  implicit val Interaction_RW: RW[Interaction] = RW[Interaction](
    {value => Js.Null},
    {case Js.Null => InteractNil}
  )

  implicit val String_LabelingTaskID_Pickler: RW[String @@ LabelingTaskID] = RW[String @@ LabelingTaskID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => LabelingTaskID(s.toString)}
  )

  implicit val Int_LabelerID_Pickler: RW[Int @@ LabelerID] = RW[Int @@ LabelerID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => LabelerID(s.toInt)}
  )

  implicit val String_Username_Pickler: RW[String @@ Username] = RW[String @@ Username](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => Username(s.toString)}
  )
  implicit val Int_ZoneID_Pickler: RW[Int @@ ZoneID] = RW[Int @@ ZoneID](
    {t => Js.Str(t.unwrap.toString)},
    {case Js.Str(s) => ZoneID(s.toInt)}
  )

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
