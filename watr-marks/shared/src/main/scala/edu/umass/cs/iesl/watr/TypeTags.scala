package edu.umass.cs.iesl.watr

import scalaz.Tag
import scala.reflect._


sealed trait SHA1String

sealed trait DocumentID

sealed trait ZoneID
sealed trait LabelID
sealed trait RegionID
sealed trait WidgetID

sealed trait PageID
sealed trait CharID
sealed trait ComponentID

sealed trait MentionID
sealed trait ClusterID
sealed trait RelationID

sealed trait PageNum

sealed trait Interval
sealed trait Offset
sealed trait Length

sealed trait Percent

sealed trait LabelerID
sealed trait LabelingTaskID

sealed trait TextReflowID
sealed trait ImageID

sealed trait Username
sealed trait Password


object TypeTags extends TypeTags


trait TypeTags {
  val SHA1String = Tag.of[SHA1String]

  val DocumentID = Tag.of[DocumentID]

  val ZoneID = Tag.of[ZoneID]
  val RegionID = Tag.of[RegionID]
  val WidgetID = Tag.of[WidgetID]
  val PageID = Tag.of[PageID]
  val CharID = Tag.of[CharID]
  val ComponentID = Tag.of[ComponentID]
  val LabelID = Tag.of[LabelID]
  val TextReflowID = Tag.of[TextReflowID]

  val MentionID  = Tag.of[MentionID]
  val ClusterID  = Tag.of[ClusterID]
  val RelationID = Tag.of[RelationID]

  val PageNum = Tag.of[PageNum]
  val Interval = Tag.of[Interval]
  val Offset = Tag.of[Offset]
  val Length = Tag.of[Length]

  val Percent = Tag.of[Percent]

  val ImageID = Tag.of[ImageID]

  val LabelerID = Tag.of[LabelerID]
  val LabelingTaskID = Tag.of[LabelingTaskID]

  val Username = Tag.of[Username]
  val Password = Tag.of[Password]

  def formatTaggedType[T:ClassTag](tt: Int @@ T): String = {
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    s"${tagClsname}:${tt.unwrap}"
  }


  import scala.math.Ordering.Implicits._

  implicit def TypeTagOrdering[T]: Ordering[Int@@T] = {
    Ordering.by(_.unwrap)
  }
}

object TypeTagPicklers {
  import upickle.default._, Aliases._
  import upickle.Js

  import TypeTags._


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
