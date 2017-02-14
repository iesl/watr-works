package edu.umass.cs.iesl.watr

import scalaz.Tag
import scala.reflect._


sealed trait SHA1String

// Documents are identified by the SHA1 hash of their contents
sealed trait DocumentID // extends SHA1String

sealed trait ZoneID
sealed trait LabelID
sealed trait RegionID

sealed trait PageID
sealed trait CharID
sealed trait ComponentID

sealed trait MentionID
sealed trait ClusterID
sealed trait RelationID

sealed trait PageNum

sealed trait Ranging
sealed trait Offset
sealed trait Length

sealed trait Percent

sealed trait LabelerID
sealed trait LabelingTaskID

sealed trait TextReflowID
sealed trait ImageID

object TypeTags extends TypeTags


trait TypeTags {
  val SHA1String = Tag.of[SHA1String]

  val DocumentID = Tag.of[DocumentID]

  val ZoneID = Tag.of[ZoneID]
  val RegionID = Tag.of[RegionID]
  val PageID = Tag.of[PageID]
  val CharID = Tag.of[CharID]
  val ComponentID = Tag.of[ComponentID]
  val LabelID = Tag.of[LabelID]
  val TextReflowID = Tag.of[TextReflowID]

  val MentionID  = Tag.of[MentionID]
  val ClusterID  = Tag.of[ClusterID]
  val RelationID = Tag.of[RelationID]

  val PageNum = Tag.of[PageNum]
  val Ranging = Tag.of[Ranging]
  val Offset = Tag.of[Offset]
  val Length = Tag.of[Length]

  val Percent = Tag.of[Percent]

  val ImageID = Tag.of[ImageID]

  val LabelerID = Tag.of[LabelerID]
  val LabelingTaskID = Tag.of[LabelingTaskID]

  def formatTaggedType[T:ClassTag](tt: Int @@ T): String = {
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    s"${tagClsname}:${tt.unwrap}"
  }


  import scala.math.Ordering.Implicits._

  implicit def TypeTagOrdering[T]: Ordering[Int@@T] = {
    Ordering.by(_.unwrap)
  }
}
