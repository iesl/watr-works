package edu.umass.cs.iesl.watr

import scalaz.Equal
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
sealed trait ScalingFactor

sealed trait LockGroupID
sealed trait LockID
sealed trait LabelerID
sealed trait WorkflowID

sealed trait TextReflowID
sealed trait ImageID

sealed trait UserID
sealed trait Username
sealed trait Password
sealed trait PasswordHash
sealed trait EmailAddr
sealed trait StatusCode
sealed trait ShapeID
sealed trait Role

sealed trait TokenID

sealed trait CorpusPath
sealed trait CorpusPathQuery
sealed trait AnnotationID
sealed trait LabelSchemaID
sealed trait LabelSchemaName


object TypeTags extends TypeTags


trait TypeTagUtils {
  val Tag = scalaz.Tag

  def formatTaggedType[T:ClassTag](tt: Int @@ T): String = {
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    s"${tagClsname}:${tt.unwrap}"
  }

  import scala.math.Ordering.Implicits._

  implicit def TypeTagOrdering[T]: Ordering[Int@@T] = {
    Ordering.by(_.unwrap)
  }

  implicit def TypeTagOrder[T] : scalaz.Order[Int@@T] = scalaz.Order.fromScalaOrdering

  import scalaz.syntax.equal._
  implicit def EqualTypeTag[A: Equal, T]: Equal[A@@T] =
    Equal.equal((a, b)  => a.unwrap===b.unwrap)

}
trait TypeTags extends TypeTagUtils {
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
  val ShapeID = Tag.of[ShapeID]

  val MentionID  = Tag.of[MentionID]
  val ClusterID  = Tag.of[ClusterID]
  val RelationID = Tag.of[RelationID]

  val PageNum = Tag.of[PageNum]
  val Interval = Tag.of[Interval]
  val Offset = Tag.of[Offset]
  val Length = Tag.of[Length]

  val Percent = Tag.of[Percent]
  val ScalingFactor = Tag.of[ScalingFactor]

  val ImageID = Tag.of[ImageID]

  val LockGroupID = Tag.of[LockGroupID]
  val LockID = Tag.of[LockID]
  val LabelerID = Tag.of[LabelerID]
  val WorkflowID = Tag.of[WorkflowID]

  val UserID = Tag.of[UserID]
  val EmailAddr = Tag.of[EmailAddr]
  val Username = Tag.of[Username]
  val Role = Tag.of[Role]
  val Password = Tag.of[Password]
  val PasswordHash = Tag.of[PasswordHash]

  // sealed trait StatusCode
  val StatusCode = Tag.of[StatusCode]

  val TokenID = Tag.of[TokenID]

  val CorpusPath = Tag.of[CorpusPath]
  val CorpusPathQuery = Tag.of[CorpusPathQuery]
  val AnnotationID = Tag.of[AnnotationID]
  val LabelSchemaID = Tag.of[LabelSchemaID]
  val LabelSchemaName = Tag.of[LabelSchemaName]
}

trait TypeTagCodecs {
  import _root_.io.circe
  import circe._

  implicit def Enc_IntTypeTags[T]: Encoder[Int@@T] = Encoder.encodeInt.contramap(_.unwrap)
  implicit def Enc_StringTypeTags[T]: Encoder[String@@T] = Encoder.encodeString.contramap(_.unwrap)

}
