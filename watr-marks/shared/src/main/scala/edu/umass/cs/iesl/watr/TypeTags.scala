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
sealed trait ScaledFontID

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

  implicit def TypeTagOrderingInt[T]: Ordering[Int@@T] = {
    Ordering.by(_.unwrap)
  }

  implicit def TypeTagOrderingStr[T]: Ordering[String@@T] = {
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
  val ScaledFontID = Tag.of[ScaledFontID]

  val ImageID = Tag.of[ImageID]

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
  import TypeTags._
  import _root_.io.circe
  import circe._

  // import cats._

  implicit def Enc_IntTypeTags[T]: Encoder[Int@@T] = Encoder.encodeInt.contramap(_.unwrap)
  implicit def Enc_StringTypeTags[T]: Encoder[String@@T] = Encoder.encodeString.contramap(_.unwrap)

  implicit val Enc_Str_DocumentID: Encoder[String@@DocumentID] = Encoder.encodeString.contramap(_.unwrap)
  implicit val Dec_Str_DocumentID: Decoder[String@@DocumentID] = Decoder.decodeString.map(DocumentID(_))

  implicit val Enc_Int_DocumentID: Encoder[Int@@DocumentID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_DocumentID: Decoder[Int@@DocumentID] = Decoder.decodeInt.map(DocumentID(_))

  implicit val Enc_Int_PageID: Encoder[Int@@PageID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_PageID: Decoder[Int@@PageID] = Decoder.decodeInt.map(PageID(_))

  implicit val Enc_Int_PageNum: Encoder[Int@@PageNum] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_PageNum: Decoder[Int@@PageNum] = Decoder.decodeInt.map(PageNum(_))

  implicit val Enc_Int_UserID: Encoder[Int@@UserID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_UserID: Decoder[Int@@UserID] = Decoder.decodeInt.map(UserID(_))

  implicit val Enc_String_EmailAddr: Encoder[String@@EmailAddr] = Encoder.encodeString.contramap(_.unwrap)
  implicit val Dec_String_EmailAddr: Decoder[String@@EmailAddr] = Decoder.decodeString.map(EmailAddr(_))

  implicit val Enc_String_WorkflowID: Encoder[String@@WorkflowID] = Encoder.encodeString.contramap(_.unwrap)
  implicit val Dec_String_WorkflowID: Decoder[String@@WorkflowID] = Decoder.decodeString.map(WorkflowID(_))

  implicit val Enc_String_CorpusPath: Encoder[String@@CorpusPath] = Encoder.encodeString.contramap(_.unwrap)
  implicit val Dec_String_CorpusPath: Decoder[String@@CorpusPath] = Decoder.decodeString.map(CorpusPath(_))

  implicit val Enc_String_StatusCode: Encoder[String@@StatusCode] = Encoder.encodeString.contramap(_.unwrap)
  implicit val Dec_String_StatusCode: Decoder[String@@StatusCode] = Decoder.decodeString.map(StatusCode(_))


  implicit val Enc_Int_LockID: Encoder[Int@@LockID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_LockID: Decoder[Int@@LockID] = Decoder.decodeInt.map(LockID(_))

  implicit val Enc_Int_AnnotationID: Encoder[Int@@AnnotationID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_AnnotationID: Decoder[Int@@AnnotationID] = Decoder.decodeInt.map(AnnotationID(_))

  // implicit val Dec_IntTypeTag[T]: Decoder[Int@@T] = Decoder.decodeInt.map(T)

  // implicit val Enc_XX: Encoder[XX] = deriveEncoder
  // implicit val Enc_Label: Encoder[Label] = Encoder.encodeString.contramap(_.fqn)
  // implicit val Dec_Label: Decoder[Label] = Decoder.decodeString.map(Label(_))

}

object TypeTagCodecs extends TypeTagCodecs
