package org.watrworks

import scalaz.Equal
import scalaz.Need
import scala.reflect._


sealed trait SHA1String

sealed trait DocumentID

sealed trait ZoneID
sealed trait LabelID
sealed trait StanzaID
sealed trait RegionID
sealed trait WidgetID

sealed trait PageID
sealed trait CharID
sealed trait GlyphID
sealed trait ComponentID

sealed trait MentionID
sealed trait ClusterID
sealed trait RelationID

sealed trait PageNum

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
  import textboxing.{TextBoxing => TB, ShowBox}, TB._
  val Tag = scalaz.Tag

  implicit def NeedShowBox[A](sh: ShowBox[A]): Need[ShowBox[A]] = {
    Need { sh }
  }

  implicit def ShowBoxInt[T: ClassTag](tt: Int @@ T): ShowBox[Int @@ T] = {

    ShowBox.show{ t =>
      val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
      s"${tagClsname}:${tt.unwrap}".box
    }
  }

  implicit def ShowBoxStr[T: ClassTag](tt: String @@ T): ShowBox[String @@ T]= {
    ShowBox.show{ t =>
      val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
      s"${tagClsname}:${tt.unwrap}".box
    }
  }

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
  val GlyphID = Tag.of[GlyphID]
  val ComponentID = Tag.of[ComponentID]
  val LabelID = Tag.of[LabelID]
  val StanzaID = Tag.of[StanzaID]
  val TextReflowID = Tag.of[TextReflowID]
  val ShapeID = Tag.of[ShapeID]

  val MentionID  = Tag.of[MentionID]
  val ClusterID  = Tag.of[ClusterID]
  val RelationID = Tag.of[RelationID]

  val PageNum = Tag.of[PageNum]
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

  def intEncoder[A]: Encoder[Int@@A] = {
    val e: Encoder[Int@@A] = Encoder.encodeInt.contramap(_.unwrap)
    e
  }
  def intDecoder[A]: Decoder[Int@@A] = {
    val T = Tag.of[A]
    val e: Decoder[Int@@A] = Decoder.decodeInt.map(T(_))
    e
  }
  def strEncoder[A]: Encoder[String@@A] = {
    val e: Encoder[String@@A] = Encoder.encodeString.contramap(_.unwrap)
    e
  }
  def strDecoder[A]: Decoder[String@@A] = {
    val T = Tag.of[A]
    val e: Decoder[String@@A] = Decoder.decodeString.map(T(_))
    e
  }
  def strCodec[A]: Codec[String@@A] = {
    Codec.from(
      strDecoder[A],
      strEncoder[A]
    )
  }
  def intCodec[A]: Codec[Int@@A] = {
    Codec.from(
      intDecoder[A],
      intEncoder[A]
    )
  }


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

  implicit val Enc_Int_CharID: Encoder[Int@@CharID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_CharID: Decoder[Int@@CharID] = Decoder.decodeInt.map(CharID(_))

  implicit val Enc_Int_GlyphID: Encoder[Int@@GlyphID] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_GlyphID: Decoder[Int@@GlyphID] = Decoder.decodeInt.map(GlyphID(_))

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


  // implicit val Enc_XX: Encoder[XX] = deriveEncoder
  // implicit val Enc_Label: Encoder[Label] = Encoder.encodeString.contramap(_.fqn)
  // implicit val Dec_Label: Decoder[Label] = Decoder.decodeString.map(Label(_))

}

object TypeTagCodecs extends TypeTagCodecs
