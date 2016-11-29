package edu.umass.cs.iesl.watr

import scalaz.Tag
import scalaz.@@

sealed trait ZoneID
sealed trait LabelID
sealed trait RegionID
sealed trait TokenID

sealed trait PageID
sealed trait CharID
sealed trait ComponentID

sealed trait MentionID
sealed trait ClusterID
sealed trait RelationID

sealed trait SHA1String

sealed trait Offset
sealed trait Length

sealed trait Percent

// Maybe someday:
// sealed trait StringCase
// sealed trait SnakeCase extends StringCase
// sealed trait CamelCase extends StringCase
// sealed trait SnakeUScoreCase extends StringCase
// val SnakeCase = Tag.of[SnakeCase]
// val SnakeUScoreCase = Tag.of[SnakeUScoreCase]
// val CamelCase = Tag.of[CamelCase]

object TypeTags {
  val SHA1String = Tag.of[SHA1String]

  val ZoneID = Tag.of[ZoneID]
  val RegionID = Tag.of[RegionID]
  val TokenID = Tag.of[TokenID]
  val PageID = Tag.of[PageID]
  val CharID = Tag.of[CharID]
  val ComponentID = Tag.of[ComponentID]
  val LabelID = Tag.of[LabelID]

  val MentionID  = Tag.of[MentionID]
  val ClusterID  = Tag.of[ClusterID]
  val RelationID = Tag.of[RelationID]


  val Offset = Tag.of[Offset]
  val Length = Tag.of[Length]

  val Percent = Tag.of[Percent]

  implicit class TagOps[A, T](val value: A@@T) extends AnyVal {
    def unwrap: A = Tag.of[T].unwrap(value)


  }

  import scala.reflect._

  def formatTaggedType[T:ClassTag](tt: Int @@ T): String = {
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    s"${tagClsname}:${tt.unwrap}"
  }



}

import TypeTags._

trait ExplicitTypeTagFormats {
  import play.api.libs.json
  import json._
  import scala.reflect._


  def WriteTaggedInt[T: ClassTag]: Writes[Int @@ T] = Writes[Int @@ T] { tagged =>
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    JsString(s"${tagClsname}:${tagged.unwrap}")
  }

  def ReadTaggedInt[T: ClassTag]: Reads[Int @@ T] = {
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName

    __.read[String].map({str =>
      val Array(id, tagType) = str.split(":")
      val tagged = tagType match {
        case `tagClsname` => Tag.of[T](id.toInt)
        case _ => sys.error("")
      }
      tagged.asInstanceOf[Int @@ T]
    })
  }

  implicit def FormatTaggedInt[T: ClassTag]: Format[Int @@ T] = Format(
    ReadTaggedInt[T], WriteTaggedInt[T]
  )
}

trait TypeTagFormats {
  import play.api.libs.json
  import json._


  val ReadPageID: Reads[Int@@PageID]   = __.read[Int].map(i => Tag.of[PageID](i))
  val WritePageID: Writes[Int@@PageID] = Writes[Int@@PageID] { i => JsNumber(i.unwrap) }
  implicit def FormatPageID            = Format(ReadPageID, WritePageID)

  val ReadZoneID: Reads[Int@@ZoneID]   = __.read[Int].map(i => Tag.of[ZoneID](i))
  val WriteZoneID: Writes[Int@@ZoneID] = Writes[Int@@ZoneID] { i => JsNumber(i.unwrap) }
  implicit def FormatZoneID            = Format(ReadZoneID, WriteZoneID)

  val ReadRegionID: Reads[Int@@RegionID]   = __.read[Int].map(i => Tag.of[RegionID](i))
  val WriteRegionID: Writes[Int@@RegionID] = Writes[Int@@RegionID] { i => JsNumber(i.unwrap) }
  implicit def FormatRegionID            = Format(ReadRegionID, WriteRegionID)

  val ReadTokenID: Reads[Int@@TokenID]   = __.read[Int].map(i => Tag.of[TokenID](i))
  val WriteTokenID: Writes[Int@@TokenID] = Writes[Int@@TokenID] { i => JsNumber(i.unwrap) }
  implicit def FormatTokenID            = Format(ReadTokenID, WriteTokenID)

  val ReadCharID: Reads[Int@@CharID]   = __.read[Int].map(i => Tag.of[CharID](i))
  val WriteCharID: Writes[Int@@CharID] = Writes[Int@@CharID] { i => JsNumber(i.unwrap) }
  implicit def FormatCharID            = Format(ReadCharID, WriteCharID)

  val ReadLabelID: Reads[Int@@LabelID]   = __.read[Int].map(i => Tag.of[LabelID](i))
  val WriteLabelID: Writes[Int@@LabelID] = Writes[Int@@LabelID] { i => JsNumber(i.unwrap) }
  implicit def FormatLabelID            = Format(ReadLabelID, WriteLabelID)

  val ReadLength: Reads[Int@@Length]   = __.read[Int].map(i => Tag.of[Length](i))
  val WriteLength: Writes[Int@@Length] = Writes[Int@@Length] { i => JsNumber(i.unwrap) }
  implicit def FormatLength            = Format(ReadLength, WriteLength)

  val ReadOffset: Reads[Int@@Offset]   = __.read[Int].map(i => Tag.of[Offset](i))
  val WriteOffset: Writes[Int@@Offset] = Writes[Int@@Offset] { i => JsNumber(i.unwrap) }
  implicit def FormatOffset            = Format(ReadOffset, WriteOffset)

  val ReadClusterID: Reads[Int@@ClusterID]   = __.read[Int].map(i => Tag.of[ClusterID](i))
  val WriteClusterID: Writes[Int@@ClusterID] = Writes[Int@@ClusterID] { i => JsNumber(i.unwrap) }
  implicit def FormatClusterID            = Format(ReadClusterID, WriteClusterID)

  val ReadMentionID: Reads[Int@@MentionID]   = __.read[Int].map(i => Tag.of[MentionID](i))
  val WriteMentionID: Writes[Int@@MentionID] = Writes[Int@@MentionID] { i => JsNumber(i.unwrap) }
  implicit def FormatMentionID            = Format(ReadMentionID, WriteMentionID)

  val ReadComponentID: Reads[Int@@ComponentID]   = __.read[Int].map(i => Tag.of[ComponentID](i))
  val WriteComponentID: Writes[Int@@ComponentID] = Writes[Int@@ComponentID] { i => JsNumber(i.unwrap) }
  implicit def FormatComponentID            = Format(ReadComponentID, WriteComponentID)

  val ReadChar: Reads[Char]   = __.read[String].map(c => c(0))
  val WriteChar: Writes[Char] = Writes[Char] { c => JsString(c.toString()) }
  implicit def FormatChar     = Format(ReadChar, WriteChar)


  implicit def FormatSHA1String  = Format(
    __.read[String].map(i => Tag.of[SHA1String](i)),
    Writes[String@@SHA1String](i => JsString(i.unwrap))
  )
}
