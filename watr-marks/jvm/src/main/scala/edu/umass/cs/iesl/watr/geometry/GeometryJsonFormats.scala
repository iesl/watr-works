package edu.umass.cs.iesl.watr
package geometry

import watrmarks._
import scalaz.Tag

// import TypeTags._


trait TypeTagFormats {
  import play.api.libs.json
  import json._

  val ReadDocumentIDI: Reads[Int@@DocumentID]   = __.read[Int].map(i => Tag.of[DocumentID](i))
  val WriteDocumentIDI: Writes[Int@@DocumentID] = Writes[Int@@DocumentID] { i => JsNumber(i.unwrap) }
  implicit def FormatDocumentIDI            = Format(ReadDocumentIDI, WriteDocumentIDI)

  val ReadDocumentID: Reads[String@@DocumentID]   = __.read[String].map(i => Tag.of[DocumentID](i))
  val WriteDocumentID: Writes[String@@DocumentID] = Writes[String@@DocumentID] { i => JsString(i.unwrap) }
  implicit def FormatDocumentID            = Format(ReadDocumentID, WriteDocumentID)

  val ReadPageID: Reads[Int@@PageID]   = __.read[Int].map(i => Tag.of[PageID](i))
  val WritePageID: Writes[Int@@PageID] = Writes[Int@@PageID] { i => JsNumber(i.unwrap) }
  implicit def FormatPageID            = Format(ReadPageID, WritePageID)

  val ReadPageNum: Reads[Int@@PageNum]   = __.read[Int].map(i => Tag.of[PageNum](i))
  val WritePageNum: Writes[Int@@PageNum] = Writes[Int@@PageNum] { i => JsNumber(i.unwrap) }
  implicit def FormatPageNum            = Format(ReadPageNum, WritePageNum)

  val ReadZoneID: Reads[Int@@ZoneID]   = __.read[Int].map(i => Tag.of[ZoneID](i))
  val WriteZoneID: Writes[Int@@ZoneID] = Writes[Int@@ZoneID] { i => JsNumber(i.unwrap) }
  implicit def FormatZoneID            = Format(ReadZoneID, WriteZoneID)

  val ReadRegionID: Reads[Int@@RegionID]   = __.read[Int].map(i => Tag.of[RegionID](i))
  val WriteRegionID: Writes[Int@@RegionID] = Writes[Int@@RegionID] { i => JsNumber(i.unwrap) }
  implicit def FormatRegionID            = Format(ReadRegionID, WriteRegionID)

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

  val ReadFloatRep: Reads[Int@@FloatRep]   = __.read[Int].map(i => Tag.of[FloatRep](i))
  val WriteFloatRep: Writes[Int@@FloatRep] = Writes[Int@@FloatRep] { i => JsNumber(i.unwrap) }
  implicit def FormatFloatRep            = Format(ReadFloatRep, WriteFloatRep)

  val ReadChar: Reads[Char]   = __.read[String].map(c => c(0))
  val WriteChar: Writes[Char] = Writes[Char] { c => JsString(c.toString()) }
  implicit def FormatChar     = Format(ReadChar, WriteChar)


  // implicit def FormatSHA1String  = Format(
  //   __.read[String].map(i => Tag.of[SHA1String](i)),
  //   Writes[String@@SHA1String](i => JsString(i.unwrap))
  // )
}


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

trait GeometryJsonCodecs extends TypeTagFormats {
  import play.api.libs.json._
  // import play.api.libs.json
  // import utils.EnrichNumerics._
  def jstr(s: String) = JsString(s)
  def num(s: Int) = JsNumber(s)

  implicit def optionalFormat[T](implicit jsFmt: Format[T]): Format[Option[T]] =
    new Format[Option[T]] {
      override def reads(json: JsValue): JsResult[Option[T]] = json match {
        case JsNull => JsSuccess(None)
        case js     => jsFmt.reads(js).map(Some(_))
      }
      override def writes(o: Option[T]): JsValue = o match {
        case None    => JsNull
        case Some(t) => jsFmt.writes(t)
      }
    }

  implicit val FormatLBBounds         = Json.format[LBBounds]

  implicit val FormatLTBounds: Format[LTBounds] = new Format[LTBounds] {
    override def reads(json: JsValue) = json match {
      case JsArray(Seq(
        JsNumber(l), JsNumber(t), JsNumber(w), JsNumber(h)
      )) =>
        JsSuccess(LTBounds.IntReps(l.intValue(), t.intValue(), w.intValue(), h.intValue()))

      case _ => JsError("LTBounds")
    }

    override def writes(o: LTBounds) = {
      val LTBounds.IntReps(l, t, w, h) = o
      Json.arr(num(l), num(t), num(w), num(h))
    }
  }

  implicit val FormatStablePageID: Format[StablePageID] = Json.format[StablePageID]
  implicit val FormatRecordedPageID: Format[RecordedPageID] = Json.format[RecordedPageID]
  implicit val FormatTargetRegion: Format[TargetRegion] = Json.format[TargetRegion]
  implicit val FormatPageRegion: Format[PageRegion] = Json.format[PageRegion]

  implicit val FormatPageGeometry     = Json.format[PageGeometry]
  implicit val FormatLabel            = Json.format[Label]
  // implicit val FormatZone             = Json.format[Zone]
  implicit val FormatCharAtom             = Json.format[CharAtom]

  // implicit val FormatCharAtom: Format[CharAtom] = new Format[CharAtom] {
  //   override def reads(json: JsValue)= json match {
  //     case JsObject(fields) => fields.get("c") match {
  //       case Some(JsArray(Seq(JsNumber(charId), JsString(achar), targetRegionJs, JsNumber(wonkyCode)))) =>
  //         JsSuccess(
  //           CharAtom(CharID(charId.intValue()), targetRegionJs.as[TargetRegion], achar, Option(wonkyCode.toInt))
  //         )

  //       case Some(JsArray(Seq(JsNumber(charId), JsString(achar), targetRegionJs))) =>
  //         JsSuccess(
  //           CharAtom(CharID(charId.intValue()), targetRegionJs.as[TargetRegion], achar, None)
  //         )
  //     }
  //     case _ => JsError(s"unmatched CharAtom ${json}")
  //   }

  //   override def writes(o: CharAtom) = o match {
  //     case a: CharAtom =>
  //       a.wonkyCharCode.map(ccode =>
  //         obj(("c", arr(JsNumber(a.id.unwrap), jstr(a.char), toJson(a.targetRegion), JsNumber(ccode))))
  //       ).getOrElse(
  //         obj(("c",
  //           arr(
  //             JsNumber(a.id.unwrap),
  //             jstr(a.char),
  //             toJson(a.targetRegion)
  //           )))
  //       )
  //   }
  // }
}
