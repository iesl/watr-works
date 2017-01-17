package edu.umass.cs.iesl.watr
package geometry 

import watrmarks._
import scalaz.Tag



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
  import Json._
  import utils.EnrichNumerics._
  def jstr(s: String) = JsString(s)

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

  implicit def FormatLBBounds         = Json.format[LBBounds]

  implicit def FormatLTBounds: Format[LTBounds] = new Format[LTBounds] {
    override def reads(json: JsValue) = json match {
      // case JsArray(Seq(JsString("lt"), JsString(bounds))) =>
      case JsString(bounds) =>
        val Array(l, t, w, h) = bounds.trim.split(" ").map(_.toDouble)
        JsSuccess(LTBounds(l, t, w, h))

      case _ => JsError("LTBounds")
    }

    override def writes(o: LTBounds) = {
      // arr(jstr("lt"), jstr(s"""${o.left.pp} ${o.top.pp} ${o.width.pp} ${o.height.pp}"""))
      jstr(s"""${o.left.pp} ${o.top.pp} ${o.width.pp} ${o.height.pp}""")
    }
  }

  implicit def FormatTargetRegion: Format[TargetRegion] = new Format[TargetRegion] {
    override def reads(json: JsValue)= json match {
      case JsArray(Seq(
        id: JsNumber, JsString(docId), targetPage: JsNumber, ltBounds
      )) => JsSuccess(
        TargetRegion(
          id.as[Int@@RegionID],
          DocumentID(docId),
          targetPage.as[Int@@PageID],
          ltBounds.as[LTBounds]
        ))
    }
    override def writes(o: TargetRegion) =
      arr(toJson(o.id), toJson(o.docId.unwrap), toJson(o.pageId), toJson(o.bbox))
  }

  implicit def FormatPageGeometry     = Json.format[PageGeometry]
  implicit def FormatLabel            = Json.format[Label]
  implicit def FormatZone             = Json.format[Zone]

  implicit def FormatCharAtom: Format[CharAtom] = new Format[CharAtom] {
    override def reads(json: JsValue)= json match {
      case JsObject(fields) => fields.get("c") match {
        case Some(JsArray(Seq(JsString(achar), targetRegionJs, JsNumber(wonkyCode)))) =>
          JsSuccess(
            CharAtom(targetRegionJs.as[TargetRegion], achar, Option(wonkyCode.toInt))
          )
        case Some(JsArray(Seq(JsString(achar), targetRegionJs))) =>
          JsSuccess(
            CharAtom(targetRegionJs.as[TargetRegion], achar, None)
          )
      }
      case _ => JsError(s"unmatched CharAtom ${json}")
    }

    override def writes(o: CharAtom) = o match {
      case a: CharAtom =>
        a.wonkyCharCode.map(ccode =>
          obj(("c", arr(jstr(a.char), toJson(a.targetRegion), JsNumber(ccode))))
        ).getOrElse(
          obj(("c", arr(jstr(a.char), toJson(a.targetRegion))))
        )
    }
  }
}
