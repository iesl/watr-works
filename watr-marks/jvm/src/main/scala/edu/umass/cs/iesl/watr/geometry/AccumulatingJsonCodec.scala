package edu.umass.cs.iesl.watr
package geometry

import watrmarks._
import play.api.libs.json._
// import TypeTags._

case class JsonHeaders(

)

class AccumulatingJsonCodec extends TypeTagFormats {
  val geometryCodecs = new GeometryJsonCodecs {}


  def getHeaders(): JsonHeaders = {
    ???
  }
  // implicit val FormatLTBounds = geometryCodecs.FormatLTBounds

  import geometryCodecs.FormatLTBounds
  import geometryCodecs.FormatLabel

  implicit val FormatStablePage: Format[StablePage] = Json.format[StablePage]
  implicit val FormatZone: Format[Zone] = Json.format[Zone]

  // implicit val FormatZone: Format[Zone] = new Format[Zone] {
  //   override def reads(json: JsValue)= json match {
  //     case JsArray(Seq(labelJs, regionsJs, idJs)) =>
  //       JsSuccess(Zone(idJs.as[Int@@ZoneID], regionsJs.as[Seq[PageRegion]], labelJs.as[Label]))

  //     case _ => JsError(s"Zone ${json}")
  //   }

  //   override def writes(o: Zone) = o match {
  //     case c@ Zone(id, regions, label) =>
  //       Json.arr(Json.toJson(label), Json.toJson(regions), Json.toJson(id))
  //   }
  // }

  implicit val FormatPageRegion: Format[PageRegion] = Json.format[PageRegion]
  // implicit val FormatTargetRegion: Format[TargetRegion] = new Format[TargetRegion] {
  //   override def reads(json: JsValue)= json match {
  //     case JsArray(Seq(JsNumber(id), bboxJs)) =>

  //       val pageId = PageID(pageIdNum.intValue())
  //       val stableId = serializationDefs
  //         .pageIdToStableID(pageId)
  //         .getOrElse { sys.error(s"no page found for pageId=${pageId}")}

  //       JsSuccess(TargetRegion(
  //         RecordedPageID(pageId, stableId),
  //         bboxJs.as[LTBounds]
  //       ))
  //     case _ => JsError(s"unmatched TargetRegion ${json}")
  //   }

  //   override def writes(o: TargetRegion) = o match {
  //     case c@ TargetRegion(
  //       RecordedPageID(
  //         PageID(pageId),
  //         StablePageID(DocumentID(stableId), PageNum(pageNum))
  //       ),
  //       bbox
  //     ) =>
  //       Json.arr(num(pageId), Json.toJson(bbox))
  //   }
  // }
}
