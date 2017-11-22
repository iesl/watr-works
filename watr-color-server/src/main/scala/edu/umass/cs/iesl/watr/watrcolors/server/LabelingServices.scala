package edu.umass.cs.iesl.watr
package watrcolors
package server


import org.http4s._
import org.http4s.{headers => H}
import org.http4s.dsl._
import org.http4s.circe._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._
import TypeTags._
import watrmarks._
import org.http4s.circe._
import geometry._
import textgrid._
import cats.implicits._, cats.data._
import cats.effect._

case class LabelerReqForm(
  labels: Seq[Label],
  description: String
)

object LabelerReqForm extends CirceJsonCodecs {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelerReqForm] = deriveEncoder
  implicit val decoder: Decoder[LabelerReqForm] = deriveDecoder
}


case class LTarget(
  page: Int,
  bbox: Seq[Int]
)
object LTarget {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LTarget] = deriveEncoder
  implicit val decoder: Decoder[LTarget] = deriveDecoder
}

// case class GlyphTarget(
//   page: Int,
//   bbox: Seq[Int],
//   char: String
// )


// object GlyphTarget {
//   import circe.generic.semiauto._
//   implicit val encoder: Encoder[GlyphTarget] = deriveEncoder
//   implicit val decoder: Decoder[GlyphTarget] = deriveDecoder
// }

case class LabelingSelection(
  annotType: String,
  targets: Seq[LTarget]
)

object LabelingSelection {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelingSelection] = deriveEncoder
  implicit val decoder: Decoder[LabelingSelection] = deriveDecoder
}

case class LabelSpanReq(
  stableId: String,
  labelChoice: Label,
  //           (page: Int, bbox: Seq[Int], char: String)
  targets: Seq[(Int, (Int, Int, Int, Int), String)]
)

object LabelSpanReq extends CirceJsonCodecs {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelSpanReq] = deriveEncoder
  implicit val decoder: Decoder[LabelSpanReq] = deriveDecoder
}

case class LabelingReqForm(
  stableId: String,
  labelChoice: Label,
  selection: LabelingSelection
)

object LabelingReqForm extends CirceJsonCodecs {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelingReqForm] = deriveEncoder
  implicit val decoder: Decoder[LabelingReqForm] = deriveDecoder
}

case class LabelsRequest(
  stableId: String
)

object LabelsRequest {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelsRequest] = deriveEncoder
  implicit val decoder: Decoder[LabelsRequest] = deriveDecoder
}
case class DeleteZoneRequest(
  stableId: String,
  zoneIds: Seq[Int]
)

object DeleteZoneRequest {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[DeleteZoneRequest] = deriveEncoder
  implicit val decoder: Decoder[DeleteZoneRequest] = deriveDecoder
}

trait LabelingServices extends ServiceCommons { self =>

  // Mounted at /api/v1xx/labeling/..
  val labelingServiceEndpoints = HttpService[IO] {
    case req @ POST -> Root / "ui" / "labeler" =>

      for {
        labels <- decodeOrErr[LabelerReqForm](req)
        panel = html.Parts.labelingPanel(labels.labels)
        resp <- Ok(panel.toString).putHeaders(H.`Content-Type`(MediaType.`text/html`))
      } yield resp

    case req @ GET -> Root / "labels" / stableIdStr =>
      val stableId = DocumentID(stableIdStr)
      val docId  = docStore.getDocument(stableId).getOrElse {
        sys.error(s"docId not found for ${stableId}")
      }

      val allDocZones = for {
        labelId <- docStore.getZoneLabelsForDocument(docId)
        zoneId <- docStore.getZonesForDocument(docId, labelId) if labelId.unwrap > 1  // TODO un hardcode this
      } yield {
        val zone = docStore.getZone(zoneId)
        zone.asJson
      }
      val jsonResp = Json.obj(
        ("zones", Json.arr(allDocZones:_*))
      )

      okJson(jsonResp)

    case req @ DELETE -> Root / "label"  =>
      println(s"Got delete label request")

      for {
        deleteReq <- decodeOrErr[DeleteZoneRequest](req)

        allZones = {
          val stableId = DocumentID(deleteReq.stableId)
          val docId  = docStore.getDocument(stableId).getOrElse {
            sys.error(s"docId not found for ${stableId}")
          }
          for {
            zoneId <- deleteReq.zoneIds
          } {
            val zone = docStore.getZone(ZoneID(zoneId))
            println(s"Deleting zones ${zone}")
            docStore.deleteZone(ZoneID(zoneId))
          }
          val allDocZones = for {
            labelId <- docStore.getZoneLabelsForDocument(docId)
            zoneId <- docStore.getZonesForDocument(docId, labelId) if labelId.unwrap > 1  // TODO un hardcode this
          } yield {
            val zone = docStore.getZone(zoneId)
            zone.asJson
          }
          Json.obj(
            ("zones", Json.arr(allDocZones:_*))
          )
        }
        resp <- Ok(allZones).putHeaders(H.`Content-Type`(MediaType.`application/json`))
      } yield {
        resp
      }

    case req @ POST -> Root / "label" / "span"  =>

      for {
        labeling <- decodeOrErr[LabelSpanReq](req)
        allZones = {
          val stableId = DocumentID(labeling.stableId)
          val docId  = docStore.getDocument(stableId).getOrElse {
            sys.error(s"docId not found for ${stableId}")
          }

          val asdf = labeling.targets.map { case (page, (l, t, w, h), charStr) =>
            val pageNum = PageNum(page)

            // val (l, t, w, h) = bbox
            // (bbox(0), bbox(1), bbox(2), bbox(3))
            val bbox = LTBounds.IntReps(l, t, w, h)
            (pageNum, bbox, charStr.head)
          }

          val textgrid = TextGrid.fromPageGlyphArrays(stableId, asdf)
          val gridBounds = textgrid.pageBounds()

          docStore.labelRegions(labeling.labelChoice, gridBounds).foreach{
            zoneId => docStore.setZoneText(zoneId, textgrid)
          }

          val allDocZones = for {
            labelId <- docStore.getZoneLabelsForDocument(docId)
            zoneId <- docStore.getZonesForDocument(docId, labelId) if labelId.unwrap > 1  // TODO un hardcode this
          } yield {
            val zone = docStore.getZone(zoneId)
            zone.asJson
          }
          Json.obj(
            ("zones", Json.arr(allDocZones:_*))
          )


        }
        resp <- Ok(allZones).putHeaders(H.`Content-Type`(MediaType.`application/json`))
      } yield resp

    case req @ POST -> Root / "label" / "region" =>

      for {
        labeling <- decodeOrErr[LabelingReqForm](req)
        allZones = {
          val stableId = DocumentID(labeling.stableId)
          val docId  = docStore.getDocument(stableId).getOrElse {
            sys.error(s"docId not found for ${stableId}")
          }


          val regions = labeling.selection.targets.map { ltarget =>
            val pageNum = PageNum(ltarget.page)

            val (l, t, w, h) = (
              ltarget.bbox(0), ltarget.bbox(1), ltarget.bbox(2), ltarget.bbox(3)
            )
            val bbox = LTBounds.IntReps(l, t, w, h)

            val pageRegions = for {
              pageId    <- docStore.getPage(docId, pageNum).toSeq
            } yield {
              val regionId = docStore.addTargetRegion(pageId, bbox)
              docStore.getTargetRegion(regionId)
            }
            pageRegions
          }

          docStore.labelRegions(labeling.labelChoice, regions.flatten)

          val allDocZones = for {
            labelId <- docStore.getZoneLabelsForDocument(docId)
            zoneId <- docStore.getZonesForDocument(docId, labelId) if labelId.unwrap > 1  // TODO un hardcode this
          } yield {
            val zone = docStore.getZone(zoneId)
            zone.asJson
          }
          Json.obj(
            ("zones", Json.arr(allDocZones:_*))
          )

        }
        resp <- Ok(allZones).putHeaders(H.`Content-Type`(MediaType.`application/json`))
      } yield resp
  }
}
