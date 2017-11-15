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
import corpora._
import fs2._
import TypeTags._
import watrmarks._

import geometry._
// import geometry.syntax._

trait TypeTagCodecs {
  // import circe.generic.semiauto._

  implicit def Enc_IntTypeTags[T]: Encoder[Int@@T] = Encoder.encodeInt.contramap(_.unwrap)
  implicit def Enc_StringTypeTags[T]: Encoder[String@@T] = Encoder.encodeString.contramap(_.unwrap)

}
trait CirceJsonCodecs extends TypeTagCodecs {
  import circe.generic.semiauto._

  implicit val Enc_LTBounds: Encoder[LTBounds] = deriveEncoder

  implicit val Enc_StablePage: Encoder[StablePage] = deriveEncoder
  implicit val Enc_PageRegion: Encoder[PageRegion] = deriveEncoder


  // implicit val Enc_XX: Encoder[XX] = deriveEncoder
  implicit val Enc_Label: Encoder[Label] = Encoder.encodeString.contramap(_.fqn)
  implicit val Dec_Label: Decoder[Label] = Decoder.decodeString.map(Label(_))

  implicit lazy val Enc_Zone: Encoder[Zone] = deriveEncoder

}

// import CirceJsonCodecs._

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

case class LabelingSelection(
  annotType: String,
  targets: Seq[LTarget]
)

object LabelingSelection {
  import circe.generic.semiauto._
  implicit val encoder: Encoder[LabelingSelection] = deriveEncoder
  implicit val decoder: Decoder[LabelingSelection] = deriveDecoder
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

trait LabelingServices extends CirceJsonCodecs { self =>

  def corpusAccessApi: CorpusAccessApi

  private lazy val docStore: DocumentZoningApi = corpusAccessApi.docStore

  def decodeOrErr[T: Decoder](req: Request): Task[T] = {
    req.as(jsonOf[T]).attemptFold(t => {
      println(s"Error: ${t}")
      println(s"Error: ${t.getCause}")
      println(s"Error: ${t.getMessage}")
      sys.error(s"${t}")
    }, ss => ss)
  }

  // Mounted at /api/v1xx/labeling/..
  val labelingServiceEndpoints = HttpService {
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

      for {
        resp <- Ok(jsonResp).putHeaders(H.`Content-Type`(MediaType.`application/json`))
      } yield resp

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

    case req @ POST -> Root / "label"  =>

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
              ltarget.bbox(0),
              ltarget.bbox(1),
              ltarget.bbox(2),
              ltarget.bbox(3)
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
