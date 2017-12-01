package edu.umass.cs.iesl.watr
package watrcolors
package server


import org.http4s._
import org.http4s.{headers => H}
import org.http4s.circe._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._
import TypeTags._
import geometry._
import textgrid._
import models._
import tsec.authentication._

trait LabelingServices extends AuthenticatedService { self =>

  // Mounted at /api/v1xx/labeling/..
  val labelingServiceEndpoints = Auth {

    case req @ POST -> Root / "ui" / "labeler" asAuthed user =>

      for {
        labels <- decodeOrErr[LabelerReqForm](req.request)
        panel = html.Parts.labelingPanel(labels.labels)
        ok <- Ok(panel.toString())
      } yield {
        ok.putHeaders(H.`Content-Type`(MediaType.`text/html`))
      }

    case req @ GET -> Root / "labels" / stableIdStr asAuthed user =>
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

    case req @ DELETE -> Root / "label"  asAuthed user =>
      println(s"Got delete label request")

      for {
        deleteReq <- decodeOrErr[DeleteZoneRequest](req.request)

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
        resp <- Ok(allZones)
      } yield {
        resp.putHeaders(H.`Content-Type`(MediaType.`application/json`))
      }

    case req @ POST -> Root / "label" / "span"  asAuthed user =>

      for {
        labeling <- decodeOrErr[LabelSpanReq](req.request)
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

        resp <- Ok(allZones)

      } yield {
        resp.putHeaders(H.`Content-Type`(MediaType.`application/json`))
      }

    case req @ POST -> Root / "label" / "region" asAuthed user =>

      for {
        labeling <- decodeOrErr[LabelingReqForm](req.request)
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
        resp <- Ok(allZones)
      } yield {
        resp.putHeaders(H.`Content-Type`(MediaType.`application/json`))
      }
  }
}
