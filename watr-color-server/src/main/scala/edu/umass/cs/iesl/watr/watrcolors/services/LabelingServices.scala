package edu.umass.cs.iesl.watr
package watrcolors
package services

import org.http4s._
import org.http4s.circe._
import _root_.io.circe
import circe._
import circe.literal._
import TypeTags._
import geometry._
import textgrid._
import models._
import tsec.authentication._
import corpora._
import database.CorpusAccessDB

trait ZoningApi extends HttpPayloads {
  import watrmarks._
  import circe._
  import circe.syntax._
  import circe.literal._

  def docStore: DocumentZoningApi
  def corpusAccessDB: CorpusAccessDB

  def readZones(stableId: String@@DocumentID): Json = {
    // val stableId = DocumentID(stableIdStr)
    val docId  = docStore.getDocument(stableId).getOrElse {
      sys.error(s"docId not found for ${stableId}")
    }

    val allDocZones = for {
      labelId <- docStore.getZoneLabelsForDocument(docId)
      if ! docStore.getLabel(labelId).fqn.startsWith("seg:")
      zoneId <- docStore.getZonesForDocument(docId, labelId)
    } yield {
      docStore.getZone(zoneId)
    }
    Json.obj(
      "zones" := allDocZones
    )
  }

  def deleteZone(zoneId: Int@@ZoneID): Unit = {
    docStore.deleteZone(zoneId)
  }

  def mergeZones(zoneIds: Seq[Int@@ZoneID]): Unit = {

  }


  def createZone(userId: Int@@UserID, label: Label, pageRegion: PageRegion): Unit = {
    // corpusAccessDB.createAnnotation(userId, docId, workflowId)
    docStore.labelRegions(label, Seq(pageRegion))
  }

  def putZoneText(): Unit = {

  }




  protected def targetToPageBounds(docId: Int@@DocumentID, ltarget: LTarget): PageRegion = {
    val pageNum = PageNum(ltarget.page)

    val bbox = LTBounds.Doubles(
      ltarget.bbox.left, ltarget.bbox.top,
      ltarget.bbox.width, ltarget.bbox.height
    )
    val pageId  = docStore.getPage(docId, pageNum).getOrElse { sys.error(s"Document ${docId} has no page ${pageNum}") }
    val regionId = docStore.addTargetRegion(pageId, bbox)
    docStore.getTargetRegion(regionId)
  }

}

trait ZoningServices extends AuthenticatedService with ZoningApi { self =>

  // Mounted at /api/v1xx/labeling/..
  val labelingServiceEndpoints = Auth {

    // Read all zones for document
    case req @ GET -> Root / "zones" / stableIdStr asAuthed user =>
      val zones = readZones(DocumentID(stableIdStr))

      Ok(zones)

    // Delete zone
    case req @ DELETE -> Root / "zones" / IntVar(zoneId) asAuthed user =>
      println(s"Got delete zone request")

      val zone = docStore.getZone(ZoneID(zoneId))
      docStore.deleteZone(zone.id)

      Ok(Json.obj())

    // Create new zone
    case req @ POST -> Root / "zones" asAuthed user =>
      println(s"${req}")

      val handler = for {
        labeling <- decodeOrErr[LabelingReqForm](req.request)
        _ = {
          val stableId = DocumentID(labeling.stableId)

          val docId  = docStore.getDocument(stableId).getOrElse {
            sys.error(s"docId not found for ${stableId}")
          }

          val pageBounds = targetToPageBounds(docId, labeling.target)

          createZone(user.id, labeling.labelChoice, pageBounds)
        }
        resp <- Ok(Json.obj())
      } yield resp

      orErrorJson(handler)

    // Update zone
    case req @ POST -> Root / "zones" / IntVar(zoneId)  asAuthed user =>
      println(s"${req}")

      val handler = for {
        update <- decodeOrErr[ZoneUpdate](req.request)
        _ = {
          update match {
            case ZoneUpdate.MergeWith(otherZoneId) =>
              println(s"merging zone ${zoneId} with ${otherZoneId}")

            case ZoneUpdate.SetText(gridJson) =>
              println(s"setting zone ${zoneId} text ")
              val textgrid = TextGrid.fromJson(gridJson)
              // docStore.getZoneTextAsJsonStr(zoneId: Int <refinement> ZoneID)
              docStore.setZoneText(ZoneID(zoneId), textgrid)
          }
        }
        resp <- Ok(Json.obj())
      } yield resp

      orErrorJson(handler)

  }
}
