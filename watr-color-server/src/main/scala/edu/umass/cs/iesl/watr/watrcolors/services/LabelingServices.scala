package edu.umass.cs.iesl.watr
package watrcolors
package services

import org.http4s._
import org.http4s.circe._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._

import TypeTags._
import geometry._
// import textgrid._
import models._
import tsec.authentication._
import corpora._
import database.CorpusAccessDB

trait ZoningApi extends HttpPayloads {
  // import watrmarks._

  def docStore: DocumentZoningApi
  def corpusAccessDB: CorpusAccessDB
  def annotApi: DocumentAnnotationApi

  def getDocumentAnnotRecs(stableId: String@@DocumentID): Json = {

    val docId  = docStore.getDocument(stableId).getOrElse {
      sys.error(s"docId not found for ${stableId}")
    }

    val allDocZones = for {
      annotId <- annotApi.listDocumentAnnotations(docId)
    } yield {
      annotApi.getAnnotationRecord(annotId).asJson
    }
    Json.arr(allDocZones:_*)
  }

  def deleteAnnotation(annotId: Int@@AnnotationID): Unit = {
    annotApi.deleteAnnotation(annotId)
  }

  def mergeZones(zoneIds: Seq[Int@@ZoneID]): Unit = {
  }


  // def createZone(userId: Int@@UserID, label: Label, docId: PageRegion): Unit = {
  //   annotApi.createAnnotation(docId)
  //   // corpusAccessDB.createAnnotation(userId, docId, workflowId)
  //   // docStore.labelRegions(label, Seq(pageRegion))
  //   ???
  // }

  def putZoneText(): Unit = {

  }

  protected def targetToPageRegion(docId: Int@@DocumentID, ltarget: LTarget): PageRegion = {
    val pageNum = PageNum(ltarget.page)

    val bbox = LTBounds.Doubles(
      ltarget.bbox.left, ltarget.bbox.top,
      ltarget.bbox.width, ltarget.bbox.height
    )

    val pageId  = docStore.getPage(docId, pageNum).getOrElse { sys.error(s"Document ${docId} has no page ${pageNum}") }
    val stablePage = docStore.getPageIdentifier(pageId)

    PageRegion(stablePage, bbox)
  }

}

trait ZoningServices extends AuthenticatedService with ZoningApi { self =>

  // Mounted at /api/v1xx/labeling/..
  val labelingServiceEndpoints = Auth {

    // Read all annotations for document
    case req @ GET -> Root / "zones" / stableIdStr asAuthed user =>
      val zones = getDocumentAnnotRecs(DocumentID(stableIdStr))

      Ok(zones)

    // Delete zone
    case req @ DELETE -> Root / "zones" / IntVar(id) asAuthed user =>
      println(s"Got delete annotation request")

      annotApi.deleteAnnotation(AnnotationID(id))

      Ok(Json.obj())

    // Create new annotation
    case req @ POST -> Root / "zones" asAuthed user =>
      println(s"${req}")

      val handler = for {
        labeling <- decodeOrErr[NewRegionLabel](req.request)
        annotRec = {
          val stableId = DocumentID(labeling.stableId)

          val docId  = docStore.getDocument(stableId).getOrElse {
            sys.error(s"docId not found for ${stableId}")
          }

          val pageRegion = targetToPageRegion(docId, labeling.target)
          val location = AnnotatedLocation.Zone(List(
            pageRegion
          ))

          val label = labeling.labelChoice

          val annotId = annotApi.createAnnotation(label, location)
          annotApi.assignOwnership(annotId, user.id)

          // annotApi.setCorpusPath(annotId, user.id)
          // val annotBody = ""

          annotApi.getAnnotationRecord(annotId).asJson
        }
        resp <- Ok(annotRec)
      } yield resp

      orErrorJson(handler)

    case req @ POST -> Root / "zones" / IntVar(annotId)  asAuthed user =>
      println(s"${req}")

      val handler = for {
        update <- decodeOrErr[ZoneUpdate](req.request)
        _ = {
          update match {
            case ZoneUpdate.MergeWith(otherZoneId) =>
              println(s"merging zone ${annotId} with ${otherZoneId}")

            case ZoneUpdate.SetText(gridJson) =>
              println(s"setting annotation ${annotId} body ")
              val body = AnnotationBody.TextGrid(gridJson.noSpaces)
              annotApi.updateBody(AnnotationID(annotId), body)
          }
        }
        resp <- Ok(Json.obj())
      } yield resp

      orErrorJson(handler)

  }
}
