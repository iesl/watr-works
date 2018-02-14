package edu.umass.cs.iesl.watr
package watrcolors
package services


import workflow._
import corpora._
import org.http4s._
import org.http4s.circe._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._
import circe.generic.auto._
import TypeTags._
import tsec.authentication._
// import cats.syntax.all._
import cats.effect.IO


import models._
import circe.parser.decode
import watrmarks.LabelSchemas


trait CurationWorkflow extends WorkflowCodecs {

  def workflowApi: WorkflowApi
  def userbaseApi: UserbaseApi
  def docStore: DocumentZoningApi

  private def getZoneLockJson(zoneLockId: Int@@ZoneLockID) = (for {
    zoneLock   <- workflowApi.getZoneLock(zoneLockId)
  } yield {
    val assignee = zoneLock.assignee.map { userbaseApi.getUser(_) }
    Json.obj(
      "zonelock" := zoneLock,
      // "zone"     := docStore.getZone(zoneLock.zone),
      // "workflow" := workflowApi.getWorkflow(zoneLock.workflow),
      "assignee" := assignee
    )
  })


  def GET_workflows(): Json = {
    val workflowDefs = for {
      workflowId <- workflowApi.getWorkflows()
    } yield {
      workflowApi.getWorkflow(workflowId)
    }
    workflowDefs.asJson
  }

  def GET_workflows_report(id: String@@WorkflowID): Json = {
    workflowApi.getWorkflowReport(id).asJson
  }


  // Get all assignments for user
  def GET_curators_assignments(userId: Int@@UserID): Json = {
    val lockedZones = (for {
      zoneLockId <- workflowApi.getLockedZones(userId)
      js <- getZoneLockJson(zoneLockId)
    } yield js).asJson

    lockedZones
  }

  // Get all assignments involving document
  def GET_documents(stableId: String@@DocumentID): Json = {
    val lockedZones = (for {
      docId <- docStore.getDocument(stableId).toSeq
      zoneId <- docStore.getDocumentZones(docId)
      zoneLockId <- workflowApi.getLockForZone(zoneId)
      js <- getZoneLockJson(zoneLockId)
    } yield js).asJson

    lockedZones
  }


  def POST_workflows_assignments(workflowId: String@@WorkflowID, userId: Int@@UserID): Json = {
    val existingLock  = workflowApi.getLockedZones(userId).headOption
    lazy val newLock =  workflowApi.lockUnassignedZones(userId, workflowId, 1).headOption

    val lockedZones = for {
      zoneLockId  <- existingLock orElse newLock
      js <- getZoneLockJson(zoneLockId)
    } yield js

    lockedZones.asJson
  }

  // Post updates to specific assignment in assignments collection
  def POST_assignments(zoneLockId: Int@@ZoneLockID, mod: WorkflowMod, userId: Int@@UserID): Json = {
    println(s"got mod ${mod}")
    mod.update match {
      case StatusUpdate(newStatus) =>
        workflowApi.updateZoneStatus(zoneLockId, StatusCode(newStatus))
        if (newStatus != "Assigned") {
          workflowApi.releaseZoneLock(zoneLockId)
        }
      case Unassign() =>
        workflowApi.releaseZoneLock(zoneLockId)
    }

    getZoneLockJson(zoneLockId).asJson
  }

  def POST_workflows(workflowForm: CurationWorkflowDef): Json = {
    val maybeSchema = decode[LabelSchemas](workflowForm.labelSchemas)
    maybeSchema.fold(
      err => {
        Json.obj(
          "error" := "could not create workflow: malformed Json"
        )
      }, succ => {
        val workflowId = workflowApi.defineWorkflow(
          workflowForm.workflow,
          workflowForm.description, None,
          succ
        )
        Json.obj("workflowId" := workflowId)
      })
  }

}

trait CurationWorkflowServices extends CurationWorkflow with AuthenticatedService with WorkflowCodecs { self =>
  /**
    workflows    : Defines what will be labeled and the label schema of a curation effort
    curators     : Annotator pool (People)
    zones        : The things which may be assigned to a curator for further annotation
    assignments  : Zones assigned to a curator
    */

  // Mounted at /api/v1xx/workflow/..
  private val workflowEndpoints = Auth {

    /** Workflows */
    // Get All Workflows
    case req @ GET -> Root / "workflows" asAuthed user =>
      Ok(GET_workflows())

    // Get Status report for a workflow
    case req @ GET -> Root / "workflows" / workflowId / "report" asAuthed user =>
      Ok(GET_workflows_report(WorkflowID(workflowId)))

    // Get next assignment for a workflow: returns zoneLockId
    case req @ POST -> Root / "workflows" / workflowId / "assignments" asAuthed user  =>
      Ok(POST_workflows_assignments(WorkflowID(workflowId), user.id))


    case req @ POST -> Root / "workflows" asAuthed user =>
      // Create a new workflow
      for {
        workflowForm <- decodeOrErr[CurationWorkflowDef](req.request)
        resp          <- Ok(POST_workflows(workflowForm))
      } yield resp


    /** Curators */
    case req @ GET -> Root / "curators"  asAuthed user  =>
      // Get all known curators
      val resp = for {
        u <- userbaseApi.getUsers()
      } yield u

      Ok(resp.asJson)

    case req @ GET -> Root / "curators" / curatorId / "assignments" asAuthed user  =>
      // Get list of assignments for curator
      Ok(GET_curators_assignments(user.id))

    /** Assignments */
    // Change status for an assignment (zoneLockId)
    case req @ POST -> Root / "assignments" / IntVar(zoneLockId) asAuthed user =>
      val resp = for {

        js <- req.request.as[Json]
        mod <-  IO{  Decoder[WorkflowMod].decodeJson(js).fold(fail => {
          throw new Throwable(s"error decoding ${js}")
        }, mod => mod) }
      } yield {
        POST_assignments(ZoneLockID(zoneLockId), mod, user.id)
      }
      Ok(resp)



    /** Zones */
    // Get any assignment status info for a particular zone
    case req @ GET -> Root / "zones" / IntVar(zoneId) asAuthed user  =>
      val lockInfo = for {
        zoneLockId   <- workflowApi.getLockForZone(ZoneID(zoneId))
        zoneLock   <- workflowApi.getZoneLock(zoneLockId)
      } yield {
        val assignee = zoneLock.assignee.map { userbaseApi.getUser(_) }
        Json.obj(
          "zonelock" := zoneLock,
          "zone"     := docStore.getZone(zoneLock.zone),
          // "workflow" := workflowApi.getWorkflow(zoneLock.workflow),
          "assignee" := assignee
        )
      }
      val res = lockInfo.getOrElse {
        Json.obj(
          "zone"     := docStore.getZone(ZoneID(zoneId)),
        )
      }
      Ok(res)



    /** Documents */
    case req @ GET -> Root / "documents" / stableId asAuthed user  =>
      // Get all target zones and assignment statuses within a document
      Ok(GET_documents(DocumentID(stableId)))


  }

  def curationServices =  workflowEndpoints

}
