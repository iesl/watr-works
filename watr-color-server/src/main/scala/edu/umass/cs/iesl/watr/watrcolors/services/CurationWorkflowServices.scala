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
import models.users._

import watrmarks.Label

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
      "zone"     := docStore.getZone(zoneLock.zone),
      "workflow" := workflowApi.getWorkflow(zoneLock.workflow),
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


  // Get all assigned
  def GET_workflows_assignments(user: User): Json = {
    val lockedZones = (for {
      zoneLockId <- workflowApi.getLockedZones(user.id)
      js <- getZoneLockJson(zoneLockId)
    } yield js).asJson

    lockedZones
  }

  def POST_workflows_assignments(workflowId: String@@WorkflowID, userId: Int@@UserID): Json = {
    val lockedZones = for {
      zoneLockId <- workflowApi.lockUnassignedZones(userId, workflowId, 1)
      js <- getZoneLockJson(zoneLockId)
    } yield js

    lockedZones.asJson
  }

  // Post updates to specific assignment in assignments collection
  def POST_workflows_assignments_assignment(workflowId: String@@WorkflowID, zoneLockId: Int@@ZoneLockID, mod: WorkflowMod, userId: Int@@UserID): Json = {
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
    val lockedZones = for {
      js <- getZoneLockJson(zoneLockId)
    } yield js

    lockedZones.asJson
  }

  // Complete assignment, set status and unlock
  def PUT_workflows_assignments(zoneId: Int@@ZoneID, statusCode: String@@StatusCode): Json = {
    for {
      zoneLockId <- workflowApi.getLockForZone(zoneId)
      zoneLock   <- workflowApi.getZoneLock(zoneLockId)
    } {
      workflowApi.updateZoneStatus(zoneLockId, statusCode)
      workflowApi.releaseZoneLock(zoneLockId)
    }

    Json.obj()
  }

  def POST_workflows(workflowForm: CurationWorkflowDef): Json = {
    val workflowId = workflowApi.defineWorkflow(
      workflowForm.workflow,
      workflowForm.description,
      workflowForm.targetLabel,
      workflowForm.labelSchemas
    )
    Json.obj("workflowId" := workflowId)
  }
}


trait CurationWorkflowServices extends CurationWorkflow with AuthenticatedService with WorkflowCodecs { self =>

  // Mounted at /api/v1xx/workflow/..
  private val workflowEndpoints = Auth {

    case req @ GET -> Root / "workflows" asAuthed user =>
      Ok(GET_workflows())

    case req @ GET -> Root / "workflows" / workflowId / "report" asAuthed user =>
      Ok(GET_workflows_report(WorkflowID(workflowId)))

    case req @ POST -> Root / "workflows" / workflowId / "assignments" asAuthed user  =>
      Ok(POST_workflows_assignments(WorkflowID(workflowId), user.id))

    case req @ POST -> Root / "workflows" / workflowId / "assignments" / IntVar(zonelockId) asAuthed user  =>
      for {
        js   <- req.request.as[Json]
        mod <-  IO{  Decoder[WorkflowMod].decodeJson(js).fold(fail => {
          throw new Throwable(s"error decoding ${js}")
        }, mod => mod) }
        resp  <- Ok(POST_workflows_assignments_assignment(WorkflowID(workflowId), ZoneLockID(zonelockId), mod, user.id))
      } yield resp

    case req @ GET -> Root / "workflows" / "assignments" asAuthed user  =>
      Ok(GET_workflows_assignments(user))

    case req @ PUT -> Root / "workflows" / workflowId / "assignments" / IntVar(zoneId) :? StatusQP(status) asAuthed user =>
      Ok(PUT_workflows_assignments(ZoneID(zoneId), StatusCode(status)))

    case req @ POST -> Root / "workflows" asAuthed user =>
      for {
        workflowForm  <- req.request.as[CurationWorkflowDef]
        resp          <- Ok(POST_workflows(workflowForm))
      } yield resp
  }

  def curationServices =  workflowEndpoints

}
