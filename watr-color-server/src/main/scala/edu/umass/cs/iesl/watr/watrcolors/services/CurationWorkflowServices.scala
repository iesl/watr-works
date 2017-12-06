package edu.umass.cs.iesl.watr
package watrcolors
package services

import org.http4s._
import org.http4s.circe._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._
import circe.generic.auto._
import TypeTags._
import tsec.authentication._
import cats.syntax.all._

import models._
import watrmarks.Label

trait CurationWorkflowServices extends AuthenticatedService with WorkflowCodecs { self =>


  private def getZoneJson(zoneLockId: Int@@ZoneLockID) = (for {
    zoneLock   <- workflowApi.getZoneLock(zoneLockId)
  } yield {
    docStore.getZone(zoneLock.zone).asJson
  })

  private val workflowEndpoints2 = Auth {
    // Get list of all workflows
    case req @ GET  -> Root / "workflows" asAuthed user =>
    case req @ GET  -> Root / "workflows" / workflowId / "report" asAuthed user =>
    case req @ POST -> Root / "workflows" / workflowId / "assignments" asAuthed user  =>
    case req @ GET  -> Root / "workflows" / workflowId / "assignments" asAuthed user  =>
    case req @ PUT  -> Root / "workflows" / workflowId / "assignments" / IntVar(zoneId) :? StatusQP(status) asAuthed user =>
    case req @ POST -> Root / "workflows" asAuthed user =>


  }
  // Mounted at /api/v1xx/workflow/..
  private val workflowEndpoints = Auth {
    // Get list of all workflows
    case req @ GET -> Root / "workflows" asAuthed user =>
      val workflowDefs = for {
        workflowId <- workflowApi.getWorkflows()
      } yield {
        workflowApi.getWorkflow(workflowId)
      }

      Ok(workflowDefs.asJson)

    case req @ GET -> Root / "workflows" / workflowId / "report" asAuthed user =>
      val workflowReport = workflowApi.getWorkflowReport(WorkflowID(workflowId))

      Ok(workflowReport.asJson)

    case req @ POST -> Root / "workflows" / workflowId / "assignments" asAuthed user  =>

      val lockedZones = (for {
        zoneLockId <- workflowApi.lockUnassignedZones(user.id, WorkflowID(workflowId), 1)
        js <- getZoneJson(zoneLockId)
      } yield js).asJson

      Ok(lockedZones)

    case req @ GET -> Root / "workflows" / workflowId / "assignments" asAuthed user  =>

      val lockedZones = (for {
        zoneLockId <- workflowApi.getLockedZones(user.id)
        js <- getZoneJson(zoneLockId)
      } yield js).asJson

      Ok(lockedZones)


    // Complete assignment, set status and unlock
    case req @ PUT -> Root / "workflows" / workflowId / "assignments" / IntVar(zoneId) :? StatusQP(status) asAuthed user =>

      for {
        zoneLockId <- workflowApi.getLockForZone(ZoneID(zoneId))
        zoneLock   <- workflowApi.getZoneLock(zoneLockId)
      } {
        workflowApi.updateZoneStatus(zoneLockId, StatusCode(status))
        workflowApi.releaseZoneLock(zoneLockId)
      }

      Ok(Json.obj())

    case req @ POST -> Root / "workflows" asAuthed user =>

      for {
        workflowForm    <- req.request.attemptAs[WorkflowForm].fold(
          decodeFailure => throw new Exception(s"decodeFailure: ${decodeFailure}"),
          succ => succ)
        workflowId     = workflowApi.defineWorkflow(workflowForm.workflow, workflowForm.description, Label(workflowForm.targetLabel))

        resp          <- Ok(Json.obj("workflowId" := workflowId))
      } yield resp



  }

  def curationServices =  workflowEndpoints

}
