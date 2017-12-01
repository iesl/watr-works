package edu.umass.cs.iesl.watr
package watrcolors
package services


import org.http4s._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._
import TypeTags._
import cats.effect._

import models._

trait CurationWorkflowServices extends ServiceCommons with WorkflowCodecs { self =>

  // Mounted at /api/v1xx/workflow/..
  val curationWorkflowEndpoints = HttpService[IO] {

    // Get all workflows
    case req @ GET -> Root / "workflows" =>
      val workflowDefs = for {
        workflowId <- workflowApi.getWorkflows()
      } yield {
        workflowApi.getWorkflow(workflowId)
      }

      okJson(workflowDefs.asJson)

    case req @ GET -> Root / "workflow" / workflowId / "report" =>
      val workflowReport = workflowApi.getWorkflowReport(WorkflowID(workflowId))

      okJson(workflowReport.asJson)

    case req @ GET -> Root / "workflow" / workflowId / "assignment" :? UserQP(userEmail)  =>
      val lockedZones = (for {
        userId     <- userbaseApi.getUserByEmail(EmailAddr(userEmail)).toSeq
        zoneLockId <- workflowApi.lockUnassignedZones(userId, WorkflowID(workflowId), 1)
        zoneLock   <- workflowApi.getZoneLock(zoneLockId)
      } yield {
        docStore.getZone(zoneLock.zone).asJson
      }).asJson

      okJson(lockedZones)

    // Complete assignment, set status and unlock
    case req @ PUT -> Root / "workflow" / workflowId / "assignment" :? UserQP(userEmail) +& ZoneQP(zoneId) +& StatusQP(status) =>

      for {
        userId     <- userbaseApi.getUserByEmail(EmailAddr(userEmail)).toSeq
        zoneLockId <- workflowApi.getLockForZone(ZoneID(zoneId))
        zoneLock   <- workflowApi.getZoneLock(zoneLockId)
      } {
        workflowApi.updateZoneStatus(zoneLockId, StatusCode(status))
        workflowApi.releaseZoneLock(zoneLockId)
      }

      okJson(Json.fromString("Ok"))

    // Get current assignments for user
    case req @ GET -> Root / "workflow" / workflowId / "assignments" :? UserQP(userEmail) =>
      val lockedZones = (for {
        userId     <- userbaseApi.getUserByEmail(EmailAddr(userEmail)).toSeq
        zoneLockId <- workflowApi.getLockedZones(userId)
        zoneLock   <- workflowApi.getZoneLock(zoneLockId)
      } yield {
        docStore.getZone(zoneLock.zone).asJson
      }).asJson

      okJson(lockedZones)

  }

}
