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
import workflow._
import corpora.{RelationModel => R}
import TypeTags._

trait WorkflowCodecs extends CirceJsonCodecs {
  import circe.generic.semiauto._

  //   implicit val Enc_XX: Encoder[XX] = deriveEncoder
  //   implicit val Dec_XX: Decoder[XX] = deriveDecoder

  lazy implicit val Enc_WorkflowDef: Encoder[R.WorkflowDef] = deriveEncoder

}

trait CurationWorkflowServices extends WorkflowCodecs { self =>

  def corpusAccessApi: CorpusAccessApi

  object UserQP extends QueryParamDecoderMatcher[String]("user")
  object ZoneQP extends QueryParamDecoderMatcher[Int]("zone")
  object StatusQP extends QueryParamDecoderMatcher[String]("status")

  private lazy val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  private lazy val userApi = corpusAccessApi.userbaseApi
  private lazy val docStore = corpusAccessApi.docStore

  def okJson(resp: Json): fs2.Task[Response] = {
    for {
      resp <- Ok(resp).putHeaders(H.`Content-Type`(MediaType.`application/json`))
    } yield resp
  }

  // Mounted at /api/v1xx/workflow/..
  val curationWorkflowEndpoints = HttpService {

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
        userId     <- userApi.getUserByEmail(userEmail).toSeq
        zoneLockId <- workflowApi.lockUnassignedZones(userId, WorkflowID(workflowId), 1)
        zoneLock   <- workflowApi.getZoneLock(zoneLockId)
      } yield {
        docStore.getZone(zoneLock.zone).asJson
      }).asJson

      okJson(lockedZones)

    // Complete assignment, set status and unlock
    case req @ PUT -> Root / "workflow" / workflowId / "assignment" :? UserQP(userEmail) +& ZoneQP(zoneId) +& StatusQP(status) =>

      for {
        userId     <- userApi.getUserByEmail(userEmail).toSeq
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
        userId     <- userApi.getUserByEmail(userEmail).toSeq
        zoneLockId <- workflowApi.getLockedZones(userId)
        zoneLock   <- workflowApi.getZoneLock(zoneLockId)
      } yield {
        docStore.getZone(zoneLock.zone).asJson
      }).asJson

      okJson(lockedZones)

  }

}
