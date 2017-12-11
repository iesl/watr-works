package edu.umass.cs.iesl.watr
package workflow

import corpora.{RelationModel => R}
import TypeTags._
import watrmarks.Label

/**
  * A Workflow describes the progress of a data curation effort
  *
  * Given a set of pre-existing zones, add new labels within those zones.
  *
  * For example, a curation effort might be:
  *    Given a set of regions labeled "Bibliography", ask users to label individual references
  *
  *    Given the pages of a document, label the titles and abstracts.
  *    For consistency, the "zone" of a full document has the label DocumentPages
  *
  * Workflow operates like so:
  *    Assume that users are present in the user table.
  *    A new workflow is defined, including a unique name and description, along with the zone label to be locked for further labeling.
  *    One or more zone locks are acquired for a user. The pool of candidate zones for locking include only those that
  *       have no lock status, i.e., have not yet been seen by any user
  *    When the user is finished adding labels to a particular zone, the lock status is changed to Completed or Skipped
  *    Users continue to acquire new zone locks until there are none left
  *
  *
  *    A WorkflowReport includes
  *       a count of as-yet unseen zones,
  *       counts of zones having a particular status
  *       users with assigned zones
  *
  */

object WorkflowStatus {
  val OnHold    = StatusCode("OnHold")
  val Active    = StatusCode("Active")
  val Complete  = StatusCode("Complete")
}


object ZoneLockStatus {
  val Assigned     = StatusCode("Assigned")
  val Completed    = StatusCode("Completed")
  val NeedsReview  = StatusCode("NeedsReview")
  val Skipped      = StatusCode("Skipped")

  val all = List(Assigned, Completed, Skipped, NeedsReview)
}

case class WorkflowReport(
  unassignedCount: Int,
  statusCounts: Map[String@@StatusCode, Int],
  userAssignmentCounts: Map[Int@@UserID, Int]
)

object WorkflowReport extends TypeTagCodecs {
  import _root_.io.circe
  import circe._
  import circe.generic.semiauto._

  implicit val encMap: Encoder[Map[String@@StatusCode, Int]] =
    Encoder[Map[String, Int]].contramap { m =>
      m.map{case (k, v) => (k.unwrap, v)}
    }

  implicit val encMap2: Encoder[Map[Int@@UserID, Int]] =
    Encoder[Map[Int, Int]].contramap { m =>
      m.map{case (k, v) => (k.unwrap, v)}
    }

  implicit lazy val enc: Encoder[WorkflowReport] = deriveEncoder
}

trait UserbaseApi {
  def addUser(email: String@@EmailAddr): Int@@UserID
  def getUser(userId: Int@@UserID): Option[R.Person]
  def getUserByEmail(email: String@@EmailAddr): Option[Int@@UserID]
  def deleteUser(userId: Int@@UserID): Unit
  def setPassword(userId:Int@@UserID, passhash: String@@PasswordHash): Unit
  def getPassword(userId:Int@@UserID): String@@PasswordHash

  def grantRole(userId: Int@@UserID, role: String@@Role): Unit
  def getRoles(userId: Int@@UserID): Seq[String@@Role]
  def revokeRole(userId: Int@@UserID, role: String@@Role): Unit

}

trait WorkflowApi {

  def defineWorkflow(slug: String, desc: String, targetLabel: Label, curatedLabels: Seq[Label]): String@@WorkflowID
  def activateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]
  def deactivateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]
  def deleteWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]

  def getWorkflows(): Seq[String@@WorkflowID]
  def getWorkflow(workflowId:String@@WorkflowID): R.WorkflowDef
  def getWorkflowReport(workflowId:String@@WorkflowID): WorkflowReport

  def lockUnassignedZones(userId: Int@@UserID, workflowId: String@@WorkflowID, count: Int): Seq[Int@@ZoneLockID]
  def updateZoneStatus(zoneLockId: Int@@ZoneLockID, newStatus: String@@StatusCode): Unit
  def releaseZoneLock(zoneLockId: Int@@ZoneLockID): Unit

  def getZoneLock(zoneLockId: Int@@ZoneLockID): Option[R.ZoneLock]
  def getLockForZone(zoneId: Int@@ZoneID): Option[Int@@ZoneLockID]
  def getLockedZones(userId: Int@@UserID): Seq[Int@@ZoneLockID]

}
