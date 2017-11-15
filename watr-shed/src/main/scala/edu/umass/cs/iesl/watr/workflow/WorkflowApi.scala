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
  *    A new workflow is defined, given a name and description, along with the zone label to be locked for further labeling.
  *    One or more zone locks are acquired for a user. The pool of candidate zones for locking include only those that
  *       have no lock status, i.e., have not been seen by any user
  *    When the user is finished adding labels to a particular zone, the lock status is changed to Completed or Skipped
  *
  *    The workflow API include a WorkflowReport, which included counts for all zones having a status, as well as unseen zone
  */

object WorkflowStatus {
  val OnHold    = StatusCode("OnHold")
  val Active    = StatusCode("Active")
  val Complete  = StatusCode("Complete")
}


object ZoneLockStatus {
  val Unassigned   = StatusCode("Unassigned")
  val Assigned     = StatusCode("Assigned")
  val Completed    = StatusCode("Completed")
  val Skipped      = StatusCode("Skipped")

  val all = List(Unassigned, Assigned, Completed, Skipped)
}

case class WorkflowReport(
  unassignedCount: Int,
  statusCounts: Map[String@@StatusCode, Int]
)

trait UserbaseApi {
  def addUser(email: String): Int@@UserID
  def getUser(userId: Int@@UserID): Option[R.Person]
  def getUserByEmail(email: String): Option[Int@@UserID]
}

trait WorkflowApi {

  def defineWorkflow(slug: String, desc: String, targetLabel: Label): String@@WorkflowID
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

  // Zone-level locking/status
  // def makeLockGroup(user: Int@@UserID, workflowId: String@@WorkflowID): Int@@LockGroupID
  // def acquireZoneLocksWithStatus(lockGroupId: Int@@LockGroupID, withStatus: String@@StatusCode, count: Int): Seq[Int@@ZoneLockID]
  // def getUserLockGroup(userId: Int@@UserID): Option[Int@@LockGroupID]


}
