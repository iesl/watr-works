package edu.umass.cs.iesl.watr
package workflow

import corpora._
import TypeTags._

import RelationModel._

object WorkflowStatus {
  val OnHold    = StatusCode("OnHold")
  val Active    = StatusCode("Active")
  val Complete  = StatusCode("Complete")
}

object ZoneLockStatus {
  val Unexamined = StatusCode("Unexamined")
  val Examined   = StatusCode("Examined")
  val Specified  = StatusCode("Specified")
  val Complete   = StatusCode("Complete")
  val Error      = StatusCode("Error")
}


trait UserbaseApi {
  def addUser(email: String): Int@@UserID
  def getUser(userId: Int@@UserID): Option[Person]
  def getUserByEmail(email: String): Option[Int@@UserID]
}

// trait WorkflowApi extends UserbaseApi with DocumentZoningApi {
trait WorkflowApi {

  def defineWorkflow(slug: String, desc: String): String@@WorkflowID
  def activateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]
  def deactivateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]
  def deleteWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]

  def getWorkflows(): Seq[String@@WorkflowID]
  def getWorkflow(workflowId:String@@WorkflowID): WorkflowDef

  // Zone-level locking/status
  def makeLockGroup(user: Int@@UserID): Int@@LockGroupID
  def aquireZoneLocks(lockGroup: Int@@LockGroupID, labelId: Int@@LabelID, count: Int): Seq[Int@@ZoneLockID]
  def aquireZoneLocksWithStatus(lockGroupId: Int@@LockGroupID, withStatus: String@@StatusCode, count: Int): Seq[Int@@ZoneLockID]
  def updateZoneStatus(zoneLockId: Int@@ZoneLockID, newStatus: String@@StatusCode): Unit
  def releaseZoneLocks(lockGroupId: Int@@LockGroupID): Unit

  def getZoneLock(zoneLockId: Int@@ZoneLockID): Option[ZoneLock]
  def getUserLockGroup(userId: Int@@UserID): Option[Int@@LockGroupID]
  def getLockForZone(zoneId: Int@@ZoneID): Option[ZoneLock]
  def getLockedZones(lockGroupId: Int@@LockGroupID): Seq[Int@@ZoneLockID]

}
