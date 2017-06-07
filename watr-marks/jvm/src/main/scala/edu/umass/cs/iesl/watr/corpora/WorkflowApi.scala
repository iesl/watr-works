package edu.umass.cs.iesl.watr
package corpora

//
import watrmarks.Label
import scalaz.Tag

case class WorkflowDef(
  workflow     : String@@WorkflowID,
  description  : String,
  sourcelabel  : Int@@LabelID,
  targetlabels : List[Int@@LabelID],
  status       : Int@@WorkflowDef.StatusCode
)
object WorkflowDef {
  sealed trait StatusCode
  val StatusCode = Tag.of[StatusCode]
  object Status {
    val OnHold    = StatusCode(1)
    val Active    = StatusCode(2)
    val Complete  = StatusCode(3)
  }
}

case class LockGroup(
  id: Int@@LockGroupID,
  user: Int@@UserID
)

case class ZoneLock(
  id         : Int@@ZoneLockID,
  group      : Option[Int@@LockGroupID],
  zone       : Int@@ZoneID,
  status     : Int@@ZoneLock.StatusCode
)

object ZoneLock {
  sealed trait StatusCode
  val StatusCode = Tag.of[StatusCode]
  object Status {
    val Unexamined = StatusCode(1)
    val Examined   = StatusCode(2)
    val Specified  = StatusCode(3)
    val Complete   = StatusCode(4)
    val Error      = StatusCode(5)
  }
}


trait WorkflowApi {
  def defineWorkflow(slug: String, desc: String, srcLabel: Label, targetLabels: Seq[Label]): String@@WorkflowID
  def activateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]
  def deactivateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]
  def deleteWorkflow(workflowId:String@@WorkflowID): Either[String, Unit]

  def getWorkflows(): Seq[String@@WorkflowID]
  def getWorkflow(workflowId:String@@WorkflowID): WorkflowDef

  // Zone-level locking/status
  def makeLockGroup(user: Int@@UserID): Int@@LockGroupID
  def aquireZoneLocks(lockGroup: Int@@LockGroupID, withStatus: Int@@ZoneLock.StatusCode, labelId: Int@@LabelID, count: Int): Seq[ZoneLock]
  def updateZoneStatus(zoneLockId: Int@@ZoneLockID, newStatus: Int@@ZoneLock.StatusCode): Unit
  def releaseZoneLocks(lockGroupId: Int@@LockGroupID): Unit

  def getZoneLock(zoneId: Int@@ZoneID): Option[ZoneLock]
  def getUserLocks(userId: Int@@UserID): Seq[Int@@LockGroupID]
}
