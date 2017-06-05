package edu.umass.cs.iesl.watr
package labeling

import watrmarks.Label
import TypeTags._


// This is a replacement for document labeler identifier
case class LabelWidgetConfig(
  zone: Int@@ZoneID,
  regions: Seq[Int@@RegionID],
  targetlabels : List[Int@@LabelID],
  paginationStart: Int,
  paginationIncrement: Int
)

case class WorkflowDef(
  workflow     : Int@@WorkflowID,
  description  : String,
  slug         : String,
  sourcelabel  : Int@@LabelID,
  targetlabels : List[Int@@LabelID],
  status       : Int@@StatusCode
)
object WorkflowDef {
  object Status {
    val New       = StatusCode(1)
    val Ready     = StatusCode(2)
    val Paused    = StatusCode(3)
    val Active    = StatusCode(4)
    val Complete  = StatusCode(5)
  }
}

// This determines the granularity of locking labeling targets wrt a user/labeler
case class WorkItem(
  entry          : Int@@WorkItemID,
  workflow       : Int@@WorkflowID,
  zone           : Int@@ZoneID,
  targetregion   : Int@@RegionID,
  label          : Int@@LabelID,
  status         : Int@@StatusCode,
  user           : Option[Int@@UserID]
)
object WorkItem {
  object Status {
    val Acquired   = StatusCode(1)
    val Examined   = StatusCode(2)
    val Specified  = StatusCode(3)
    val Complete   = StatusCode(4)
    val Error      = StatusCode(5)
  }
}

case class WorkflowBatch(
  entries: Seq[WorkItem]
)


trait WorkflowApi {
  def defineWorkflow(slug: String, desc: String, srcLabel: Label, targetLabels: Seq[Label]): Int@@WorkflowID
  def activateWorkflow(workflowId:Int@@WorkflowID): Either[String, Unit]
  def deactivateWorkflow(workflowId:Int@@WorkflowID): Either[String, Unit]
  def deleteWorkflow(workflowId:Int@@WorkflowID): Either[String, Unit]


  def getWorkflow(workflowId:Int@@WorkflowID): WorkflowDef

  def getWorkflows(): Seq[WorkflowDef]
  def getWorkflows(status: Int@@StatusCode): Seq[WorkflowDef]

  def getEntries(workflowId: Int@@WorkflowID): Seq[WorkItem]

  def assignWork(workflowId: Int@@WorkflowID, user: Int@@UserID, batchSize: Int): WorkflowBatch
  def getAssignedWork(user: Int@@UserID): WorkflowBatch
  def commitWork(workItemId: Int@@WorkItemID, status: Int@@StatusCode): WorkflowBatch



}
