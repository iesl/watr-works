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
  targetlabels : List[Int@@LabelID],
  status       : Int@@StatusCode
)

case class WorkflowEntry(
  workflow       : Int@@WorkflowID,
  zone           : Int@@ZoneID,
  targetregion   : Option[Int@@RegionID], //if None then status applies to entire zone
  label          : Option[Int@@LabelID], // if None then status applies to every target label
  status         : Int@@StatusCode,
  user           : Option[Int@@UserID]
)

object Workflow {
  object WorkflowStatus {
    val New       = StatusCode(1)
    val Ready     = StatusCode(2)
    val Paused    = StatusCode(3)
    val Active    = StatusCode(4)
    val Complete  = StatusCode(5)
  }

  object EntryStatus {
    val Unexamined = StatusCode(1)
    val Examined   = StatusCode(2)
    val Specified  = StatusCode(3)
    val Complete   = StatusCode(4)
    val Error      = StatusCode(5)
  }
}

trait WorkflowApi {
  // Define the workflow, but don't init any items
  def createWorkflow(slug: String, desc: String, labels: Seq[Label]): Int@@WorkflowID

  // Move workflows through various states
  // Create initial Workflow Entries from documents/zone in db
  def initWorkflow(workflowId:Int@@WorkflowID): Unit

  // Status setters
  def activateWorkflow(workflowId:Int@@WorkflowID): Unit
  def pauseWorkflow(workflowId:Int@@WorkflowID): Unit
  def completeWorkflow(workflowId:Int@@WorkflowID): Unit


  def getWorkflow(workflowId:Int@@WorkflowID): WorkflowDef

  def getWorkflows(): Seq[WorkflowDef]
  def getWorkflows(status: Int@@StatusCode): Seq[WorkflowDef]

  def getEntries(workflowId: Int@@WorkflowID): Seq[WorkflowEntry]

}
