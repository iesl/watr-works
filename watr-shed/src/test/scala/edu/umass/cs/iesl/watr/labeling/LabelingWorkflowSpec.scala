package edu.umass.cs.iesl.watr
package labeling


// import scalaz._, Scalaz._
// import scalaz.concurrent.Task

import org.scalatest._
import watrmarks.{StandardLabels => LB}


class LabelingWorkflowSpec extends FlatSpec with Matchers {
  // Walkthrough..
  behavior of "Workflow"
  // Create a list of known Workflows
  def workflowApi: WorkflowApi = ???
  // import Workflow.WorkflowStatus
  def someUser: Int@@UserID = ???
  // import Workflow.EntryStatus



  it should "walk through basic functionality" in {

    val workflowId = workflowApi.defineWorkflow(
      "meta5",
      "Coarse-grained labeling for headers+reference section blocks",
      LB.Pages,
      Seq(LB.Authors, LB.Affiliations)
    )

    // val workflowDef = workflowApi.getWorkflow(workflowId)

    // Initialize workflow entries to zone:Unexamined, workflow to Ready
    workflowApi.activateWorkflow(workflowId)

    val workBatch = workflowApi.assignWork(workflowId, someUser, 10)




  }
  it should "lock a set of zones for a user to annotate" in {

  }

}
