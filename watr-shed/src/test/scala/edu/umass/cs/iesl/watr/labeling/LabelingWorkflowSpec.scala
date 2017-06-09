package edu.umass.cs.iesl.watr
package labeling


// import scalaz._, Scalaz._
// import scalaz.concurrent.Task

import org.scalatest._
import watrmarks.{StandardLabels => LB}
import corpora._
import workflow._


class LabelingWorkflowSpec extends FlatSpec with Matchers {
  behavior of "Workflow"
  def workflowApi: WorkflowApi = ???
  def someUser: Int@@UserID = ???



  it should "walk through basic functionality" in {

    // val workflowId = workflowApi.defineWorkflow(
    //   "meta5",
    //   "Coarse-grained labeling for headers+reference section blocks",
    //   LB.DocumentPages,
    //   Seq(LB.Authors, LB.Affiliations)
    // )

    // val workflowDef = workflowApi.getWorkflow(workflowId)
    // Initialize workflow entries to zone:Unexamined, workflow to Ready
    // workflowApi.activateWorkflow(workflowId)

  }

}
