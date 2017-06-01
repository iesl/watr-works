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
  import Workflow.WorkflowStatus
  import Workflow.EntryStatus


  sealed trait WeekDay
  object WeekDay {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = new WeekDay {}
    val values: Set[WeekDay] = utils.EnumVals
  }


  WeekDay.values

  it should "walk through basic functionality" in {
    workflowApi.getWorkflows().length shouldBe 0

    val workflowId = workflowApi.createWorkflow(
      "meta5",
      "Coarse-grained labeling for headers+reference section blocks",
      Seq(LB.Authors, LB.Affiliation)
    )

    workflowApi.getWorkflows(WorkflowStatus.New).length shouldBe 1
    workflowApi.getWorkflows(WorkflowStatus.Ready).length shouldBe 0

    val workflowDef = workflowApi.getWorkflow(workflowId)

    workflowApi.initWorkflow(workflowId)

  }

  // List Workflows with outstanding items

  // Select next set of labeling targets for user

  // Assign workflow task to user

}
