package edu.umass.cs.iesl.watr
package workflow

import TypeTags._
import corpora.database.DatabaseTest
import watrmarks.{DocSegLabels, Label, LabelSchema, LabelSchemas}

class WorkflowApiSpec extends DatabaseTest with UserbaseTestHelpers with DocSegLabels {

  val Math: Label = Label.auto

  val testSchema = LabelSchemas(LabelSchemaName("TestSchema"), List(
    LabelSchema(Sup),
    LabelSchema(Sub),
    LabelSchema(Math)
  ))

  val dryRunPath = CorpusPath("Root.A")

  def initWorkflows(n: Int): Seq[String@@WorkflowID] = {
    annotApi.createLabelSchema(testSchema)

    (0 until n) map { i =>
      workflowApi.defineWorkflow(
        s"curation-workflow-${i}",
        testSchema.name,
        dryRunPath
      )
    }
  }

  def addSampleDocs(n: Int): Seq[String@@DocumentID] = {
    corpusDirectory.makeDirectory(dryRunPath)
    val docs = addDummyDocs(n)
    docs.foreach { stableId =>
      corpusDirectory.moveEntry(stableId, dryRunPath)
    }
    docs
  }



  it should "define, activate, deactivate workflows" in new EmptyDatabase {
    val workflows = initWorkflows(10)
    val workflowIds = workflowApi.getWorkflows()

    workflows.length shouldBe workflowIds.length
  }

  def printWorkflowReport(id: String@@WorkflowID): Unit = {
    val workflowsWithReports = workflowApi.getWorkflows.map{  id =>
      (workflowApi.getWorkflowReport(id), workflowApi.getWorkflow(id))
    }
    println(WorkflowReport.prettyPrint(workflowsWithReports))
  }

}
