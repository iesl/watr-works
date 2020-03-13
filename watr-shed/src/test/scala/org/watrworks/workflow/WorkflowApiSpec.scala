package org.watrworks
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

  def dryRunPath(i: Int) = CorpusPath(s"Root.A.${i}")

  def initWorkflows(n: Int): Seq[String@@WorkflowID] = {
    annotApi.createLabelSchema(testSchema)

    (0 until n) map { i =>
      workflowApi.defineWorkflow(
        s"curation-workflow-${i}",
        testSchema.name,
        dryRunPath(i)
      )
    }
  }

  def getDocumentIds(): Seq[Int@@DocumentID] =  for {
    stableId <- docStore.getDocuments()
    docId <- docStore.getDocument(stableId)
  } yield docId


  def addSampleDocs(n: Int): Seq[String@@DocumentID] = {
    corpusDirectory.makeDirectory(dryRunPath(0))
    val docs = addDummyDocs(n)
    docs.foreach { stableId =>
      corpusDirectory.moveEntry(stableId, dryRunPath(0))
    }
    docs
  }

  def printWorkflowReport(id: String@@WorkflowID): Unit = {
    val workflowsWithReports = workflowApi.getWorkflows
      .filter { _ == id  }
      .map{  id => (workflowApi.getWorkflowReport(id), workflowApi.getWorkflow(id)) }
    println(WorkflowReport.prettyPrint(workflowsWithReports))
  }


  it should "define, activate, deactivate workflows" in new EmptyDatabase {
    val defaultCorpusSize = 10
    addSampleDocs(defaultCorpusSize)
    val workflows = initWorkflows(3)
    val workflowIds = workflowApi.getWorkflows()
    val userIds = initUsers(3)
    val user0 = userIds.head

    workflowIds.length shouldBe 3
    workflows.length shouldBe workflowIds.length

    getDocumentIds().foreach { docId =>
      corpusLockApi.createLock(docId, dryRunPath(0))
    }

    val workflow0 = workflowIds.head



    for {
      i           <- (0 until (defaultCorpusSize * 3)).toStream
      userId       = userIds( i % userIds.length )
      _            = println(s"${i}. User ${userId} attempting lock")
      lockId      <- corpusLockApi.acquireLock(userId, dryRunPath(0))
      _            = println(s"      success ${i} ")
      lock        <- corpusLockApi.getLockRecord(lockId)
    } {
      printWorkflowReport(workflow0)
      corpusLockApi.releaseLock(lockId)
    }

    workflowIds.foreach { workflowId =>
      printWorkflowReport(workflowId)
      workflowApi.deleteWorkflow(workflowId)
    }

    workflowApi.getWorkflows().length shouldBe 0

  }


}
