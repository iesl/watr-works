package edu.umass.cs.iesl.watr
package workflow

import TypeTags._
import corpora.database.DatabaseTest
import textgrid.TextGridBuilder
import watrmarks.{DocSegLabels, Label, LabelSchema, LabelSchemas}

class WorkflowApiSpec extends DatabaseTest with TextGridBuilder with UserbaseTestHelpers with DocSegLabels {
  behavior of "Zone lock/unlock + basic user support"

  val Math: Label = Label.auto

  val testSchema = LabelSchemas(List(
    LabelSchema(Sup),
    LabelSchema(Sub),
    LabelSchema(Math)
  ))

  val dryRunPath = CorpusPath("Root.A")

  def initWorkflows(n: Int): Seq[String@@WorkflowID] = {
    0 until n map { i =>
      workflowApi.defineWorkflow(
        s"curation-workflow-${i}",
        s"sample labeling task $i",
        Some(FullPdf),
        testSchema,
        dryRunPath,
        2
      )
    }
  }

  def addSampleDocs(n: Int): Seq[String@@DocumentID] = {
    corpusDirectory.makeDirectory(dryRunPath)
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr"
    )
      (0 until n).map{ i =>
        val stableId = DocumentID(s"doc#${i}")
        addDocument(stableId, doc)
        corpusDirectory.moveEntry(stableId, dryRunPath)
        stableId
      }
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


  it should "lock/unlock target zones, to exhaustion" in new EmptyDatabase {
    val corpusSize = 2
    addSampleDocs(corpusSize)
    val userIds = initUsers(3)
    val workflowId = initWorkflows(1).head

    for {
      i           <- (1 to (corpusSize * 3)).toStream
      userId       = userIds( i % userIds.length )
      _            = println(s"${i}. User ${userId} attempting lock")
      zoneLockId  <- workflowApi.lockUnassignedZone(userId, workflowId)
      lock        <- workflowApi.getZoneLock(zoneLockId)
    } {
      println(s"Workflow #${i}: $lock")
      printWorkflowReport(workflowId)

      val report = workflowApi.getWorkflowReport(workflowId)

      report.unassignedCount shouldBe corpusSize - i

      report.statusCounts shouldBe Map(
        (ZoneLockStatus.Assigned, 1),
        (ZoneLockStatus.InProgress, 0),
        (ZoneLockStatus.Completed, i-1),
        (ZoneLockStatus.Skipped, 0)
      )


      workflowApi.getLockedZones(userId).length shouldBe 1

      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Completed)
      workflowApi.releaseZoneLock(zoneLockId)

      workflowApi.getLockedZones(userId).length shouldBe 0
    }

  }

  it should "annotate a locked document" in new EmptyDatabase {
    val corpusSize = 2
    addSampleDocs(corpusSize)
    val userIds = initUsers(3)
    val workflowId = initWorkflows(1).head
    val stableId0 = docStore.getDocuments(1, 0).head
    val docId0 = docStore.getDocument(stableId0).get
    val annotId = corpusAccessDB.createAnnotation(userIds(0),  docId0, workflowId)
    val annot = corpusAccessDB.getAnnotation(annotId)
    // corpusAccessDB.updateAnnotationJson(annotId, )
    corpusAccessDB.updateAnnotationStatus(annotId, StatusCode("NewStatus"))

    println(s"Annot = ${annot}")

    val annot1 = corpusAccessDB.getAnnotation(annotId)
    println(s"Annot 1 = ${annot1}")
  }

}
