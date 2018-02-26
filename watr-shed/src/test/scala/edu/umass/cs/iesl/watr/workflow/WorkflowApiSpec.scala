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

  it should "lock/unlock target zones, to exhaustion, with single user" in new EmptyDatabase {
    val corpusSize = 2
    addSampleDocs(corpusSize)
    val userIds = initUsers(3)
    val workflowId = initWorkflows(1).head

    for {
      i           <- 1 to (corpusSize * 3)
      userId      <- userIds
      zoneLockId  <- workflowApi.lockUnassignedZones(userId, workflowId)
      lock        <- workflowApi.getZoneLock(zoneLockId)
    } {
      println(s"Workflow ${i}: $lock")
      printWorkflowReport(workflowId)

      // workflowApi.getWorkflowReport(workflowId) shouldBe WorkflowReport(
      //   corpusSize-i, Map(
      //     (ZoneLockStatus.Assigned, 1),
      //     (ZoneLockStatus.InProgress, 0),
      //     (ZoneLockStatus.Completed, i-1),
      //     (ZoneLockStatus.Skipped, 0)
      //   ),
      //   Map((userId, 1)),
      //   userMap()
      // )

      // workflowApi.getLockedZones(userId).length shouldBe 1

      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Completed)
      workflowApi.releaseZoneLock(zoneLockId)

      // workflowApi.getLockedZones(userId).length shouldBe 0
    }

  }

  // it should "handle multiple workflows, users" in new EmptyDatabase {
  //   addSampleDocs(3) // 9 zones/doc, so 27 total zones
  //   val users = initUsers(3)
  //   val workflows = initWorkflows(3)

  //   workflowApi.lockUnassignedZones(users(0), workflows(0))
  //   workflowApi.getLockedZones(users(0)).length shouldBe 3

  //   workflowApi.lockUnassignedZones(users(1), workflows(1))
  //   workflowApi.getLockedZones(users(1)).length shouldBe 3
  //   workflowApi.getLockedZones(users(0)).length shouldBe 3

  //   { val report = workflowApi.getWorkflowReport(workflows(0))

  //     report.unassignedCount shouldBe 21

  //     report.statusCounts shouldBe Map(
  //       (ZoneLockStatus.Assigned, 6),
  //       (ZoneLockStatus.InProgress, 0),
  //       (ZoneLockStatus.Completed, 0),
  //       (ZoneLockStatus.Skipped, 0)
  //     )
  //   }

  //   workflowApi.getLockedZones(users(0)).foreach { zoneLockId =>
  //     workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Completed)
  //   }
  //   workflowApi.getLockedZones(users(1)).foreach { zoneLockId =>
  //     workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Skipped)
  //     workflowApi.releaseZoneLock(zoneLockId)
  //   }

  //   { val report = workflowApi.getWorkflowReport(workflows(0))

  //     report.unassignedCount shouldBe 21

  //     report.statusCounts shouldBe Map(
  //       (ZoneLockStatus.Assigned, 0),
  //       (ZoneLockStatus.InProgress, 0),
  //       (ZoneLockStatus.Completed, 3),
  //       (ZoneLockStatus.Skipped, 3)
  //     )
  //   }

  // }

}
