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

  val dryRunPath = CorpusPath("BioarxivPmid.Dryrun")

  def initWorkflows(n: Int): Seq[String@@WorkflowID] = {
    0 until n map { i =>
      workflowApi.defineWorkflow(
        s"curation-workflow-${i}",
        s"sample labeling task $i",
        Some(VisualLine),
        testSchema,
        dryRunPath,
        3
      )
    }
  }

  def addSampleDocs(n: Int): Seq[String@@DocumentID] = {
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr"
    )
      (0 until n).map{ i =>
        val stableId = DocumentID(s"doc#${i}")
        addDocument(stableId, doc)
        stableId
      }
  }


  it should "define, activate, deactivate workflows" in new EmptyDatabase {
    val workflows = initWorkflows(10)
    val workflowIds = workflowApi.getWorkflows()

    workflows.length shouldBe workflowIds.length
  }

  it should "lock/unlock target zones, to exhaustion, with single user" in new EmptyDatabase {
    addSampleDocs(1)
    val userId = initUsers(1).head
    val workflowId = initWorkflows(1).head

    println(workflowApi.getWorkflowReport(workflowId))

    val locks = workflowApi.lockUnassignedZones(userId, workflowId, 3)
      .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

    locks.length shouldBe 3

    workflowApi.getWorkflowReport(workflowId) shouldBe WorkflowReport(
      6, Map(
        (ZoneLockStatus.Assigned, 3),
        (ZoneLockStatus.InProgress, 0),
        (ZoneLockStatus.Completed, 0),
        (ZoneLockStatus.Skipped, 0)
      ),
      Map((userId, 3)),
      userMap()
    )

    workflowApi.getLockedZones(userId).length shouldBe 3

    val locks2 = workflowApi.lockUnassignedZones(userId, workflowId, 6)
      .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

    workflowApi.getWorkflowReport(workflowId) shouldBe WorkflowReport(
      0, Map(
        (ZoneLockStatus.Assigned, 9),
        (ZoneLockStatus.Completed, 0),
        (ZoneLockStatus.InProgress, 0),
        (ZoneLockStatus.Skipped, 0)
      ), Map(
        (userId, 9)
      ), userMap()
    )

    locks2.length shouldBe 6

    workflowApi.getLockedZones(userId).length shouldBe 9

    val locks3 = workflowApi.lockUnassignedZones(userId, workflowId, 1)
      .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

    locks3.length shouldBe 0

    workflowApi.getLockedZones(userId).length shouldBe 9

    for {
      maybeLock <- locks
      lock <- maybeLock
    } workflowApi.releaseZoneLock(lock.id)

    workflowApi.getLockedZones(userId).length shouldBe 6

    for {
      maybeLock <- locks2
      lock <- maybeLock
    } workflowApi.releaseZoneLock(lock.id)

    workflowApi.getLockedZones(userId).length shouldBe 0

  }

  it should "handle multiple workflows, users" in new EmptyDatabase {
    addSampleDocs(3) // 9 zones/doc, so 27 total zones
    val users = initUsers(3)
    val workflows = initWorkflows(3)

    workflowApi.lockUnassignedZones(users(0), workflows(0), 3)
    workflowApi.getLockedZones(users(0)).length shouldBe 3

    workflowApi.lockUnassignedZones(users(1), workflows(1), 3)
    workflowApi.getLockedZones(users(1)).length shouldBe 3
    workflowApi.getLockedZones(users(0)).length shouldBe 3

    { val report = workflowApi.getWorkflowReport(workflows(0))

      report.unassignedCount shouldBe 21

      report.statusCounts shouldBe Map(
        (ZoneLockStatus.Assigned, 6),
        (ZoneLockStatus.InProgress, 0),
        (ZoneLockStatus.Completed, 0),
        (ZoneLockStatus.Skipped, 0)
      )
    }

    workflowApi.getLockedZones(users(0)).foreach { zoneLockId =>
      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Completed)
    }
    workflowApi.getLockedZones(users(1)).foreach { zoneLockId =>
      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Skipped)
      workflowApi.releaseZoneLock(zoneLockId)
    }

    { val report = workflowApi.getWorkflowReport(workflows(0))

      report.unassignedCount shouldBe 21

      report.statusCounts shouldBe Map(
        (ZoneLockStatus.Assigned, 0),
        (ZoneLockStatus.InProgress, 0),
        (ZoneLockStatus.Completed, 3),
        (ZoneLockStatus.Skipped, 3)
      )
    }

  }

}
