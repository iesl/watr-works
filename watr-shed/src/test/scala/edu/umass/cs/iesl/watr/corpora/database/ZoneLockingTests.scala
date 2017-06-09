package edu.umass.cs.iesl.watr
package corpora
package database

import workflow._
import watrmarks.{StandardLabels => LB}
import TypeTags._

class ZoneLockingTest extends DatabaseTest with PlainTextCorpus {
  behavior of "Zone lock/unlock + basic user support"

  it should "handle user creation" in new CleanDocstore {
    val userId = userbaseApi.addUser("acs@zz.com")
    val person = userbaseApi.getUser(userId)

    val p2 = userbaseApi.getUserByEmail("acs@zz.com")
      .flatMap(uid => userbaseApi.getUser(uid))

    person shouldEqual(p2)

    userbaseApi.getUserByEmail("noone@zz.com") shouldBe None

  }

  def add4pg_3x3SampleDoc(): String@@DocumentID = {
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr",
      "stu\nvwx\nyzz"
    )

    val stableId = DocumentID("doc#0")
    addDocument(stableId, doc)
    stableId
  }

  it should "lock/unlock zones for labeling" in new CleanDocstore {
    add4pg_3x3SampleDoc()
    val userId = userbaseApi.addUser("acs@zz.com")
    val labelId = docStore.ensureLabel(LB.VisualLine)
    val workflowId = workflowApi.defineWorkflow("wf-slug-0", "sample labeling task")
    val workflow = workflowApi.getWorkflow(workflowId)
    println(workflow)

    {
      val lockGroupId = workflowApi.makeLockGroup(userId)
      workflowApi.getUserLockGroup(userId).isDefined shouldBe true

      val locks = workflowApi.aquireZoneLocks(lockGroupId, labelId, 10)
        .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

      locks.length shouldBe 10

      workflowApi.getLockedZones(lockGroupId).length shouldBe 10

      val locks2 = workflowApi.aquireZoneLocks(lockGroupId, labelId, 14)
        .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

      locks2.length shouldBe 2

      workflowApi.getLockedZones(lockGroupId).length shouldBe 12

      workflowApi.releaseZoneLocks(lockGroupId)

      workflowApi.getUserLockGroup(userId).isDefined shouldBe false

    }
    {
      val lockGroupId = workflowApi.makeLockGroup(userId)
      workflowApi.getUserLockGroup(userId).isDefined shouldBe true

      val locks = workflowApi.aquireZoneLocks(lockGroupId, labelId, 10)
        .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

      locks.length shouldBe 0

      workflowApi.releaseZoneLocks(lockGroupId)
    }
    {
      val lockGroupId = workflowApi.makeLockGroup(userId)

      val locks = workflowApi.aquireZoneLocksWithStatus(lockGroupId, ZoneLockStatus.Unexamined, 3)
        .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

      locks.length shouldBe 3

      workflowApi.aquireZoneLocksWithStatus(lockGroupId, ZoneLockStatus.Unexamined, 3)
        .foreach { zoneLockId =>
          workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Specified)
        }

      workflowApi.aquireZoneLocksWithStatus(lockGroupId, ZoneLockStatus.Specified, 10).length shouldBe 6

      workflowApi.releaseZoneLocks(lockGroupId)
    }

  }



}
