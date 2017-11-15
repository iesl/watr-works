package edu.umass.cs.iesl.watr
package corpora
package database

import workflow._
// import watrmarks.{StandardLabels => LB}
import watrmarks.Label
import TypeTags._
import textgrid._

class ZoneLockingTest extends DatabaseTest with TextGridBuilder {
  behavior of "Zone lock/unlock + basic user support"

  val VisualLine: Label = Label.auto

  def initWorkflow(): String@@WorkflowID = {
    val workflowId = workflowApi.defineWorkflow("wf-slug-0", "sample labeling task", VisualLine)
    val workflow = workflowApi.getWorkflow(workflowId)
    println(workflow)

    workflowId
  }

  def initUsers(n: Int): Seq[Int@@UserID] = {
    0 until n map { i =>
      userbaseApi.addUser(s"user${i}@umass.edu")
    }
  }


  it should "handle user creation" in new CleanDocstore {
    for {
      (userId, i)   <- initUsers(10).zipWithIndex
      user0 <- userbaseApi.getUser(userId)
      userIdByEmail <- userbaseApi.getUserByEmail(s"user${i}@umass.edu")
      user1 <- userbaseApi.getUser(userIdByEmail)
    } {
      user0 shouldEqual(user1)
    }

    userbaseApi.getUserByEmail("noone@zz.com") shouldBe None
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

  it should "define, activate, deactivate workflows" in new CleanDocstore {
  }

  it should "return lock status info for zones" in new CleanDocstore {
    addSampleDocs(1)
    initWorkflow()


  }

  it should "lock/unlock target zones, to exhaustion, with single user" in new CleanDocstore {
    addSampleDocs(1)
    val userId = initUsers(1).head
    val workflowId = initWorkflow()

    val locks = workflowApi.lockUnassignedZones(userId, workflowId, 3)
      .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

    locks.length shouldBe 3

    workflowApi.getLockedZones(userId).length shouldBe 3

    val locks2 = workflowApi.lockUnassignedZones(userId, workflowId, 6)
      .map(zoneLockId => workflowApi.getZoneLock(zoneLockId))

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

}
