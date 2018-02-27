package edu.umass.cs.iesl.watr
package workflow

import TypeTags._
import corpora.database.DatabaseTest
import textgrid.TextGridBuilder
import watrmarks.{DocSegLabels, Label, LabelSchema, LabelSchemas}


class CorpusLockingApiSpec extends DatabaseTest with TextGridBuilder with UserbaseTestHelpers with DocSegLabels {

  behavior of "Corpus locks"

  val doc = List(
    "abc\ndef\nghi",
    "012\n345\n678",
    "jkl\nmno\npqr"
  )


  // corpusLockApi
  it should "lock/unlock documents" in new EmptyDatabase {
    val corpusSize = 10
    val docIds = (0 until 4).foreach{i => addSampleDoc(doc)}

    val userIds = initUsers(3)

    val lockReason = "Annotate::Headers"
    val lockIds = for {
      stableId <- docStore.getDocuments()
      docId <- docStore.getDocument(stableId)
    } yield {
      corpusLockApi.createLock(docId, lockReason)
    }

    lockIds.foreach { lockId =>
      val rec = corpusLockApi.getLockRecord(lockId)
      println(s"rec> ${rec}")
    }
    for {
      i           <- (1 to (corpusSize * 3)).toStream
      userId       = userIds( i % userIds.length )
      _            = println(s"${i}. User ${userId} attempting lock")
      lockId      <- corpusLockApi.acquireLock(userId, lockReason)
      lock        <- corpusLockApi.getLockRecord(lockId)
    } {
      val rec = corpusLockApi.getLockRecord(lockId)
      println(s"acquired> ${rec}")
    }


    // for {
    //   i           <- (1 to (corpusSize * 3)).toStream
    //   userId       = userIds( i % userIds.length )
    //   _            = println(s"${i}. User ${userId} attempting lock")
    //   zoneLockId  <- workflowApi.lockNextDocument(userId, workflowId)
    //   lock        <- corpusLockApi.getLockRecord(zoneLockId)
    // } {
    //   println(s"Workflow #${i}: $lock")
    //   // printWorkflowReport(workflowId)

    //   val report = workflowApi.getWorkflowReport(workflowId)

    //   report.unassignedCount shouldBe corpusSize - i

    //   report.statusCounts shouldBe Map(
    //     (CorpusLockStatus.Available, corpusSize - i),
    //     (CorpusLockStatus.Locked, 1),
    //     (CorpusLockStatus.Completed, i-1)
    //   )

    //   corpusLockApi.getUserLocks(userId).length shouldBe 1

    //   corpusLockApi.releaseLock(zoneLockId)

    //   corpusLockApi.getUserLocks(userId).length shouldBe 0
    // }

  }
}
