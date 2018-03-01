package edu.umass.cs.iesl.watr
package workflow

import corpora.database.DatabaseTest
import corpora.DocumentZoningApi

import TypeTags._

trait CorpusLockingTestHelpers extends UserbaseTestHelpers {
  def corpusLockApi: CorpusLockingApi
  def docStore: DocumentZoningApi

  val lockPath = CorpusPath("Root.Group.SubGroup")
  val defaultCorpusSize = 10

  def initCorpusLocks(n: Int): Unit = {
    for {
      _ <- 0 until n
      stableId <- docStore.getDocuments()
      docId <- docStore.getDocument(stableId)
    } {
      corpusLockApi.createLock(docId, lockPath)
    }
  }

  def getLocksWithStatus(s: String@@StatusCode): Seq[Int@@LockID] = {
    corpusLockApi.getLocks().filter { lockId =>
      corpusLockApi.getLockRecord(lockId).exists { rec =>
        rec.status == s
      }
    }
  }


}

class CorpusLockingApiSpec extends DatabaseTest with CorpusLockingTestHelpers {

  def doDefaultSetup(): Unit = {
    addDummyDocs(defaultCorpusSize)
    initUsers(3)
    initCorpusLocks(1)
  }

  def getUser0(): Int@@UserID = {
    userbaseApi.getUsers().head
  }

  behavior of "Corpus locks"

  it should "create one or more locks per document" in new EmptyDatabase {
    addDummyDocs(defaultCorpusSize)
    initCorpusLocks(1)

    // Every document has 1 lock
    val locks1 = for {
      stableId <- docStore.getDocuments()
      docId <- docStore.getDocument(stableId)
    } yield { corpusLockApi.getDocumentLocks(docId) }

    locks1.flatten.length shouldBe defaultCorpusSize

    corpusLockApi.getLocks().length shouldBe defaultCorpusSize

    initCorpusLocks(1)

    corpusLockApi.getLocks().length shouldBe defaultCorpusSize*2

    {
      corpusLockApi.getLocks().zipWithIndex
        .foreach{ case (lockId, i) =>
          corpusLockApi.getLocks().length shouldBe (defaultCorpusSize*2)-i
          corpusLockApi.deleteLock(lockId)
        }
      corpusLockApi.getLocks().length shouldBe 0
    }

  }

  it should "acquire/release user locks" in new EmptyDatabase {
    doDefaultSetup()
    val userIds = userbaseApi.getUsers()

    for {
      i           <- (0 until (defaultCorpusSize * 3)).toStream
      userId       = userIds( i % userIds.length )
      // _            = println(s"${i}. User ${userId} attempting lock")
      lockId      <- corpusLockApi.acquireLock(userId, lockPath)
      // _            = println(s"      success ${i} ")
      lock        <- corpusLockApi.getLockRecord(lockId)
    } {
      getLocksWithStatus(CorpusLockStatus.Locked).length shouldBe i+1
    }

    // Users may only hold one lock at a time
    getLocksWithStatus(CorpusLockStatus.Locked).length shouldBe userIds.length

    corpusLockApi.acquireLock(userIds.head, lockPath).isEmpty shouldBe true

    for {
      // (lockId, i) <- corpusLockApi.getLocks().zipWithIndex
      (lockId, i) <- getLocksWithStatus(CorpusLockStatus.Locked).zipWithIndex
    } {
      getLocksWithStatus(CorpusLockStatus.Completed).length shouldBe i
      corpusLockApi.releaseLock(lockId)
    }

    getLocksWithStatus(CorpusLockStatus.Completed).length shouldBe userIds.length
    getLocksWithStatus(CorpusLockStatus.Available).length shouldBe defaultCorpusSize - userIds.length
    getLocksWithStatus(CorpusLockStatus.Locked).length shouldBe 0


  }

  it should "not allow user to acquire more than one lock at a time" in new EmptyDatabase {
    doDefaultSetup()
    val user0 = getUser0()

    for {
      i           <- (0 until defaultCorpusSize).toStream
      lockId      <- corpusLockApi.acquireLock(user0, lockPath)
    } {
      getLocksWithStatus(CorpusLockStatus.Locked).length shouldBe i+1
    }

    getLocksWithStatus(CorpusLockStatus.Locked).length shouldBe 1
    getLocksWithStatus(CorpusLockStatus.Available).length shouldBe defaultCorpusSize - 1

  }

  it should "not allow user to lock same doc more than once" in new EmptyDatabase {
    val corpusSize = 2
    addDummyDocs(corpusSize)
    initCorpusLocks(2) // 2 locks per document

    val user0 = initUsers(1).head
    // There are 2 docs, 2 locks/doc, so 4 locks available, but
    //   a user can only lock a particular document once, so a particular
    //   user may only acquire (and release) 2 locks (in this case) before they are exhausted
    for {
      i           <- (0 until 100).toStream
      lockId      <- corpusLockApi.acquireLock(user0, lockPath)
    } {
      corpusLockApi.releaseLock(lockId)
    }

    getLocksWithStatus(CorpusLockStatus.Locked).length shouldBe 0
    getLocksWithStatus(CorpusLockStatus.Completed).length shouldBe 2
    getLocksWithStatus(CorpusLockStatus.Available).length shouldBe 2
  }
}
