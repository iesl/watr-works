package edu.umass.cs.iesl.watr
package workflow

import TypeTags._
import corpora.database.DatabaseTest
import textgrid.TextGridBuilder


class CorpusLockingApiSpec extends DatabaseTest with TextGridBuilder with UserbaseTestHelpers {

  behavior of "Corpus locks"

  def addSampleDocuments(n: Int): Seq[String@@DocumentID] = {
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr")

    (0 until n).map{ i =>
        val stableId = DocumentID(s"doc#${i}")
        addDocument(stableId, doc)
        stableId
      }
  }

  val lockReason = "Annotate::Headers"

  def initCorpus(n: Int): Unit = {

  }

  def initCorpusLocks(n: Int): Unit = {
    for {
      i <- 0 until n
      stableId <- docStore.getDocuments()
      docId <- docStore.getDocument(stableId)
    } {
      corpusLockApi.createLock(docId, lockReason)
    }
  }
  it should "create one or more locks per document" in new EmptyDatabase {
    val corpusSize = 10
    addSampleDocuments(corpusSize)

    initCorpusLocks(1)

    // Every document has 1 lock
    val locks1 = for {
      stableId <- docStore.getDocuments()
      docId <- docStore.getDocument(stableId)
    } yield { corpusLockApi.getDocumentLocks(docId) }

    locks1.flatten.length shouldBe corpusSize

    corpusLockApi.getLocks().length shouldBe corpusSize

    initCorpusLocks(1)

    corpusLockApi.getLocks().length shouldBe corpusSize*2

    {
      corpusLockApi.getLocks().zipWithIndex
        .foreach{ case (lockId, i) =>
          corpusLockApi.getLocks().length shouldBe (corpusSize*2)-i
          corpusLockApi.deleteLock(lockId)
        }
      corpusLockApi.getLocks().length shouldBe 0
    }

  }

  it should "acquire/release locks" in new EmptyDatabase {
    val corpusSize = 10
    val docIds =  addSampleDocuments(corpusSize)
    val userIds = initUsers(3)

    initCorpusLocks(1)

    for {
      i           <- (0 until (corpusSize * 3)).toStream
      userId       = userIds( i % userIds.length )
      _            = println(s"${i}. User ${userId} attempting lock")
      lockId      <- corpusLockApi.acquireLock(userId, lockReason)
      lock        <- corpusLockApi.getLockRecord(lockId)
    } {
      val rec = corpusLockApi.getLockRecord(lockId)
      println(s"acquired> ${rec}")
    }

    corpusLockApi.acquireLock(userIds.head, lockReason).isEmpty shouldBe true

    for {
      lockId <- corpusLockApi.getLocks()
    } {
      corpusLockApi.releaseLock(lockId)
    }


    val locks = corpusLockApi.getLocks()// .length shouldBe 0

    val completed = locks.filter { lockId =>
      corpusLockApi.getLockRecord(lockId).exists { rec =>
        rec.status == CorpusLockStatus.Completed
      }
    }
    completed.length shouldBe corpusSize

    val available = locks.filter { lockId =>
      corpusLockApi.getLockRecord(lockId).exists { rec =>
        rec.status == CorpusLockStatus.Available
      }
    }
    available.length shouldBe 0

    val locked = locks.filter { lockId =>
      corpusLockApi.getLockRecord(lockId).exists { rec =>
        rec.status == CorpusLockStatus.Locked
      }
    }

    locked.length shouldBe 0
  }

  it should "not allow user to acquire more than one lock at a time" in new EmptyDatabase {
  }

  it should "not allow user to lock same doc more than once" in new EmptyDatabase {
  }
}
