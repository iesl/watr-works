package edu.umass.cs.iesl.watr
package corpora
package database

import org.scalatest._
import workflow._

trait DatabaseTest extends FlatSpec with Matchers with CorpusTestingUtil
    with BeforeAndAfterEach
    with BeforeAndAfterAll {


  lazy val corpusAccessDB = new CorpusAccessDB(
    dbname="watrdev",
    dbuser="watrworker",
    dbpass="watrpasswd"
  )

  def workflowApi: WorkflowApi = corpusAccessDB.workflowApi
  def annotApi: DocumentAnnotationApi = corpusAccessDB.annotApi
  def userbaseApi: UserbaseApi = corpusAccessDB.userbaseApi
  def corpusLockApi: CorpusLockingApi = corpusAccessDB.corpusLockApi
  def corpusDirectory: DatabaseCorpusDirectory = new DatabaseCorpusDirectory()(corpusAccessDB)

  def createEmptyDocumentZoningApi(): DocumentZoningApi = {
    corpusAccessDB.runqOnce {
      corpusAccessDB.veryUnsafeDropDatabase().run
    }

    corpusAccessDB.dropAndRecreate
    corpusAccessDB.docStore
  }

  override def beforeAll(): Unit = {
    println("beforeAll: re-initing db connections")
    // corpusAccessDB.reinit()
  }

  override def afterAll(): Unit = {
    println("afterAll: shutting down db connections")
    corpusAccessDB.shutdown()
  }
  override def beforeEach(): Unit = {
    // corpusAccessDB.reinit()
    // println("beforeEach")
    // corpusAccessDB.reinit()
  }

  override def afterEach(): Unit = {
  }
}
