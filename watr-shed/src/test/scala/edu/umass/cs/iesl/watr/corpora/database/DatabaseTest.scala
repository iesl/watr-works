package edu.umass.cs.iesl.watr
package corpora
package database

import org.scalatest._
import workflow._

trait DatabaseTest extends FlatSpec with Matchers with CorpusTestingUtil
    with BeforeAndAfterEach
    with BeforeAndAfterAll {


  lazy val reflowDB = new CorpusAccessDB(
    dbname="watrdev",
    dbuser="watrworker",
    dbpass="watrpasswd"
  )

  def workflowApi: WorkflowApi = reflowDB.workflowApi
  def userbaseApi: UserbaseApi = reflowDB.userbaseApi
  def corpusDirectory: DatabaseCorpusDirectory = new DatabaseCorpusDirectory()(reflowDB)

  def createEmptyDocumentZoningApi(): DocumentZoningApi = {
    reflowDB.runqOnce {
      reflowDB.veryUnsafeDropDatabase().run
    }

    reflowDB.dropAndRecreate
    reflowDB.docStore
  }

  override def beforeAll(): Unit = {
    println("beforeAll: re-initing db connections")
    reflowDB.reinit()
  }

  override def afterAll(): Unit = {
    println("afterAll: re-initing db connections")
    reflowDB.reinit()
  }
  override def beforeEach(): Unit = {
    println("beforeEach")
    // reflowDB.reinit()
  }

  override def afterEach(): Unit = {
  }
}
