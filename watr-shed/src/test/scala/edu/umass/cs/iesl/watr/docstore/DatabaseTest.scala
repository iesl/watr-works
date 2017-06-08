package edu.umass.cs.iesl.watr
package docstore

import org.scalatest._
import corpora._

trait DatabaseTest extends FlatSpec with Matchers with CorpusTestingUtil with BeforeAndAfterEach {
  lazy val tables = new TextReflowDBTables()

  lazy val reflowDB = new TextReflowDB(
    tables,
    dbname="watrdev",
    dbuser="watrworker",
    dbpass="watrpasswd"
  )

  def workflowApi: WorkflowApi = reflowDB.workflowApi
  def userbaseApi: UserbaseApi = reflowDB.userbaseApi

  def createEmptyDocumentCorpus(): DocumentCorpus = {
    reflowDB.runqOnce {
      reflowDB.veryUnsafeDropDatabase().run
    }

    reflowDB.dropAndRecreate
    reflowDB.docStore
  }
  override def beforeEach(): Unit = {
    println("re-initing db connections")
    reflowDB.reinit()
  }
  override def afterEach(): Unit = {
    println("closing db connections")
    reflowDB.shutdown()
  }
}
