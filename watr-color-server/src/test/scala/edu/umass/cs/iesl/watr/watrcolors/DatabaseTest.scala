package edu.umass.cs.iesl.watr
package watrcolors

import org.scalatest._
import corpora._
import corpora.database._
import workflow._

trait DatabaseTest extends FlatSpec with Matchers with CorpusTestingUtil with BeforeAndAfterEach {

  lazy val reflowDB = new CorpusAccessDB(
    dbname="watrdev",
    dbuser="watrworker",
    dbpass="watrpasswd"
  )

  def workflowApi: WorkflowApi = reflowDB.workflowApi
  def userbaseApi: UserbaseApi = reflowDB.userbaseApi

  def createEmptyDocumentZoningApi(): DocumentZoningApi = {
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
    // println("closing db connections")
    // reflowDB.shutdown()
  }
}
