package edu.umass.cs.iesl.watr
package watrcolors

import org.scalatest._
import corpora._
import corpora.database._
import workflow._

trait DatabaseBaseTest extends Matchers with CorpusTestingUtil {
  lazy val reflowDB = new CorpusAccessDB(
    dbname="watrdev",
    dbuser="watrworker",
    dbpass="watrpasswd"
  )

  def workflowApi: WorkflowApi = reflowDB.workflowApi
  def userbaseApi: UserbaseApi = reflowDB.userbaseApi

  def createEmptyDocumentZoningApi(): DocumentZoningApi = {
    println("Dropping/recreating DB tables")
    reflowDB.runqOnce {
      reflowDB.veryUnsafeDropDatabase().run
    }

    reflowDB.dropAndRecreate
    reflowDB.docStore
  }

}

trait DatabaseFreeSpec extends FreeSpec with DatabaseBaseTest with BeforeAndAfterEach {
  override def beforeEach(): Unit = {
    reflowDB.reinit()
  }

  override def afterEach(): Unit = {}
}

trait DatabaseTest extends FlatSpec with DatabaseBaseTest with BeforeAndAfterEach {

  override def beforeEach(): Unit = {
    println("re-initing db connections")
    reflowDB.reinit()
  }

  override def afterEach(): Unit = {
  }
}
