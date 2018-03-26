package edu.umass.cs.iesl.watr
package watrcolors

import org.scalatest._
import corpora._
import corpora.database._
import workflow._

trait DatabaseBaseTest extends Matchers with CorpusTestingUtil {
  lazy val corpusAccessDB = new CorpusAccessDB(
    dbname="watrdev",
    dbuser="watrworker",
    dbpass="watrpasswd"
  )

  def workflowApi: WorkflowApi = corpusAccessDB.workflowApi
  def userbaseApi: UserbaseApi = corpusAccessDB.userbaseApi
  def annotApi: DocumentAnnotationApi = corpusAccessDB.annotApi
  def corpusLockApi: CorpusLockingApi = corpusAccessDB.corpusLockApi

  def createEmptyDocumentZoningApi(): DocumentZoningApi = {
    println("Dropping/recreating DB tables")
    corpusAccessDB.runqOnce {
      corpusAccessDB.veryUnsafeDropDatabase().run
    }

    corpusAccessDB.dropAndRecreate
    corpusAccessDB.docStore
  }

}

trait DatabaseFreeSpec extends FreeSpec with DatabaseBaseTest with BeforeAndAfterEach {
  override def beforeEach(): Unit = {
    corpusAccessDB.reinit()
  }

  override def afterEach(): Unit = {}
}

trait DatabaseTest extends FlatSpec with DatabaseBaseTest with BeforeAndAfterEach {

  override def beforeEach(): Unit = {
    println("re-initing db connections")
    corpusAccessDB.reinit()
  }

  override def afterEach(): Unit = {
  }
}
