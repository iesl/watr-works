package edu.umass.cs.iesl.watr
package corpora
package database

import org.scalatest._
import workflow._

trait DatabaseTest extends FlatSpec with Matchers with CorpusTestingUtil with BeforeAndAfterEach {
  lazy val tables = new CorpusAccessDBTables()

  lazy val reflowDB = new CorpusAccessDB(
    tables,
    dbname="bioarxiv",
    dbuser="watrworker",
    dbpass="watrpasswd"
  )
  // lazy val reflowDB = new CorpusAccessDB(
  //   tables,
  //   dbname="watrdev",
  //   dbuser="watrworker",
  //   dbpass="watrpasswd"
  // )

  def workflowApi: WorkflowApi = reflowDB.workflowApi
  def userbaseApi: UserbaseApi = reflowDB.userbaseApi

  def createEmptyDocumentZoningApi(): DocumentZoningApi = {
    // reflowDB.runqOnce {
    //   reflowDB.veryUnsafeDropDatabase().run
    // }

    // reflowDB.dropAndRecreate
    reflowDB.docStore
  }
  override def beforeEach(): Unit = {
    println("re-initing db connections")
    // reflowDB.reinit()
  }
  override def afterEach(): Unit = {
    println("closing db connections")
    // reflowDB.shutdown()
  }
}
