package org.watrworks
package corpora
package database

import org.scalatest._
import workflow._

trait DatabaseTest extends AnyFlatSpec with Matchers with CorpusTestingUtil
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  import cats.effect._

  // import java.util.concurrent._
  import scala.concurrent._
  val MainCPUBound = ExecutionContext.global
  implicit val cs: ContextShift[IO] = IO.contextShift(MainCPUBound)

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
      for{
        _ <- corpusAccessDB.tables.dropAll()
        _ <- corpusAccessDB.tables.createAll()
      } yield ()
    }

    corpusAccessDB.docStore
  }

  // override def beforeAll(): Unit = {}
  // override def beforeEach(): Unit = {}
  // override def afterEach(): Unit = {}

  override def afterAll(): Unit = {
    println("afterAll: shutting down db connections")
    // corpusAccessDB.shutdown()
  }

}
