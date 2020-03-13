package org.watrworks
package corpora
package database

import org.scalatest._
import TypeTags._

class DatabaseBulkImportTest extends AnyFlatSpec with Matchers with CorpusTestingUtil with BeforeAndAfterEach {
  behavior of "bulk document import"

  var memZoneApi = new MemDocZoningApi
  freshDocstore = Some(memZoneApi)

  def createEmptyDocumentZoningApi(): DocumentZoningApi = memZoneApi

  import cats.effect._
  import scala.concurrent._
  val MainCPUBound = ExecutionContext.global
  implicit val cs: ContextShift[IO] = IO.contextShift(MainCPUBound)

  def initDB(): CorpusAccessDB = {
    new CorpusAccessDB(
      dbname="watrdev",
      dbuser="watrworker",
      dbpass="watrpasswd"
    )
  }

  lazy val corpusAccessDB = initDB()
  def annotApi: DocumentAnnotationApi = corpusAccessDB.annotApi

  override def beforeEach(): Unit = {
    println("re-initing db connections")
    corpusAccessDB.runqOnce {
      corpusAccessDB.veryUnsafeDropDatabase().run
    }

    corpusAccessDB.dropAndRecreate
    // corpusAccessDB.reinit()
  }

  override def afterEach(): Unit = {}

  def add4pg_3x3SampleDoc(stableId: String@@DocumentID): Unit = {
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr",
      "stu\nvwx\nyzz"
    )

    addDocument(stableId, doc)
  }

  it should "import a mem-based document db" in new EmptyDatabase {
    memZoneApi = new MemDocZoningApi
    freshDocstore = Some(memZoneApi)
    val id0 = DocumentID("doc#0")
    val id1 = DocumentID("doc#1")
    add4pg_3x3SampleDoc(id0)
    add4pg_3x3SampleDoc(id1)
    corpusAccessDB.docStore.batchImport(memZoneApi)
  }


  it should "properly translate mem-based db key into postgres keys" in new EmptyDatabase {
    memZoneApi = new MemDocZoningApi
    freshDocstore = Some(memZoneApi)
    // Extract into memory-db, add to postgres
    val id0 = DocumentID("doc#0")
    add4pg_3x3SampleDoc(id0)
    // docStore.batchImport
    corpusAccessDB.docStore.batchImport(memZoneApi)

    memZoneApi = new MemDocZoningApi
    freshDocstore = Some(memZoneApi)
    val id1 = DocumentID("doc#1")
    add4pg_3x3SampleDoc(id1)
    corpusAccessDB.docStore.batchImport(memZoneApi)
  }

}
