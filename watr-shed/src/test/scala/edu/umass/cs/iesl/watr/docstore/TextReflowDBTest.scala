package edu.umass.cs.iesl.watr
package docstore

import org.scalatest._
import scalaz.concurrent.Task
import doobie.imports._
import TypeTags._
import watrmarks.{StandardLabels => LB}
import corpora._


class TextReflowDBTest extends FlatSpec with Matchers with CorpusTestingUtil {

  val doLogging = false
  val loggingProp = if (doLogging) "?loglevel=2" else ""

  val xa = DriverManagerTransactor[Task](
    s"org.postgresql.Driver",
    s"jdbc:postgresql:watrdev${loggingProp}",
    "watrworker", "watrpasswd"
  )

  def createEmptyDocumentCorpus(): DocumentCorpus = {
    val tables = new TextReflowDBTables(xa)
    tables.dropAndCreate.unsafePerformSync
    val reflowDB = new TextReflowDB(tables)
    reflowDB.docstorage
  }

  behavior of "database-backed corpus"

  it should "add zones" in new FreshDocstore(pageCount=1) {
    try {
      println(
        reportDocument(stableId)
      )
      // val pageNum = PageNum(1)
      // val docId = docStore.getDocument(stableId).get
      // val pageId = docStore.getPage(docId, pageNum).get
      // val targetRegionsPg1 = docStore.getTargetRegions(pageId)
      // val newZoneId = docStore.createZone(docId)
      // docStore.addZoneLabel(newZoneId, LB.PageLines)
      // val newZone = docStore.getZone(newZoneId)
      // println(s"newZone: ${newZone}")
      // targetRegionsPg1.foreach { regionId =>
      //   val zones = docStore.getZonesForTargetRegion(regionId)
      //   println(s"Got zones ${zones}")
      // }
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
    }
  }


}
