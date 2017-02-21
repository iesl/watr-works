package edu.umass.cs.iesl.watr
package docstore

import org.scalatest._
// import watrmarks.{StandardLabels => LB}
import corpora._


class TextReflowDBTest extends FlatSpec with Matchers with CorpusTestingUtil {

  def createEmptyDocumentCorpus(): DocumentCorpus = {
    val tables = new TextReflowDBTables()

    val reflowDB = new TextReflowDB(
      tables,
      dbname="watrdev",
      dbuser="watrworker",
      dbpass="watrpasswd"
    )
    reflowDB.dropAndRecreate
    reflowDB.docstorage
  }

  behavior of "database-backed corpus"

  it should "add zones" in new FreshDocstore(pageCount=2) {
    try {
      println(reportDocument(stableId))
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
