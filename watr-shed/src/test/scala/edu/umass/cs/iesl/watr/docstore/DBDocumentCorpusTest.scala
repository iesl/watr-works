package edu.umass.cs.iesl.watr
package docstore

import org.scalatest._
// import watrmarks.{StandardLabels => LB}
import corpora._
import geometry._
import TypeTags._

class DBDocumentCorpusTest extends FlatSpec with Matchers with CorpusTestingUtil {

  def createEmptyDocumentCorpus(): DocumentCorpus = {
    val tables = new TextReflowDBTables()

    val reflowDB = new TextReflowDB(
      tables,
      dbname="watrdev",
      dbuser="watrworker",
      dbpass="watrpasswd"
    )


    reflowDB.runq {
      reflowDB.veryUnsafeDropDatabase().run
    }

    reflowDB.dropAndRecreate
    reflowDB.docStore
  }

  behavior of "database-backed corpus"


  it should "add zones" in new CleanDocstore {
    // val stableId = DocumentID("doc0")
    // val docId = docStore.addDocument(stableId)
    // val pageId = docStore.addPage(docId, PageNum(0))
    // val regionId = docStore.addTargetRegion(pageId, LTBounds(5d, 4d, 3d, 2d))

    // val zoneId = docStore.createZone(regionId)
    // val zone = docStore.getZone(zoneId)

    // println(zone)
  }


}
