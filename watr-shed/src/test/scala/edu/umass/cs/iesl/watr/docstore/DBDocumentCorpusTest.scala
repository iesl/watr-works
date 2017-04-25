package edu.umass.cs.iesl.watr
package docstore

import org.scalatest._
// import watrmarks.{StandardLabels => LB}
import corpora._
// import geometry._
// import TypeTags._

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


  import TypeTags._


  it should "run default tests" in new CleanDocstore {
    test1()
    val stableId = DocumentID("doc#0")
    val docId = docStore.getDocument(stableId).get
    for {
      pageId <- docStore.getPages(docId)
      pageDef <- docStore.getPageDef(pageId)
      labelId <- docStore.getZoneLabelsForDocument(docId)
      zoneId <- docStore.getZonesForDocument(docId, labelId)
    } {
      println(visualizeDocuments())
      println(s"deleting zone ${zoneId}")
      docStore.deleteZone(zoneId)
    }

  }



}
