package edu.umass.cs.iesl.watr
package docstore

import corpora._

class DBDocumentCorpusTest extends DatabaseTest {

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
