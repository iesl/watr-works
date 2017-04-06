package edu.umass.cs.iesl.watr
package corpora

import org.scalatest._

// import geometry._
import TypeTags._
// import watrmarks.{StandardLabels => LB}

class DocstoreTest extends FlatSpec with Matchers with CorpusTestingUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "In-memory Tables"

  def add4pg_3x3SampleDoc(): Unit = {
    val docs = List(
      List( 
        "abc\ndef\nghi"
      )
    )

    for { (doc, i) <- docs.zipWithIndex } {
      addDocument(DocumentID(s"doc#${i}"), doc)
    }
  }

  it should "handle zones" in new CleanDocstore {
    add4pg_3x3SampleDoc()
    println(visualizeDocuments())

    // val stableId = DocumentID("doc0")
    // val docId = docStore.addDocument(stableId)
    // val pageId = docStore.addPage(docId, PageNum(0))
    // val regionId = docStore.addTargetRegion(pageId, LTBounds(5d, 4d, 3d, 2d))

    // val zoneId = docStore.createZone(regionId, LB.VisualLine)
    // val zone = docStore.getZone(zoneId)

    // println(zone)

  }


}
