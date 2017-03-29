package edu.umass.cs.iesl.watr
package corpora

import org.scalatest._

import geometry._
import TypeTags._

class DocstoreTest extends FlatSpec with Matchers with CorpusTestingUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "In-memory Tables"

  val ZT = ZoneTrees

  it should "handle zones" in new CleanDocstore {

    val stableId = DocumentID("doc0")
    val docId = docStore.addDocument(stableId)
    val pageId = docStore.addPage(docId, PageNum(0))
    val regionId = docStore.addTargetRegion(pageId, LTBounds(5d, 4d, 3d, 2d))

    val zoneId = docStore.createZone(regionId)
    val ztree = docStore.getZone(zoneId)

    println(ZT.prettyPrintTree(ztree))

  }


}
