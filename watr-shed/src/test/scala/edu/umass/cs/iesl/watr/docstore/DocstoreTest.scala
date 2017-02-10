package edu.umass.cs.iesl.watr
package docstore


import spindex._
import edu.umass.cs.iesl.watr.{geometry => G}

import TypeTags._

import textreflow._

class DocstoreTest extends ConnectedComponentTestUtil {

  lazy val docStore: ReflowDocstore = new MemDocstore

  behavior of "In-memory Tables"


  it should "insert basic relation types" in {
    val dstore = new MemDocstore

    val stableId = DocumentID("doc-id")
    val pageNum = PageNum(23)

    val docId = dstore.addDocument(stableId)

    val pageId = dstore.addPage(docId, pageNum)

    val geom = G.LTBounds(0, 1, 2, 3)

    dstore.updatePageGeometry(pageId, geom)

  }


}
