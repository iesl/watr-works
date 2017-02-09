package edu.umass.cs.iesl.watr
package docstore


import spindex._
import edu.umass.cs.iesl.watr.{geometry => G}

import TypeTags._

import textreflow._

class DocstoreTest extends ConnectedComponentTestUtil {

  lazy val docStore: ReflowDocstore = MemDocstore

  behavior of "In-memory Tables"


  it should "insert basic relation types" in {
    val dstore = MemDocstore

    val docId = DocumentID("doc-id")
    val doc = dstore.addDocument(docId)

    val geom = G.PageGeometry(PageNum(23), G.LTBounds(0, 1, 2, 3))
    val mGeom = dstore.addPage(docId, geom)

    println(mGeom)


  }


}
