package edu.umass.cs.iesl.watr
package geometry

import TypeTags._
import watrmarks.{StandardLabels => LB}

import corpora._
import labeling._

class ZoningSpec extends LabelWidgetTestUtil {

  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  import ZoneTrees._

  behavior of "zone trees"

  it should "construct a zone" in new CleanDocstore  {
    val stableId = DocumentID("doc0")
    val docId = docStore.addDocument(stableId)
    val pageId = docStore.addPage(docId, PageNum(0))

    val pageRegion = mkTargetRegionDbl(pageId, 5d, 4d, 3d, 2d)

    val l0 = leaf(pageRegion.id)
    val l01 = role(LB.VisualLine, l0)
    val l02 = ref(ZoneID(32), l0)
    val l03 = role(LB.Authors, l02)
    val n01 = node(List(
      l0, l01, l02
    ))

    val n011 = ref(ZoneID(22), role(LB.Title, n01))

    println(ZoneTrees.prettyPrintTree(n011))

  }

}
