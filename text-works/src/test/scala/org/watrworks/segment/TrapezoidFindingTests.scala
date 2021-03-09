package org.watrworks
package segment

import TypeTags._

class TrapezoidFindingTests extends SegmentationTest {
  behavior of "Trapezoid Clustering"

  import ammonite.{ops => fs}

  it should "extract" in {
    val path = fs.pwd / "corpus.d" / "cmp-lg9503025.pdf.d" / "cmp-lg9503025.pdf"

    val docId = DocumentID("todo")
    val segmenter = DocumentSegmenter.createSegmenter(docId, path)

    segmenter.runDocumentSegmentation()
  }
}
