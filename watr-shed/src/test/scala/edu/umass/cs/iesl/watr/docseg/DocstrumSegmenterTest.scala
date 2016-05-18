package edu.umass.cs.iesl.watr
package docseg

import org.scalatest._

import watrmarks._
import ext._

class DocstrumSegmenterTest extends FlatSpec {
  behavior of "docstrum segmenter"


  it should "compute angles correctly" in {
    import Bounds._

     // c.bbox.toCenterPoint.angleTo(component.bbox.toCenterPoint)

    val points = List(
      Point(1, 0) -> Point(1, 0),
      Point(1, 0) -> Point(1, 1),
      Point(1, 0) -> Point(0, 1),
      Point(1, 0) -> Point(-1, -1)
    )

    points.foreach { case(p1, p2) =>
      println(s"${p1.prettyPrint} -> ${p2.prettyPrint}: angle = ${p1.angleTo(p2)}")
    }

  }



  it should "segment page 1" in {
    // extract chars (via cermine)
    val charsAndGeometry = CermineExtractor.extractChars(papers.`6376.pdf`)
    println(s"extracted ${charsAndGeometry.length} pages")

    val zoneIndex = ZoneIndexer.loadSpatialIndices(charsAndGeometry)

    val docstrum = new DocstrumSegmenter(zoneIndex)
    zoneIndex.getPages.foreach { pageId =>
      docstrum.segmentPage(pageId)
    }

  }
}
