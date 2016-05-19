package edu.umass.cs.iesl
package watr
package docseg

import org.scalatest._

import watrmarks._
// import iesl.watr.LB
import ext._


class DocstrumSegmenterTest extends FlatSpec with Matchers {
  behavior of "docstrum segmenter"

  val LB = watrmarks.StandardLabels


  // it should "compute angles correctly" in {
  //   import Bounds._


  //   val points = List(
  //     Point(1, 0) -> Point(1, 0),
  //     Point(1, 0) -> Point(1, 1),
  //     Point(1, 0) -> Point(0, 1),
  //     Point(1, 0) -> Point(-1, -1)
  //   )

  //   points.foreach { case(p1, p2) =>
  //     println(s"${p1.prettyPrint} -> ${p2.prettyPrint}: angle = ${p1.angleTo(p2)}")
  //   }
  // }


  // alloys a: (l:234.43, t:519.98, w:7.52, h:15.24) -> (l:241.95, t:519.98, w:4.71, h:15.24)


  case class Example(
    bbox: LTBounds,
    expectedChars: String,
    expectedTranslation: String
  )

  val page0 = PageID(0)

  it should "handle super/subscripts" in {
    val examples = List(
      Example(
        LTBounds(166.0d, 586.0, 350.0, 12.0),
        """a Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan""".replaceAll(" ", ""),
        """^a^ Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan"""
      )
      ,Example(
        LTBounds(166.0d, 549.0, 350.0, 15.0),
        """Y. Adachi a,∗, H. Morita a, T. Kanomata b, A. Sato b, H. Yoshida c,""".replaceAll(" ", ""),
        """Y. Adachi ^a,∗^, H. Morita ^a^, T. Kanomata ^b^, A. Sato ^b^, H. Yoshida ^c^,"""
      )
      ,Example(
        LTBounds(84.33d, 700.0, 403.2, 12.2),
        """to be 431 K and +2.6 × 10−2 GPa−1 for Rh2MnSn, and 471 K and +1.7 × 10−2 for GPa−1 Rh2MnGe, respectively.""".replaceAll(" ", ""),
        """to be 431 K and +2.6 × 10^−2^ GPa^−1^ for Rh_2_MnSn, and 471 K and +1.7 × 10^−2^ for GPa^−1^ Rh_2_MnGe, respectively."""
      )
    )
    val charsAndGeometry = CermineExtractor.extractChars(papers.`6376.pdf`)

    val zoneIndex = ZoneIndexer.loadSpatialIndices(charsAndGeometry)
    examples.foreach{ example =>
      println()
      println()

      val chars = zoneIndex.queryChars(PageID(0), example.bbox)
      val lineChars = chars.sortBy(_.bbox.left)
      val ccs = Component(lineChars.map(Component(_)), 0d, LB.Line)

      // val found = chars.sortBy(_.bbox.left).map({ cbox => cbox.char }).toList.mkString
      // println(s"trying: $found")
      // println()

      // val docstrum = new DocstrumSegmenter(zoneIndex)
      // val orientation = docstrum.computeOrientation(Seq(ccs))
      val tokenized = ccs.tokenizeLine()
      // println(s"   = ${tokenized.toText}")


      assertResult(example.expectedTranslation)(tokenized.toText)

    }

  }




  // it should "segment page 1" in {
  //   // extract chars (via cermine)
  //   val charsAndGeometry = CermineExtractor.extractChars(papers.`6376.pdf`)
  //   println(s"extracted ${charsAndGeometry.length} pages")

  //   val zoneIndex = ZoneIndexer.loadSpatialIndices(charsAndGeometry)


  //   // zoneIndex.getPages.foreach { pageId =>
  //   //   docstrum.segmentPage(pageId)
  //   // }
  // }
}
