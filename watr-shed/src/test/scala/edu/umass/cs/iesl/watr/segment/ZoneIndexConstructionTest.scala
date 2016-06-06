package edu.umass.cs.iesl
package watr
package segment

import watrmarks._
import extract._

class ZoneIndexConstructionTest extends DocsegTestUtil  {

  behavior of "zone index for segmentation"

  case class Example(
    region: TestRegion,
    skip: Boolean = false
  )

  val examples = List(
    Example(
      TestRegion(papers.`6376.pdf`, page(0), LTBounds(0.0d, 740.0, 300.0, 15.0))
    )
      // Example(
      //   TestRegion(papers.`6376.pdf`, page(0), LTBounds(166.0d, 586.0, 350.0, 48.0)),
      //   wsv("8510 8537 8577 2123")
      // )
  )

  it should "identify text lines" in {
    examples.foreach { example =>
      if (!example.skip) {
        val pages = DocumentExtractor.extractChars(example.region.pdf)
        pages.foreach { case(pageChars, geom) =>
          println(s"page bounds = ${geom.bounds.prettyPrint}")
          pageChars.chars.take(20).foreach { charbox =>
            println(s"${charbox.char} = ${charbox.bbox.prettyPrint}")
          }
        }
        val zoneIndex = ZoneIndexer.loadSpatialIndices(pages)
      }

    }
  }
}
// J:  '74'
//     Rise              0.0                                => Float
//     AscentLine         [236.5003 738.5823, 1.0}}] -> [239.60068 738.5823, 1.0}}] 3.1003723}       => LineSegment
//     Baseline           [236.5003 733.1387, 1.0}}] -> [239.60068 733.1387, 1.0}}] 3.1003723}         => LineSegment
//     Baseline (uns)     [0.0 0.0, 1.0}}] -> [0.389 0.0, 1.0}}] 0.389} => LineSegment
//     DescentLine        [236.5003 731.4092, 1.0}}] -> [239.60068 731.4092, 1.0}}] 3.1003723}      => LineSegment
//     Mcid              null              => Integer
//     PdfString         J              => PdfString

// o:  '111'
//     Rise              0.0                                => Float
//     AscentLine         [239.60068 738.5823, 1.0}}] -> [243.58572 738.5823, 1.0}}] 3.9850464}       => LineSegment
//     Baseline           [239.60068 733.1387, 1.0}}] -> [243.58572 733.1387, 1.0}}] 3.9850464}         => LineSegment
//     Baseline (uns)     [0.0 0.0, 1.0}}] -> [0.5 0.0, 1.0}}] 0.5} => LineSegment
//     DescentLine        [239.60068 731.4092, 1.0}}] -> [243.58572 731.4092, 1.0}}] 3.9850464}      => LineSegment
//     Mcid              null              => Integer
//     PdfString         o              => PdfString

// u:  '117'
//     Rise              0.0                                => Float
//     AscentLine         [243.58572 738.5823, 1.0}}] -> [247.57077 738.5823, 1.0}}] 3.9850464}       => LineSegment
//     Baseline           [243.58572 733.1387, 1.0}}] -> [247.57077 733.1387, 1.0}}] 3.9850464}         => LineSegment
//     Baseline (uns)     [0.0 0.0, 1.0}}] -> [0.5 0.0, 1.0}}] 0.5} => LineSegment
//     DescentLine        [243.58572 731.4092, 1.0}}] -> [247.57077 731.4092, 1.0}}] 3.9850464}      => LineSegment
//     Mcid              null              => Integer
//     PdfString         u              => PdfString

