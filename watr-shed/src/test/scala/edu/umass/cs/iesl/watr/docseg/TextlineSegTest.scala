package edu.umass.cs.iesl
package watr
package docseg

// import scalaz.@@

import watrmarks._
import ext._




class TextlineSegTest extends DocsegTestUtil  {
  behavior of "text line identification"
  import Component._


  val testExamples = List(
    TextExample(
      """|Page:0 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200401025.pdf.d/101016jactamat200401025.pdf
         |""".stripMargin,
      """|A bs t r a c t               (l:42.52, t:294.50, w:32.20, h:8.97)
         |""".stripMargin,
      """|Abstract               (l:42.52, t:294.50, w:32.20, h:8.97)
         |""".stripMargin
    ),

    TextExample(
      """|Page:0 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200401025.pdf.d/101016jactamat200401025.pdf
         |""".stripMargin,
      """|safet y       (l:519.76, t:452.43, w:24.88, h:9.96)
         |c om b i ne   (l:334.94, t:452.43, w:36.08, h:9.96)
         |and           (l:498.16, t:452.43, w:15.98, h:9.96)
         |at            (l:376.78, t:452.43, w:8.30, h:9.96)
         |To            (l:317.14, t:452.43, w:12.13, h:9.96)
         |b es t        (l:390.73, t:452.43, w:17.01, h:9.96)
         |weld ing      (l:459.55, t:452.43, w:32.96, h:9.96)
         |e co n o mi c (l:413.40, t:452.43, w:40.44, h:9.96)
         |""".stripMargin,
      """|To combine at best economic welding and safety
         |""".stripMargin
    ),

    TextExample(
      """| Page:1 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat201501032.pdf.d/101016jactamat201501032.pdf
         |""".stripMargin,
      """|C.M. Cepeda-Jime´nez et al. / Acta Materialia 88 (2015) 232–244               (l:183.34, t:47.54, w:220.59, h:8.08)
         |""".stripMargin,
      """|C.M. Cepeda-Jime´nez et al. / Acta Materialia 88 (2015) 232–244               (l:183.34, t:47.54, w:220.59, h:8.08)
         |""".stripMargin
    ),

    TextExample(
      """|Page:1 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat201501032.pdf.d/101016jactamat201501032.pdf
         |""".stripMargin,
      """| grain size of 1 lm in               (l:42.52, t:76.04, w:87.32, h:9.84)
         | pure               (l:133.74, t:76.04, w:18.23, h:9.46)
         | Mg samples fabricated               (l:155.91, t:76.04, w:94.32, h:9.46)
         | by               (l:254.15, t:76.04, w:9.96, h:9.46)
         | hot               (l:268.04, t:76.04, w:13.60, h:9.46)
         |""".stripMargin,
      """|grain size of 1 lm in pure Mg samples fabricated by hot
         |""".stripMargin
    )

  )


  it should "identify text lines" in {
    testExamples.map(cutAndPasteToTestExample(_)).foreach{ example =>

      println(s"\n\ntesting ${example.source}")
      val pdfIns = papers.paper(example.source)

      // CermineExtractor.extractChars(pdfIns, Set(375, 376))// add char ids to output pathological debug info
      val zoneIndex = ZoneIndexer.loadSpatialIndices(
        CermineExtractor.extractChars(pdfIns)
      )

      // Assume these example regions are all from one page
      val pageId = example.regions.map(_._2).head

      val allBboxes = example.regions.map(_._3)

      val minX = allBboxes.map(_.left).min
      val minY = allBboxes.map(_.top).min
      val maxX = allBboxes.map(_.right).max
      val maxY = allBboxes.map(_.bottom).max

      val totalBounds = LTBounds(
        minX, minY,
        maxX-minX,
        maxY-minY
      )

      val interestingChars = zoneIndex.queryCharsIntersects(pageId, totalBounds)

      // val cinfos = charInfosBox(approxSortYX(interestingChars))
      println(squishb(interestingChars))

      val docstrum = new DocstrumSegmenter(zoneIndex)

      val lines = docstrum.determineLines_v2(pageId, interestingChars)

      println(s"got ${lines.length} lines")
      zoneIndex.pageGeometry(pageId).bounds.prettyPrint
      println(s"page ${pageId} Bounds: ${zoneIndex.pageGeometry(page(0)).bounds.prettyPrint}")
      println(s"      Borders: ${zoneIndex.pageGeometry(page(0)).borders}")

      // println(zoneIndex.pageGeometry(page(0)).bounds.prettyPrint)

      lines.sortBy(_.findCenterY()).foreach{ l =>
        println(l.tokenizeLine().toText)
      }

      // assertResult(lines.length)(example.identifyingRegex.length)

    }
  }



}






// val examples = List(
//   Example( // Very simple one line example
//     TestRegion(papers.`6376.pdf`, page(0), LTBounds(0.0d, 740.0, 300.0, 15.0)),
//     wsv("8510 8537 8577 2123")
//   )
//     // Example(
//     //   TestRegion(papers.`6376.pdf`, page(0), LTBounds(166.0d, 586.0, 350.0, 48.0)),
//     //   wsv("8510 8537 8577 2123")
//     // )
// )














// ## should be one line
// Page:0 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200401025.pdf.d/101016jactamat200401025.pdf
// ba initic               (l:249.79, t:548.07, w:31.78, h:9.96)
// to               (l:233.74, t:548.07, w:8.82, h:9.96)
// wel de d               (l:125.29, t:548.07, w:29.55, h:9.96)
// aﬀected               (l:42.52, t:548.07, w:32.36, h:9.96)
// joint s,               (l:162.14, t:548.07, w:26.31, h:9.96)
// of               (l:109.19, t:548.07, w:8.82, h:9.96)
// zo ne               (l:82.20, t:548.07, w:19.71, h:9.96)
// leadi ng               (l:195.70, t:548.07, w:30.78, h:9.96)
// ###

// Page:0 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200401025.pdf.d/101016jactamat200401025.pdf
// ## should be one line
// ba initic               (l:249.79, t:548.07, w:31.78, h:9.96)
// to               (l:233.74, t:548.07, w:8.82, h:9.96)
// wel de d               (l:125.29, t:548.07, w:29.55, h:9.96)
// aﬀected               (l:42.52, t:548.07, w:32.36, h:9.96)
// joint s,               (l:162.14, t:548.07, w:26.31, h:9.96)
// of               (l:109.19, t:548.07, w:8.82, h:9.96)
// zo ne               (l:82.20, t:548.07, w:19.71, h:9.96)
// leadi ng               (l:195.70, t:548.07, w:30.78, h:9.96)
// ###

// Page:1 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat201501032.pdf.d/101016jactamat201501032.pdf
// ##Want: extrusion of ball-milled powders, and Chino et al. [21]""
// extru sion               (l:42.52, t:86.64, w:37.54, h:9.46)
// of               (l:85.27, t:86.64, w:8.38, h:9.46)
// ball-mill ed               (l:98.87, t:86.64, w:43.03, h:9.46)
// pow ders,               (l:147.12, t:86.64, w:36.45, h:9.46)
// and               (l:188.84, t:86.64, w:15.18, h:9.46)
// Chi no               (l:209.20, t:86.64, w:25.02, h:9.46)
// et               (l:239.47, t:86.64, w:7.29, h:9.46)
// a l.               (l:251.94, t:86.64, w:9.84, h:9.46)
// [21]               (l:267.02, t:86.64, w:14.58, h:9.46)
