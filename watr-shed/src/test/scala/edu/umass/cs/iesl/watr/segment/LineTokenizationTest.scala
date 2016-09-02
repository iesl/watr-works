package edu.umass.cs.iesl.watr

package segment

import org.scalatest._

import spindex._
import EnrichGeometricFigures._
import ComponentOperations._
import GeometricFigure._

class LineTokenizationTest extends DocsegTestUtil  with DiagrammedAssertions {
  behavior of "text line identification"

  // val testExamplesFromSVG = List(
  //   """| page="0" file="/0575.pdf"
  //      |------------------
  //      |JOURNAL OF SOLID STATE CHEMISTRY {^{78,294–300(1989)}} --> <svg:rect class="linebox" x="53.52" y="52.42" width="206.14"  height="8.21" />
  //      |============
  //      |JOURNAL OF SOLID STATE CHEMISTRY 78, 294-300 (1989)
  //      |""".stripMargin,

  val testExamples = List(
    TextExample(
      """|Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-test/101016japsusc201210126.pdf.d/101016japsusc201210126.pdf
         |""".stripMargin,
      """|be    (l:32.23, t:488.00, w:251.54, h:9.0)
         |""".stripMargin,
      """|Proton exchange membrane fuel cell (PEMFC) is considered to
         |""".stripMargin
    ),
    TextExample(
      """|Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-test/101016japsusc201210126.pdf.d/101016japsusc201210126.pdf
         |""".stripMargin,
      """|be    (l:32.23, t:488.00, w:251.54, h:18.43)
         |""".stripMargin,
      """|Proton exchange membrane fuel cell (PEMFC) is considered to
         |be the most attractive energy technology for the future due to
         |""".stripMargin
    ),
    TextExample(
      """|Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat201112024.pdf.d/101016jactamat201112024.pdf
         |""".stripMargin,
      """|the                                                         (l:520.27, t:465.98, w:13.17, h:9.96)
         |Perhaps                                                     (l:477.92, t:465.98, w:34.10, h:9.96)
         |variables.                                                  (l:429.05, t:465.98, w:40.61, h:9.96)
         |and                                                         (l:374.11, t:465.98, w:15.98, h:9.96)
         |ing),                                                       (l:311.53, t:465.98, w:19.18, h:9.96)
         |other                                                       (l:398.32, t:465.98, w:22.51, h:9.96)
         |strain,                                                     (l:338.97, t:465.98, w:26.85, h:9.96)
         |most                                                        (l:541.64, t:465.98, w:20.84, h:9.96)
         |""".stripMargin,
      """|ing), strain, and other variables. Perhaps the most
         |""".stripMargin
    ),
    // TextExample(
    //   """|Page:0 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200401025.pdf.d/101016jactamat200401025.pdf
    //      |""".stripMargin,
    //   """|A bs t r a c t               (l:42.52, t:294.50, w:32.20, h:8.97)
    //      |""".stripMargin,
    //   """|Abstract
    //      |""".stripMargin
    // ),

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
      """|C.M. Cepeda-Jiménez et al. / Acta Materialia 88 (2015) 232–244               (l:183.34, t:47.54, w:220.59, h:8.08)
         |""".stripMargin,
      """|C.M. Cepeda-Jiménez et al./Acta Materialia 88 (2015) 232–244
         |""".stripMargin
    ),
// é got é
    // decomposition: (101 769) ('e' '́')
    TextExample(
      """|Page:1 /home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat201501032.pdf.d/101016jactamat201501032.pdf
         |""".stripMargin,
      """| grain size of 1 lm in               (l:42.52, t:76.04, w:87.32, h:9.84)
         | pure               (l:133.74, t:76.04, w:18.23, h:9.46)
         | Mg samples fabricated               (l:155.91, t:76.04, w:94.32, h:9.46)
         | by               (l:254.15, t:76.04, w:9.96, h:9.46)
         | hot               (l:268.04, t:76.04, w:13.60, h:9.46)
         |""".stripMargin,
      """|grain size of 1lm in pure Mg samples fabricated by hot
         |""".stripMargin
    )

  )


  it should "identify text lines" in {
    val justRunThisOne:Option[Int] = None
    // val justRunThisOne:Option[Int] = Some(3)

    val examples = testExamples.map(cutAndPasteToTestExample(_))

    justRunThisOne
      .map(i => examples.drop(i).take(1))
      .getOrElse(examples)
      .foreach({example =>
        testExample(example)
      })

  }


  def testExample(example: ParsedExample): Unit = {
    println(s"\ntesting ${example.source}")

    val pdfIns = papers.paper(example.source)

    val segmenter =  DocumentSegmenter.createSegmenter(pdfIns, Seq())

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
    segmenter.zoneIndexer.dbgFilterPages(pageId)
    segmenter.zoneIndexer.dbgFilterComponents(pageId, totalBounds)

    val interestingChars = segmenter.zoneIndexer
      .getPageInfo(pageId)
      .componentIndex
      .queryForIntersects(totalBounds)

    println("tokenizing line w/chars: ["+squishb(interestingChars)+"]")

    utils.VisualTracer.visualTraceLevel = utils.VisualTraceLevel.Off
    // utils.VisualTracer.visualTraceLevel = utils.VisualTraceLevel.Print

    segmenter.runLineDetermination()

    // find visual lines in bounds:
    val lineComponents = segmenter.zoneIndexer
      .getPageInfo(pageId)
      .componentIndex
      .queryForIntersects(totalBounds)
      .sortBy(_.bounds.top)
      .filter(_.roleLabel == LB.VisualLine)


    val tokenizedLines = lineComponents.map { lineComponent =>
      lineComponent.tokenizeLine()
      ComponentRendering.VisualLine.render(lineComponent).toString()
    }


    // println(s"""component= ${lineComponent.chars}, ${lineComponent.id} (${lineComponent.getLabels.mkString(", ")})""")
    // println(s"""expecting: ${example.expectedOutput.mkString(" \\\\  ")}""")
    // println(s"""tokenized: ${tokenizedLines.mkString(" \\\\  ")}""")

    example.expectedOutput.zip(tokenizedLines)
      .foreach({case (expect, actual) =>
        if (expect != actual) {
          println("expect: " + expect.toList.map(_.toInt))
          println("got   : " + actual.toList.map(_.toInt))
          println(s"want> $expect")
          println(s"got > $actual")
        } else {
          println(s"ok> $expect")
        }
        assertResult(expect){ actual }
      })

    // assertResult(example.expectedOutput.length)(tokenized.length)
  }


}









