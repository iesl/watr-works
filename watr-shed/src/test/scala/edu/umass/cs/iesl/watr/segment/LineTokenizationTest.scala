package edu.umass.cs.iesl.watr

package segment

import org.scalatest._

import spindex._
import EnrichGeometricFigures._
import ComponentOperations._
import GeometricFigure._

class LineTokenizationTest extends DocsegTestUtil  with DiagrammedAssertions {
  behavior of "text line identification"


  val testExamples = List(
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """TaS2.["]Only a s s u m p t i o n s c a n b e m a d e a b o u t t h e arrange- (l:42.70, t:36.67, w:230.84, h:6.14)""",
      """TaS_{2}^{[6]}. Only assumptions can be made about the arrange-"""
    ),
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """sponding to the selected points of the curves, the oxidation was interrupted  (l:43.70, t:313.95, w:230.99, h:4.79)""",
      """sponding to the selected points of the curves, the oxidation was interrupted"""
    ),
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """[Fe3(CO)9(p3-O)]2Q l I 3 O with one equivalent of       (l:301.00, t:292.27, w:230.74, h:6.16)""",
      ""
    ),
    // The system fails to  find the entire line for this example
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """[Mn(CO)3(C H3CN) 3][PF, l' 61                                                (l:300.70, t:302.50, w:236.51, h:6.35)""",
      ""
    ),
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """0 x 0 cluster PPN[Fe3Mn(CO),,(p4-0)] 2 in 75% ~ i e 1 d . I 'T~h e           (l:301.00, t:323.67, w:230.79, h:6.31)""",
      ""
    ),
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """structureof 2 is shown in Figure 1(see also Table The                        (l:301.00, t:334.09, w:230.79, h:6.51)""",
      ""
    ),
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """TaS2.["]Only assumptions can be m a d e a b o u t the arrange-               (l:42.70, t:36.67, w:230.84, h:6.14)""",
      ""
    ),
    TextExample(
      """Page:0 file:///Schauer-1987.pdf""",
      """t u r e of t h e C 4 F 9 c h a i n s is unlikely, because of t h e size of   (l:43.00, t:68.30, w:230.97, h:6.41)""",
      ""
    ),
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
    // val justRunThisOne:Option[Int] = Some(0)

    val examples = testExamples.map(cutAndPasteToTestExample(_))

    justRunThisOne
      .map(i => examples.drop(i).take(1))
      .getOrElse(examples)
      .foreach({example =>
        testExample(example)
      })

  }

  // import TypeTags._
  import scalaz.@@
  import java.io.InputStream



  def testExample(example: ParsedExample): Unit = {
    println(s"\ntesting ${example.source}")

    val pdfIns = papers.paper(example.source)
    // Assume these example regions are all from one page
    val pageId = example.regions.map(_._2).head
    val segmenter = createFilteredZoneIndexer(pdfIns, pageId, example.regions.map(_._3))


    utils.VisualTracer.visualTraceLevel = utils.VisualTraceLevel.Off
    // utils.VisualTracer.visualTraceLevel = utils.VisualTraceLevel.Print

    segmenter.runLineDetermination()

    val pageInfo = segmenter.zoneIndexer.getPageInfo(pageId)
    val lineComponents = pageInfo.getComponentsWithLabel(LB.VisualLine)

    val tokenizedLines = lineComponents.map { lineComponent =>
      lineComponent.tokenizeLine()
      ComponentRendering.VisualLine.render(lineComponent).get.toString()
    }


    // println(s"""component= ${lineComponent.chars}, ${lineComponent.id} (${lineComponent.getLabels.mkString(", ")})""")
    // println(s"""expecting: ${example.expectedOutput.mkString(" \\\\  ")}""")
    // println(s"""tokenized: ${tokenizedLines.mkString(" \\\\  ")}""")

    example.expectedOutput.zip(tokenizedLines)
      .foreach({case (expect, actual) =>
        if (!expect.isEmpty && expect != actual) {
          println(s"want> $expect")
        } else  if (expect.isEmpty) {
          println(s"want? (not specified)")
        }
        println(s"got> ${actual}")
        // if (!expect.isEmpty()) {
        //   assertResult(expect){ actual }
        // }
      })

    // assertResult(example.expectedOutput.length)(tokenized.length)
  }


}

