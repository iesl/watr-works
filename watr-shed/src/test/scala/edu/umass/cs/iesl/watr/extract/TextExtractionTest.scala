package edu.umass.cs.iesl.watr
package extract

import segment._
import spindex._
import IndexShapeOperations._
// import ComponentOperations._
import ComponentTypeEnrichments._

class TextExtractionTest extends DocsegTestUtil {
  // import watrmarks._
  // import StandardLabels._

  behavior of "itextpdf - generated text"


  val examples = List(
    TextExample(
      """|Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-test/0575.pdf.d/0575.pdf
         |""".stripMargin,
      """|1.13760(3I)nm,c/a=1.704,                                                                      (l:71.04, t:332.26, w:103.44, h:8.21)
         |=0.4391nm3,Z=9.X-rayintensitydatawereobtainedfromafour-                                       (l:184.80, t:332.26, w:244.49, h:8.21)
         |V                                                                                             (l:176.40, t:332.95, w:4.58, h:7.33)
         |circlediffractometer;thestructurewassolvedbyPattersonmethodsandrefinedbyfull-matrixleast-     (l:70.08, t:342.10, w:359.84, h:8.21)
         |""".stripMargin,
      """|1.13760(3 I) nm, c/a = 1.704, V = 0.4391 nm3, Z = 9. X-ray intensity data were obtained from a fourcircle
         |diffractometer; the structure was solved by Patterson methods and refined by full-matrix least
         |""".stripMargin
    )
  )

  it should "compute text bounds properly" in   {
    examples.map(cutAndPasteToTestExample(_)).foreach{ example =>



      println(s"\n\ntesting ${example.source}")

      val pdfIns = papers.paper(example.source)

      val segmenter = DocumentSegmenter.createSegmenter(pdfIns)

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

      segmenter.runLineDetermination()
      val interestingChars = segmenter.zoneIndexer.pageInfos(pageId).rCharIndex.queryForIntersects(totalBounds)

      println("["+squishb(interestingChars)+"]")

      for {
        p <- segmenter.visualLineOnPageComponents
        line <- p
      } {
        if (line.toText.startsWith("1.1") ||
          line.toText.startsWith("circle")) {

          println(s"examining line: ${line.toText}")

          line.descendants().foreach { desc =>
            // println(s"c> ${desc.bounds}, ${desc}")
            desc match {
              case PageComponent(id, comp, zoneIndex) =>
                comp match {
                  case cr:CharAtom =>
                    println("   " + cr.debugPrint)
                  case _ =>
                }
              case _ =>
            }

          }

        }
      }



    }
  }

  it should "properly extract the following characters" in {
    // Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(166.0d, 549.0, 350.0, 15.0)),
    //   expectedChars = """Y. Adachi a∗, H. Morita a, T. Kanomata b, A. Sato b, H. Yoshida c,""".replaceAll(" ", ""),
    //   desiredChars  = """Y. Adachi a,∗, H. Morita a, T. Kanomata b, A. Sato b, H. Yoshida c,""".replaceAll(" ", ""),
    // Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(84.33d, 700.0, 403.2, 12.2)),
    //   expectedChars = """to be 431K and  2.6 10−2 GPa−1 for Rh2MnSn, and 471 K and  1.7 10−2 for GPa−1 Rh2MnGe, respectively""",
    //   desiredChars  = """to be 431K and +2.6×10−2 GPa−1 for Rh2MnSn, and 471 K and +1.7×10−2 for GPa−1 Rh2MnGe, respectively.""",
    // Example(TestRegion(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 540.0, 223.2, 12.0)),
    //   expectedChars = """phosphate(1·PF6;2mmol)dissolvedin""",
    //   desiredChars = """phosphate (1·PF%-%6%; 2 mmol) dissolved in""",

    // Problems here: bad/missing char, math sym extraction, incorrect line joining
    //   want output like this:  bohedral) cell (a_{hex} = a_{0}{\sqrt}3, c_{hex} = 3c_{0}
    //    <!--
    //     however, disclosed an even larger (rhom– -->
    //     <svg:rect class="linebox" x="57.60" y="116.25" width="189.97"  height="9.99" />
    //     <!--
    //     bohedral) cell (ahex= aoti, chex= -->
    //     <svg:rect class="linebox" x="57.60" y="128.49" width="167.92"  height="9.99" />
    //     <!--
    //     34 -->
    //     <svg:rect class="linebox" x="233.04" y="130.55" width="7.66"  height="7.33" />
    //     <!--
    //     which also applied for the ThB*C phase, -->
    //     <svg:rect class="linebox" x="57.84" y="140.01" width="191.18"  height="9.99" />

    val _ = DocumentExtractor.extractChars(papers.`6376.pdf`)

    // assertResult(example.expectedChars.replaceAll(" ",""))(found)
    // val exc = example.expectedChars.replaceAll(" ", "")
    // val des = example.desiredChars.replaceAll(" ", "")
    // if (!des.isEmpty && exc != des) {
    //   println(s"""|warning: extracted characters need improvement... \n    ${found}
    //               | want: ${des}
    //               | have: ${exc}
    //               |""")
    // }


  }

}
