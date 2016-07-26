package edu.umass.cs.iesl.watr
package segment

import spindex._
import ComponentOperations._
// import IndexShapeOperations._
import GeometricFigure._

class SuperSubScriptTest extends DocsegTestUtil {
  behavior of "docstrum segmenter"

  // super/subscript
  //Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat201111030.pdf.d/101016jactamat201111030.pdf
  // respect to Pb(Zr_x_Ti_1_ _x_)O_3_ (PZT) solid solution thin ﬁlms                                                                                 (l:32.71, t:634.53, w:250.95, h:10.93)
  // Pb(Zr_x_Ti_1_ _x_)O_3_ is one of the most studied ferroelectric                                                                                  (l:313.63, t:658.46, w:239.03, h:10.93)

  // Page:1 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat201111030.pdf.d/101016jactamat201111030.pdf
  // Pb_1__.__2_Zr_0__.__5__3_Ti_0__.__4__7_O_3_ (PZT53) precursor solution. Reference    (l:311.53, t:117.10, w:250.97, h:10.83)

  //Page:1 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200401009.pdf.d/101016jactamat200401009.pdf
  // Ti (under 38 lm) and ethylene glycol (5.0 10^3^ mm^3^ for                 (l:50.63, t:187.88, w:239.08, h:11.58)

  // Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200401025.pdf.d/101016jactamat200401025.pdf
// ^a^, A.F. Gourgues ^b^^,^^*^, A. Pineau ^b^                                                                             (l:142.19, t:202.91, w:302.77, h:15.44)

  case class Example(
    region: TestRegion,
    expectedTokenization: String, // This is the translation we see now
    skip: Boolean = false
  )

  it should "handle super/subscripts" in {
    val examples = List(
      Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(166.0d, 586.0, 350.0, 12.0)),
        """{^{a}} Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan"""
      ),

      Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(166.0d, 549.0, 350.0, 15.0)),
        """Y. Adachi {^{a∗},} H. Morita {^{a},} T. Kanomata {^{b},} A. Sato {^{b},} H. Yoshida {^{c},}"""
      ),

      Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(84.33d, 700.0, 403.2, 12.2)),
        """to be 431 K and 2.6 {10^{−2}} {GPa^{−1}} for {Rh_{2}MnSn,} and 471 K and 1.7 {10^{−2}} for {GPa^{−1}} {Rh_{2}MnGe,} respectively"""
      ),

      Example(TestRegion(papers.`bongard2005.pdf`, page(0), LTBounds(30.33d, 145.0, 453.2, 15.2)),
        """by Dirk {Bongard^{a}),} Martin {Möller^{a})^{b}),} S. Nagaraja Rao, David {Corr^{b}),} and Lorenz {Walder*^{a})}"""
      ),


      Example(TestRegion(papers.`bongard2005.pdf`, page(1), LTBounds(30.33d, 240.0, 453.2, 15.2)),
        """{(E_{F(c)=}^{ð}E_{ð}^{#{2}}_{phenylvio}^{Þ}_{logenÞ}).} Electron transfer is slow in situation {c)^{ð}} due to {th^{Þ}e} activation barrier."""
      ),

      Example(TestRegion(papers.`bongard2005.pdf`, page(1), LTBounds(30.33d, 232.0, 453.2, 12.0)),
        """layer {(E_{F(a)=}E^{#{2}}_{alkylviologen}),} b) flatband situation {(E_{F(b)=}E_{FB(b)=}E^{#{2}}_{benzylviologen}),} and c) depletion layer"""
      ),

      Example(TestRegion(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 540.0, 223.2, 12.0)),
        """phosphate {(1·PF^{#{1}}_{6}} ; 2 mmol) dissolved in"""

      ),

      Example(TestRegion(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 588.0, 500.0, 15.0)),
        """{245_{8}.} {^{1}H-NMR} (250 MHz, {CD_{3}CN):} 7.15 (d, {^{3}J_{=}8.9,} arom. H); 7.61 (d, {^{3}J_{=}8.9,} arom. H); 7.89 (d, {^{3}J_{=}6.1,}"""
      )
    )

    examples.foreach { example =>
      if (! example.skip) {
        // DocumentExtractor.extractChars(pdfIns, Set(375, 376))// add char ids to output pathological debug info

        val zoneIndex = ZoneIndexer.loadSpatialIndices(
          format.DocumentIO.extractChars(example.region.pdf)
        )

        val chars = zoneIndex.getPageInfo(example.region.page).charAtomIndex.queryForIntersects(example.region.bbox)

        val found = chars.sortBy(_.region.bbox.left).map({ cbox => cbox.char }).toList.mkString

        val lineChars = chars.sortBy(_.region.bbox.left)
        val ccs = zoneIndex.concatRegions(lineChars).addLabel(LB.VisualLine)

        val tokenized = ccs.tokenizeLine().toText

        assertResult(example.expectedTokenization)(tokenized)
      }

    }
  }

}
