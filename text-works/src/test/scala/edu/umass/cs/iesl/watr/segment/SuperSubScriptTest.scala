package edu.umass.cs.iesl.watr
package segment

// import spindex._
// import geometry._

// import watrmarks.{StandardLabels => LB}

class SuperSubScriptTest extends SegmentationTestUtils {
  behavior of "super/subscript labeling"

  case class Example(
    region: TestRegion,
    expectedTokenization: String, // This is the translation we see now
    skip: Boolean = false
  )
  it should "handle super/subscripts" in {
    // val examples = List(
    //   // Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(172.45, 236.71, 240.05, 8.71)),
    //   //   """{^{a}} Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan"""
    //   // ),
    //   // Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(124.0d, 197.43, 337.38, 16.0)),
    //   //   """Y. Adachi {^{a∗},} H. Morita {^{a},} T. Kanomata {^{b},} A. Sato {^{b},} H. Yoshida {^{c},}"""
    //   // ),
    //   // Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(42.33, 349.94, 502.12, 12.22)),
    //   //   """to be 431 K and 2.6 {10^{−2}} {GPa^{−1}} for {Rh_{2}MnSn,} and 471 K and 1.7 {10^{−2}} for {GPa^{−1}} {Rh_{2}MnGe,} respectively"""
    //   // ),

    //   // Example(TestRegion(papers.paperUrl("bongard2005.pdf"), page(0), LTBounds(145.95, 150.21, 310.0, 8.06)),
    //   //   """by Dirk {Bongard^{a}),} Martin {Möller^{a})^{b}),} S. Nagaraja Rao, David {Corr^{b}),} and Lorenz {Walder*^{a})}"""
    //   // )


    //   // Example(TestRegion(papers.`bongard2005.pdf`, page(1), LTBounds(30.33d, 240.0, 453.2, 15.2)),
    //   //   """{(E_{F(c)=}^{ð}E_{ð}^{#{2}}_{phenylvio}^{Þ}_{logenÞ}).} Electron transfer is slow in situation {c)^{ð}} due to {th^{Þ}e} activation barrier."""
    //   // ),
    //   // Example(TestRegion(papers.`bongard2005.pdf`, page(1), LTBounds(30.33d, 232.0, 453.2, 12.0)),
    //   //   """layer {(E_{F(a)=}E^{#{2}}_{alkylviologen}),} b) flatband situation {(E_{F(b)=}E_{FB(b)=}E^{#{2}}_{benzylviologen}),} and c) depletion layer"""
    //   // ),
    //   // Example(TestRegion(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 540.0, 223.2, 12.0)),
    //   //   """phosphate {(1·PF^{#{1}}_{6}} ; 2 mmol) dissolved in"""
    //   // ),
    //   // Example(TestRegion(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 588.0, 500.0, 15.0)),
    //   //   """{245_{8}.} {^{1}H-NMR} (250 MHz, {CD_{3}CN):} 7.15 (d, {^{3}J_{=}8.9,} arom. H); 7.61 (d, {^{3}J_{=}8.9,} arom. H); 7.89 (d, {^{3}J_{=}6.1,}"""
    //   // )
    // )

  //   import TextReflowConversion.toTextReflow
  //   examples.foreach { example =>
  //     val pdfIns = example.region.pdfUrl
  //     val pageId = example.region.page
  //     val bounds = example.region.bbox

  //     val segmenter = createFilteredMultiPageIndex(pdfIns, pageId, Seq(bounds))
  //     val pageIndex = segmenter.mpageIndex.getPageIndex(pageId)

  //     // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Off
  //     tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Print

  //     segmenter.runLineDetermination()

  //     val lineComponents = pageIndex.getComponentsWithLabel(LB.VisualLine)

  //     val tokenizedLines = lineComponents.map { lineComponent =>
  //       lineComponent.tokenizeLine()
  //       toTextReflow(lineComponent).get.toString()
  //     }

  //     // println(s"""tokenized: ${tokenizedLines.mkString(" \\\\  ")}""")

  //   }
  }

}
