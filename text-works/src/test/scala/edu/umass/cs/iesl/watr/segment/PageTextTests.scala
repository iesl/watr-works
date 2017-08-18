package edu.umass.cs.iesl.watr
package segment

import corpora._
import spindex._

class PageTextTest extends SegmentationTestUtils  {
  /**

    These are meant to be minimal smokescreen tests to insure that basic text
    extraction is working reasonable well with respect to spacing,
    char/line ordering, super/subscripts, etc.

   */

  it should "extract text reasonably well" in {

    val expectedText = {
      """|
         |  bioRxiv preprint first posted online Nov. 30, 2016; doi: http://dx.doi.org/10.1101/090498. The copyright holder for this preprint (which was not
         |        peer-reviewed) is the author/funder. It is made available under a CC-BY-NC-ND 4.0 International license.
         |
         |         The effect of known mutator alleles in a cluster of clinical
         |                   Saccharomyces cerevisiae strains
         |
         |  Daniel A. Skelly⁽1,3⁾, Paul M. Magwene⁽1⁾, Brianna Meeks⁽2⁾, Helen A. Murphy2
         |
         |  ⁽1⁾ Department of Biology, Duke University, Durham, North Carolina
         |  ⁽2⁾ Department of Biology, The College of William and Mary, Williamsburg, Virginia
         |  ⁽3⁾ Current address: The Jackson Laboratory, Bar Harbor, Maine
         |""".stripMargin
    }
    expectedText.split("\n").map(_.trim())

    // 6376.pdf  => page 2 column seg is wrong, many paths making up graphs kill performance
    // 101016jcarbon201301056.pdf => reference section columns are wrong, page 1 has repeated lines
    // bongard2005.pdf => reference section is dividing into 2 cols, one for markers one for ref

    val allTestPdfs = parsePdfs {
      """|
         |101016jcarbon201301056.pdf.pages/pg_0001.pdf
         |101016jcarbon201301056.pdf.pages/pg_0002.pdf
         |101016jactamat201112024.pdf
         |101016jcarbon201301056.pdf
         |101016jactamat201501032.pdf
         |saccharomyces-1-page.pdf
         |gr-qc9903064.pdf.pages/pg_0001.pdf
         |bongard2005.pdf
         |austenite.pdf
         |6376.pdf
         |2839.pdf
         |hep-ph9503349.pdf
         |astro-ph9903259.pdf
         |cond-mat9803032.pdf
         |101016japsusc201210126.pdf
         |acsnano.5b00028.pdf
         |font-type-0-1-t.pdf
         |font-type-1-3.pdf
         |quantification-Smith-2013.pdf
         |Schauer-1987.pdf
         |""".stripMargin
    }

    val (docId, path) = allTestPdfs(0)


    def tracecb(pageIndex: PageIndex): Unit = {
      println("tracecb!")
    }

    tracing.VisualTracer.visualTraceLevel = tracemacros.VisualTraceLevel.Visualize

    PageIndex.activeTracingCallback = tracecb(_)

    val segmenter = DocumentSegmenter.createSegmenter(docId, path, new MemDocZoningApi)

    segmenter.runPageSegmentation()

    val content = formats.DocumentIO.documentToPlaintext(segmenter.mpageIndex)

    println(content)

  }
}
