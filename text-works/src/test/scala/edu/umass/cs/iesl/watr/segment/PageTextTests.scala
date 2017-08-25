package edu.umass.cs.iesl.watr
package segment

import corpora._
import edu.umass.cs.iesl.watr.tracing.{VisualTracer, TraceCallbacks}
import ammonite.{ops => fs}

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

    val allTestPdfs = parsePdfsSinglePages {
      """|101016jactamat201112024.pdf.pages   #22
         |101016jactamat201501032.pdf.pages   #13
         |101016japsusc201210126.pdf.pages    #10
         |101016jcarbon201301056.pdf.pages    #09
         |1609.03499.pdf.pages                #15
         |2839.pdf.pages                      #06
         |6376.pdf.pages                      #03
         |acsnano.5b00028.pdf.pages           #12
         |astro-ph9903259.pdf.pages           #03
         |austenite.pdf.pages                 #12
         |bongard2005.pdf.pages               #10
         |cond-mat9803032.pdf.pages           #15
         |example-from-latex.pdf.pages        #24
         |gr-qc9903064.pdf.pages              #04
         |hep-ph9503349.pdf.pages             #94
         |hep-th9903041.pdf.pages             #09
         |quantification-Smith-2013.pdf.pages #05
         |saccharomyces-1-page.pdf.pages      #01
         |Schauer-1987.pdf.pages              #02
         |""".stripMargin
    }

    // tracing.VisualTracer.visualTraceLevel = tracemacros.VisualTraceLevel.Debug

    selectPdfPage(allTestPdfs, "1056", 2).foreach {
      case (docId, page, path) =>

        val tracer = new VisualTracer {
          def traceCallbacks: TraceCallbacks = new TraceCallbacks {
          }
        }

        val segmenter = DocumentSegmenter.createSegmenter(docId, path, new MemDocZoningApi, tracer)

        segmenter.runPageSegmentation()

        val content = formats.DocumentIO.documentToPlaintext(segmenter.mpageIndex)

        val textfile = s"${docId.unwrap}.txt"

        fs.write(fs.pwd / textfile, content)

        println(content)
    }



  }
}
