package edu.umass.cs.iesl.watr
package segment

import corpora._
// import edu.umass.cs.iesl.watr.tracing._
import edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => VTL}

class PageTextTest extends SegmentationTestUtils  {
  /**

    These are meant to be minimal smokescreen tests to insure that basic text
    extraction is working reasonable well with respect to spacing,
    char/line ordering, super/subscripts, etc.

   */

  it should "extract text reasonably well" in {

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

    // tracing.VisualTracer.addTraceLevel(VTL.PrintLogs)

    // import ammonite.{ops => fs}
    // allTestPdfs.foreach {
    selectPdfPage(allTestPdfs, "1056", 1).foreach {
      case (docId, page, path) =>


        val segmenter = DocumentSegmenter.createSegmenter(docId, path, new MemDocZoningApi)

        segmenter.runPageSegmentation()

        val content = formats.DocumentIO.documentToStructuredPlaintext(segmenter.mpageIndex)

        // val textfile = s"${docId.unwrap}.txt"
        // val outputFile = fs.pwd / textfile
        // if (fs.exists(outputFile)) {
        //   fs.rm(outputFile)
        // }

        // fs.write(outputFile, content)

        println(content)
    }



  }
}
