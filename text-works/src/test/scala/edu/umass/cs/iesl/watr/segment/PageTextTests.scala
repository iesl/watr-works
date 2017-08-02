package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}

import TypeTags._
// import textreflow.data._

class PageTextTest extends DocsegTestUtil  {
  /**
    These are meant to be minimal smokescreen tests to insure that basic text
    extraction is working reasonable well with respect to spacing, spacing,
    char/line ordering, super/subscripts, etc.
   */

  it should "extract text" in {

    val expectedText = {
      """|
         | bioRxiv preprint first posted online Nov. 30, 2016; doi: http://dx.doi.org/10.1101/090498. The copyright holder for this preprint (which was not
         |                   peer-reviewed) is the author/funder. It is made available under a CC-BY-NC-ND 4.0 International license.
         |
         |
         |          The effect of known mutator alleles in a cluster of clinical
         |                      Saccharomyces cerevisiae strains
         |
         |    Daniel A. Skelly^{1,3}, Paul M. Magwene^{1}, Brianna Meeks^{2}, Helen A.Murphy^{2}
         |
         |    ^{1}Department of Biology, Duke University, Durham, North Carolina
         |    ^{2}Department of Biology, The College of William and Mary, Williamsburg, Virginia
         |    ^{3}Current address: The Jackson Laboratory, Bar Harbor, Maine
         |
         |""".stripMargin
    }
    expectedText.split("\n").map(_.trim())

    // 6376.pdf  => page 2 column seg is wrong, many paths making up graphs kill performance
    // 101016jcarbon201301056.pdf => reference section columns are wrong, page 1 has repeated lines
    // bongard2005.pdf => reference section is dividing into 2 cols, one for markers one for ref

    val allTestPdfs =
      """|
         |101016jcarbon201301056.pdf.pages/pg_0001.pdf
         |101016jcarbon201301056.pdf
         |bongard2005.pdf
         |austenite.pdf
         |101016jactamat201112024.pdf
         |6376.pdf
         |2839.pdf
         |saccharomyces-1-page.pdf
         |hep-ph9503349.pdf
         |astro-ph9903259.pdf
         |cond-mat9803032.pdf
         |101016jactamat201501032.pdf
         |101016japsusc201210126.pdf
         |acsnano.5b00028.pdf
         |font-type-0-1-t.pdf
         |font-type-1-3.pdf
         |quantification-Smith-2013.pdf
         |Schauer-1987.pdf
         |""".stripMargin.split("\n")
        .map(_.trim())
        .filter(_.length()>0)



    val pdfName = allTestPdfs(0)
    val pdfIns = papers.paperUrl(pdfName)
    val path = fs.Path(pdfIns.getPath)
    val stableIdent = pdfName.replaceAll("/","_")
    val docId = DocumentID(stableIdent)
    import corpora._

    // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Print

    val segmenter = DocumentSegmenter.createSegmenter(docId, path, new MemDocZoningApi)

    segmenter.runPageSegmentation()
    // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Print
    // segmenter.runLineDeterminationOnPage(PageID(1), PageNum(0))
    // val docStore = segmenter.docStore

    val content = formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex)

    // println(content)
    // println("All VisualLines")

    // for {
    //   docId    <- docStore.getDocument(docId).toList
    //   pageId   <- docStore.getPages(docId)
    //   (lineZone, lineNum) <- docStore.getPageVisualLines(pageId).zipWithIndex
    //   reflow   <- docStore.getTextReflowForZone(lineZone.id)
    // } yield {
    //   val asText = reflow.toFormattedText()

    //   println(asText)
    //   (lineNum, asText)
    // }

  }
}
