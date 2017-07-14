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

    // val pdfIns = papers.paperUrl("saccharomyces-1-page.pdf")
    val pdfIns = papers.paperUrl("austenite/pg_0001.pdf")
    val path = fs.Path(pdfIns.getPath)
    val docId = DocumentID("dummy-id")
    import corpora._

    val segmenter = DocumentSegmenter.createSegmenter(docId, path, new MemDocZoningApi)
    val rTreePageSegmenter = new RTreePageSegmentation(
      segmenter.mpageIndex
    )

    rTreePageSegmenter.runPageSegmentation()
    // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Print
    // segmenter.runLineDeterminationOnPage(PageID(1), PageNum(0))
    // val docStore = segmenter.docStore

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
