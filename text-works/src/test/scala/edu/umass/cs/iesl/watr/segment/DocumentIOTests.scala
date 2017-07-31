package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}

import TypeTags._
  // import textreflow.data._

class DocumentIOTests extends DocsegTestUtil  {

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

    val allTestPdfs =
      """|
         |saccharomyces-1-page.pdf
         |""".stripMargin.split("\n")
        .map(_.trim())
        .filter(_.length()>0)



    val pdfName = allTestPdfs(0)
    val pdfIns = papers.paperUrl(pdfName)
    val path = fs.Path(pdfIns.getPath)
    val docId = DocumentID(s"${pdfName}")
    import corpora._

    val segmenter = DocumentSegmenter.createSegmenter(docId, path, new MemDocZoningApi)

    segmenter.runPageSegmentation()

    val content = formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex)

    println(content)

  }
}
