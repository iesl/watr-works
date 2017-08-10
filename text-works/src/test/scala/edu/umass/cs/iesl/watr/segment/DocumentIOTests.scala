package edu.umass.cs.iesl.watr
package segment

import corpora._

class DocumentIOTests extends DocsegTestUtil  {

  behavior of "Segmentation IO"

  val allTestPdfs = parsePdfs {
    """|
       |example-from-latex.pdf.d/pg_0015.pdf
       |example-from-latex.pdf.d/example-from-latex.pdf
       |""".stripMargin
  }

  it should "save/load page indexes and rtrees" in {

    val (docId, path) = allTestPdfs(0)

    val segmenter = DocumentSegmenter.createSegmenter(docId, path, new MemDocZoningApi)

    segmenter.runPageSegmentation()

    val mpageIndex = segmenter.mpageIndex
    mpageIndex

    // val content = formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex)

    // println(content)

  }
}
