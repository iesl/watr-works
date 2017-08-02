package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}

import TypeTags._
  // import textreflow.data._

class DocumentIOTests extends DocsegTestUtil  {

  it should "extract text" in {

    val allTestPdfs =
      """|
         |example-from-latex.pdf.d/pg_0015.pdf
         |example-from-latex.pdf.d/example-from-latex.pdf
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

    val content = formats.DocumentIO.richTextSerializeDocument(segmenter.mpageIndex)

    // println(content)

  }
}
