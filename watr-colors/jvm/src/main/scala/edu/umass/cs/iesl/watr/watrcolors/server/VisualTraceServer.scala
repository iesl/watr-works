package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._

import segment._

// import IndexShapeOperations._
// import ComponentTypeEnrichments._
// import ComponentRendering._

import TypeTags._
import watrmarks.{StandardLabels => LB}


class VisualTraceServer(
  rootDirectory: Path
) extends VisualTraceApi  {
  lazy val corpus = Corpus(rootDirectory)

  def createView(): List[HtmlUpdate] = {
    List(
      HtmlReplaceInner("#main", new html.VisualTraceView().init().toString)
    )
  }

  def runTrace(): List[VisualTrace.DSL] = {
    // Hard-code the pdf/line info we are interested in:
    // Text should be: JOURNAL OF SOLID STATE CHEMISTRY 78, 294–300 (1989)
    val page = 0
    val corpusEntryId = "0575.pdf.d"
    val bbox = spindex.LTBounds(
      53.52, 52.42, 206.14, 8.21
    )

    // println(s"getting corpusEntry '${corpusEntryId}'")
    (for {
      entry <- corpus.entry(corpusEntryId).toList
      pdfArtifact <- entry.getPdfArtifact
      // f <- pdfArtifact.asPath.toOption
      pdfIns <- pdfArtifact.asInputStream.toOption
    } yield {
      // val corpusPath = f.relativeTo(corpus.corpusRoot)
      // println(s"VisualTrace: createView(${corpusEntryId}) path=(${corpusPath})")

      testExample(pdfIns)
    }).flatten
  }

  import java.io.InputStream
  import utils.TraceLog




  def testExample(pdfIns: InputStream): List[VisualTrace.DSL] = {
    import spindex._

    val segmenter = DocumentSegmenter.createSegmenter(pdfIns)

    val pageId = PageID(0)

    val bbox = LTBounds(
      53.52, 52.42, 206.14, 8.21
    )
    val totalBounds = bbox

    val interestingChars = segmenter.zoneIndexer
      .getPageInfo(pageId)
      .charAtomIndex
      .queryForIntersects(totalBounds)

    // println("["+squishb(interestingChars)+"]")


    segmenter.runLineDetermination()

    import spindex.ComponentOperations._

    // find visual lines in bounds:
    val lineComponents = segmenter.zoneIndexer
      .getPageInfo(pageId)
      .componentIndex
      .queryForIntersects(totalBounds)
      .sortBy(_.bounds.top)
      .filter(_.getLabels.contains(LB.VisualLine))

    val tokenizedLines = lineComponents.map { lineComponent =>
      lineComponent.tokenizeLine().toText
    }


    import TypeConverters._

    TraceLog.getAndClearTraceLog().map(convertVisualTraceTypes(_)).toList


  }

}
