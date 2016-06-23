package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._

// import spindex._
import segment._
// import IndexShapeOperations._
// import ComponentTypeEnrichments._
// import ComponentRendering._

import TypeTags._
import watrmarks.{StandardLabels => LB}

object TypeConverters {

  implicit class RicherShape(val shape: spindex.Shape) extends AnyVal {
    def toOverlay(): Overlay = shape match {
      case v: spindex.LTBounds => v.toBBox
      case v: spindex.Point => v.toPoint
      case v: spindex.Line => v.toLine

      case spindex.LBBounds(left, bottom, width, height) =>
        BBox(left, bottom-height, width, height)
    }
  }

  implicit class RicherLine(val ltb: spindex.Line) extends AnyVal {
    def toLine(): Line = Line(ltb.p1.toPoint, ltb.p2.toPoint)
  }

  implicit class RicherPoint(val ltb: spindex.Point) extends AnyVal {
    def toPoint(): Point = Point(ltb.x, ltb.y)
  }

  implicit class RicherLTBounds(val ltb: spindex.LTBounds) extends AnyVal {
    def toBBox(): BBox = BBox(ltb.left, ltb.top, ltb.width, ltb.height)
  }
}

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
  import TypeConverters._


  def convertVisualTraceTypes(cc1: utils.VisualTrace.DSL[_]): VisualTrace.DSL = {
    import spindex._
    cc1 match {
      case utils.VisualTrace.Noop                              => VisualTrace.Noop
      case utils.VisualTrace.SetViewport(b: LTBounds)          => VisualTrace.SetViewport(b.toBBox)
      case utils.VisualTrace.GetViewport()                     => VisualTrace.GetViewport()
      case utils.VisualTrace.Show(s: Shape)                    => VisualTrace.Show(s.toOverlay)
      case utils.VisualTrace.ShowVDiff(d1: Double, d2: Double) => VisualTrace.ShowVDiff(d1, d2)
      case utils.VisualTrace.FocusOn(s: Shape)                 => VisualTrace.FocusOn(s.toOverlay)
      case utils.VisualTrace.HRuler(s: Double)                 => VisualTrace.HRuler(s)
      case utils.VisualTrace.Message(s: String)                => VisualTrace.Message(s)
      case utils.VisualTrace.And(t1, t2)                       =>  ???
        // VisualTrace.And(convertVisualTraceTypes(t1),convertVisualTraceTypes(t2))
      case utils.VisualTrace.AndThen(t1, t2)                   => ??? // VisualTrace.AndThen(t1, t2)
    }
  }


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


    // import VisualTrace._
    // import play.api.libs.json
    // import json._

    TraceLog.getTraceLog().map(convertVisualTraceTypes(_)).toList


  }

}
