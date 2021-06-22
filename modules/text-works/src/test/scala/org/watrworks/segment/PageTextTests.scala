package org.watrworks
package segment

import TypeTags._
import utils.PathUtils._
import utils.ExactFloats._

class PageTextTest extends SegmentationTestUtils {

  // it should "extract text reasonably well" in {

  //   val pathstr   = "./latex-src.d/sample1.pdf"
  //   val path      = strToAmmPath(pathstr)
  //   val docId     = DocumentID("SampleDoc")
  //   val segmenter = DocumentSegmenter.createSegmenter(docId, path)

  //   segmenter.docScope.docStats.initTable[Int @@ PageNum, String @@ ScaledFontID, Int @@ FloatRep](
  //     "PagewiseLineWidths"
  //   )

  //   segmenter.pageSegmenters.foreach { p =>
  //     p.findContiguousGlyphSpans()
  //   }
  //   segmenter.computeScaledFontHeightMetrics(LB.CharRunFontBaseline)
  //   segmenter.computeScaledSymbolicFontMetrics()

  //   println(segmenter.fontDefs.report())

  //   segmenter.pageSegmenters.foreach { p =>
  //     p.buildGlyphTree()
  //   }

  // }

  import transcripts.{Transcript => T}
  import cats.data.State
  import cats.Eval
  import cats.data.IndexedStateT

  it should "use state/eval properly" in {
    val initState = State[T.Label, Unit](traceLabel => (traceLabel, ()))

    initState.modify(l => l.withChildren())
    initState

  }
}
