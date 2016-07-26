package edu.umass.cs.iesl.watr
package extract

import segment._
import spindex._
// import ComponentOperations._
import ComponentTypeEnrichments._
import GeometricFigure._
import IndexShapeOperations._

class TextExtractionTest extends DocsegTestUtil {
  // import watrmarks._
  // import StandardLabels._

  behavior of "glyph extraction/identification"

// font-type-0-1-t.pdf
// font-type-1-3.pdf



  it should "properly extract the following characters" in {
    // val paper1 = papers.paper("font-type-1-3.pdf")
    val paper1 = papers.paper("font-type-0-1-t.pdf")

      val charExtractor = new PdfTextExtractor(
        Set(),
        utils.IdGenerator[RegionID]()
      )

    charExtractor.extractGlyphs(paper1)


  }

}
