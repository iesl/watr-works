package edu.umass.cs.iesl.watr
package extract

import segment._
// import spindex._
// import ComponentOperations._
// import ComponentTypeEnrichments._
// import GeometricFigure._
// import IndexShapeOperations._

class TextExtractionTest extends DocsegTestUtil {
  // import watrmarks._
  // import StandardLabels._

  behavior of "glyph extraction/identification"

// font-type-0-1-t.pdf
// font-type-1-3.pdf

  // import com.itextpdf.kernel.font._
  // import com.itextpdf.io.font._
  // import com.itextpdf.io.font._


  // it should "load fonts properly" in {
  //   val font = FontProgramFactory.createType1Font(
  //     "/usr/share/fonts/type1/gsfonts/a010013l.afm",
  //     "/usr/share/fonts/type1/gsfonts/a010013l.pfb"
  //   )
  //   println(
  //     font.getFontNames.getFullName.map(_.mkString(", ")).mkString(", ")
  //   )
  //   val g = font.getGlyph(1)


  // }



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
