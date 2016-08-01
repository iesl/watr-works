package edu.umass.cs.iesl.watr
package extract
package fonts

import com.itextpdf
import itextpdf.kernel.pdf.PdfReader
import com.itextpdf.io.font.FontProgramFactory
import com.itextpdf.io.font.otf.Glyph
import com.itextpdf.kernel.pdf.PdfName
// import com.itextpdf.io.font.{ FontIdentification, FontNames, FontProgram, FontProgramFactory }
// import com.itextpdf.kernel.font.PdfFont
// import com.itextpdf.kernel.geom.LineSegment
// import com.itextpdf.kernel.geom
// import com.itextpdf.kernel.pdf.{ PdfArray, PdfIndirectReference, PdfStream, PdfString }
// import com.itextpdf.kernel.pdf.PdfDictionary;
// import com.itextpdf.kernel.pdf.PdfObject
import itextpdf.kernel.pdf.canvas.parser.data._


import DocumentFontInfo._

object GlyphPositioning {
  // http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/PDF32000_2008.pdf

  /**
    |   BT
    |   0.8182 0 0 1 53.52 661.2  Tm
    |     =>   0.8182   0.0    0
    |          0.0      1.0    0
    |         53.52   661.2    1
    |   3  Tr
    |      => Set Text Render Mode
    |   0 0 0 rg
    |      => Set non-stroke color (0 0 0 = rgb black)
    |   /F0 5.76  Tf
    |       => Set font w/ size = 5.76
    |   -0.1107  Tc
    |       => Set Char Spacing
    |   0  Tw
    |       => Set Word Spacing
    |   (JOURNAL ) Tj
    |       => Draw Text
    |   0  Tr    42.8269 0 TD   3 Tr    -0.0403 Tc     -0.0854 Tw    (OF )        Tj
    |   0  Tr    14.9601 0 TD   3 Tr    0.0042  Tc     -0.1397 Tw    (SOLID )     Tj
    |   0  Tr    28.7468 0 TD   3 Tr    -0.0169 Tc     -0.114  Tw    (STATE )     Tj
    |   0  Tr    30.2135 0 TD   3 Tr    -0.1186 Tc     0.0103  Tw    (CHEMISTRY ) Tj
    |   0  Tr
    |   ET
    */

  def traceGlyphPositioning(tri: TextRenderInfo, reader: PdfReader): Unit = {
    val graphicState = tri.gs
    val font = tri.getFont
    val fontProgramEmbedded = tri.getFont.getFontProgram
    val maybeFontProgram = FontLookup.getFontProgram(fontProgramEmbedded.getFontNames.getFontName.toLowerCase())
    val fontProgram = maybeFontProgram.getOrElse { fontProgramEmbedded }
    val fontMetrics = font.getFontProgram.getFontMetrics

    println(outputFontProgramInfo(fontProgram))
    println(outputFontMetrics(fontMetrics))

    // 9.2.4 Glyph Positioning and Metrics
    val charCode = tri.getCharCode(tri.getText)
    val glyph = fontProgram.getGlyph(charCode)

    println(outputGlyphInfo(glyph, reader))

    // Transform Glyph -> Text Space
    //   For all but Type3 fonts text-space = glyph-space*1000
    //   For Type3, T=FontMatrix
    val fontMatrix = tri.fontMatrix
    println(formatMatrixArr(fontMatrix, Some("FontMatrix")))


    // Get font horizontal displacement (width)
    //   From font dict
    val fontDictWidths = font.getPdfObject.get(PdfName.Widths)
    //   From font program
    val fontProgramWidths = fontProgram.getFontMetrics.getGlyphWidths()


    println(outputGraphicsState(graphicState))

    // Text state parameters:
    //   Tc Character spacing
    val charSpacing = graphicState.getCharSpacing()
    //   Tw Word spacing
    val wordSpacing = graphicState.getWordSpacing()
    //   Th Horizontal scaling
    val hScale = graphicState.getHorizontalScaling()
    //   Tl Leading
    val leading = graphicState.getLeading()
    //   Tf Text font
    //   Tfs Text font size
    val fontSize = graphicState.getFontSize
    //   Tmode Text rendering mode
    val textRenderMode = graphicState.getTextRenderingMode
    //   Trise Text rise
    val textRise = graphicState.getTextRise
    //   Tk Text knockout
    val textKnockout = graphicState.getTextKnockout

    // Get font width info:
    val charWidth = getCharWidth(glyph: Glyph, fontSize: Float, hScale: Float, charSpacing: Float, wordSpacing: Float)
    println(s"Char Width = ${charWidth}")
  }

  val TEXT_SPACE_COEFF = 1000

  def getCharWidth(g: Glyph, fontSize: Float, hScale: Float=1f, characterSpacing: Float, wordSpacing: Float) = {

    var resultWidth = g.getWidth() * fontSize * hScale

    resultWidth += characterSpacing * hScale * TEXT_SPACE_COEFF

    if (g.hasValidUnicode() && g.getUnicode() == ' ') {
      resultWidth += wordSpacing * hScale * TEXT_SPACE_COEFF
    }
    resultWidth
  }



}

object GlyphTest extends App {
  val arialFontPath = "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf"
  val arialFont = FontProgramFactory.createFont(arialFontPath)
  val fontMetrics = arialFont.getFontMetrics
  println(outputFontProgramInfo(arialFont))
  println(outputFontMetrics(fontMetrics))

}
