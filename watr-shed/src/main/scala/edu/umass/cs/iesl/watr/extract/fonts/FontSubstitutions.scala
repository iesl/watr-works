package edu.umass.cs.iesl.watr
package extract
package fonts


/*
 Liberation is the collective name of four TrueType font families: Liberation
 Sans, Liberation Sans Narrow, Liberation Serif and Liberation Mono. These fonts
 are metrically compatible with Monotype Corporation's Arial, Arial Narrow,
 Times New Roman, and Courier New (respectively), the most commonly used fonts
 on Microsoft Windows operating system and Office suite, for which Liberation is
 intended as free substitute.[2]


 Although the characters are not exactly the same, Nimbus Sans L has metrics almost identical to Helvetica and Arial.


 One of the best-known Palatino PostScript clones is "Book Antiqua" (originally by Monotype),



 Table:




 Nimbus Sans L ~= Liberation Sans
 Helvetica = Arial
 */

object FontLookup {
  import com.itextpdf.kernel.pdf._
  import com.itextpdf.io.font.FontProgramFactory
  import com.itextpdf.io.font.FontProgram
  import scala.collection.mutable


  val fontPrograms = mutable.HashMap[String, FontProgram]()

  val fontPaths = List[(String, String)](
  )

  def init(): Unit = {
    fontPaths.foreach { case (fname, fpath) =>
      fontPrograms.put(fname, FontProgramFactory.createFont(fpath))
    }
  }

  init()


  def getFontProgram(fontName: String): Option[FontProgram] = {
    fontPrograms.get(fontName)
  }

  // def substituteFontProgram()
  // /BaseFont :: name => /Arial
  //             /Encoding :: name => /WinAnsiEncoding
  //             /Name :: name => /F0
  //             /Subtype :: name => /TrueType
  //             /Type :: name => /Font
  val fontSubs = Map(
    "Arial" ->  "Liberation Sans",
    "Arial Narrow" ->  "Liberation Sans Narrow",
    "Times New Roman" ->  "Liberation Serif",
    "Courier New" ->  "Liberation Mono",
    "Book Antiqua" ->  "Palitino"
  )
  def getSubstituteFontName(fontName: String): Option[String] ={
    val hasEmbeddingPrefix = fontName.matches("""^/\\w{6}\\+.*""")
/*
 Arial
 ArialBlack
 Arial,Bold
 Arial,BoldItalic
 Arial-BoldItalicMT
 Arial,Italic
 ArialMT
 ArialUnicodeMS-KSCms-UHC-H-Identity-H

 BookAntiqua
 BookAntiqua,Bold
 BookAntiqua,BoldItalic
 BookmanOldStyle

 Candara

 CenturySchoolbook,Bold

 Courier-Bold
 Courier-BoldOblique
 CourierNew
 CourierNew,Bold
 CourierNew,BoldItalic
 CourierNew,BoldItalic
 CourierNew,Italic
 Courier-Oblique

 Helvetica
 Helvetica-Bold
 Helvetica-BoldOblique
 Helvetica-BoldOblique
 HelveticaNeue-Black
 HelveticaNeue-BoldCond
 HelveticaNeue-Condensed
 HelveticaNeue-Italic
 HelveticaNeue-LightCond
 HelveticaNeue-Roman
 Helvetica-Oblique

 Times-Bold
 Times-BoldItalic
 Times-Italic
 TimesNewRoman
 Times.New.Roman075
 TimesNewRoman,Bold
 TimesNewRoman,BoldItalic
 TimesNewRoman,BoldItalic
 TimesNewRoman,Italic
 TimesNewRoman,Italic
 TimesNewRomanMTExtraBold,Bold
 TimesNewRomanPS-BoldItalicMT
 TimesNewRomanPSMT
 Times-Roman

 Symbol
 SymbolMT
 ZapfDingbats

 KozGoPro-Regular-90msp-RKSJ-H-Identity-H

 Minion-Bold
 Minion-Italic
 Minion-Regular

 MS-Mincho-90ms-RKSJ-H-Identity-H
 PMingLiU-Identity-H
 SimSun-GBK-EUC-H-Identity-H
 */

    None
  }

  def checkMaybeNonEmbeddedFont(fontObject: PdfDictionary): Option[FontProgram] = {

    val baseFont = fontObject.getAsString(PdfName.BaseFont).getValue
    val encoding = fontObject.get(PdfName.BaseFont)
    if (encoding.isName()) {

    }


    None
  }

  def getFontProgram(fontObject: PdfDictionary): FontProgram = {
    // Jump through hoops to figure out whether which FontProgram to use

    // Font is non-embedded, standard encoding,
    checkMaybeNonEmbeddedFont(fontObject)



    ???
  }
}


object GlyphHashLookup {
  import java.io.BufferedInputStream
  import java.util.zip.GZIPInputStream

  def hex2int(h: String) = Integer.parseInt(h, 16)

  def apply(): GlyphHashLookup = {
    val inputStream = new GZIPInputStream(new BufferedInputStream(this.getClass.getResourceAsStream("/glyph-hash.tsv.gz")))
    val sourceFile = io.Source.fromInputStream(inputStream)

    val mappings = sourceFile.getLines
      .map({line =>
        val recs = line.split("\t")
        val hash = recs(0).trim
        val names = recs(1).trim
        val unicodes = recs(2).trim.split(",").map(_.trim).filterNot(_.toLowerCase=="ffffffff").map(hex2int(_))
        val widths = recs(3).trim.split(",").map(_.trim).map(_.toInt)

        unicodes
          .filterNot(_ == -1)
          .headOption.map({ unicode =>
            (hash, unicode)
          })
      })
      .toSeq
      .flatten

    new GlyphHashLookup(mappings.toMap)
  }

  lazy val global = apply()
}

class GlyphHashLookup(mapping: Map[String, Int]) {
  def lookup(hash: String): Option[Char] = {
    mapping.get(hash).map(_.toChar)
  }

}
