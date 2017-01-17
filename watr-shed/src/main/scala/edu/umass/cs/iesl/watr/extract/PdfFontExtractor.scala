package edu.umass.cs.iesl.watr
package extract 

import java.io.{InputStream, IOException}

import _root_.com.itextpdf
import itextpdf.kernel.pdf._

import scala.collection.JavaConversions._


object FontExtractor {


  def extractFontObjects(stream: InputStream): String = {
    import TB._

    try {
      val reader = new PdfReader(stream)
      val document = new PdfDocument(reader)

      val fontsPerPage = for (pageNumber <- 1 to document.getNumberOfPages) yield {
        val pdfPage = document.getPage(pageNumber)
        val resources = pdfPage.getResources
        val pdfObject = resources.getPdfObject
        val fontDict = pdfObject.getAsDictionary(PdfName.Font)

        if (fontDict != null) {
          fontDict.keySet().toList.map { key =>
            val fontVal = fontDict.getAsDictionary(key)
            (key.toString, fontVal)
          }.toMap
        } else {
          Map[String, PdfObject]()
        }
      }

      val allFonts = fontsPerPage.reduce { _ ++ _ }

      val fontsBox = allFonts.toSeq.map{ case (name, fontObj) =>
        name atop fonts.formatting.formatObject(fontObj, reader)
      }

      render(vcat(fontsBox))

    } catch {
      case ex: IOException =>
        throw new Exception("Cannot extract characters from PDF file", ex)
      case ex: Throwable =>
        throw new Exception("Invalid PDF file", ex)
    }
  }



  // val ALT_TO_STANDART_FONTS = Map[String, PdfName](
  //   "CourierNew" -> PdfName.COURIER,
  //   "CourierNew,Bold" -> PdfName.COURIER_BOLD,
  //   "CourierNew,BoldItalic" -> PdfName.COURIER_BOLDOBLIQUE,
  //   "CourierNew,Italic" -> PdfName.COURIER_OBLIQUE,
  //   "Arial" -> PdfName.HELVETICA,
  //   "Arial,Bold" -> PdfName.HELVETICA_BOLD,
  //   "Arial,BoldItalic" -> PdfName.HELVETICA_BOLDOBLIQUE,
  //   "Arial,Italic" -> PdfName.HELVETICA_OBLIQUE,
  //   "TimesNewRoman" -> PdfName.TIMES_ROMAN,
  //   "TimesNewRoman,Bold" -> PdfName.TIMES_BOLD,
  //   "TimesNewRoman,BoldItalic" -> PdfName.TIMES_BOLDITALIC,
  //   "TimesNewRoman,Italic" -> PdfName.TIMES_ITALIC
  // )

  // def processAlternativeFontNames(resources: PdfDictionary): Unit = {
  //   val fontsDictionary = resources.getAsDict(PdfName.FONT)

  //   if (fontsDictionary == null) {
  //     return
  //   }
  //   for (pdfFontName <- fontsDictionary.getKeys()) {
  //     if (!(fontsDictionary.get(pdfFontName).isInstanceOf[PRIndirectReference])) {
  //       return
  //     } else {
  //       val indRef = fontsDictionary.get(pdfFontName).asInstanceOf[PRIndirectReference]
  //       val fontDictionary = PdfReader.getPdfObjectRelease(indRef).asInstanceOf[PdfDictionary]

  //       val baseFont = fontDictionary.getAsName(PdfName.BASEFONT)
  //       if (baseFont != null) {
  //         val fontName = PdfName.decodeName(baseFont.toString())
  //         if (fontDictionary.getAsArray(PdfName.WIDTHS) == null && ALT_TO_STANDART_FONTS.containsKey(fontName)) {
  //           fontDictionary.put(PdfName.BASEFONT, ALT_TO_STANDART_FONTS(fontName))
  //         }
  //       }
  //     }
  //   }
  // }

}
