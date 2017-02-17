package edu.umass.cs.iesl.watr
package extract

import java.io.{InputStream, IOException}

import com.itextpdf
import itextpdf.kernel.pdf._
import itextpdf.kernel.font._
import itextpdf.io.font._

import scala.collection.JavaConversions._

object GlyphExtractor {

  def extractGlyphs(stream: InputStream): String = {
    try {
      val reader = new PdfReader(stream)
      val document = new PdfDocument(reader)

      for (pageNumber <- 1 to document.getNumberOfPages) {
        println(s"page ${pageNumber}")
        val pdfPage = document.getPage(pageNumber)
        val resources = pdfPage.getResources
        val pdfObject = pdfPage.getResources.getPdfObject
        val fontDict = pdfObject.getAsDictionary(PdfName.Font)
        // val glyph = pdfFont.getGlyph(1)

        fontDict.keySet().foreach{ key =>
          println("Font...")
          val fontVal = fontDict.getAsDictionary(key)
          val pdfFont = PdfFontFactory.createFont(fontVal)
          println(fonts.formatting.formatObject(fontVal, reader))

          if (pdfFont.getFontProgram.isInstanceOf[Type1Font]) {
            val fontProgram = pdfFont.getFontProgram.asInstanceOf[Type1Font]
            if (fontProgram.isBuiltInFont()) {
              // println("builtin font")
            } else {
            }
          }


          // val ftype = fontVal.getAsString(PdfName.Type)
          // val baseFont = fontVal.getAsString(PdfName.BaseFont)
          // val fname = fontVal.getAsString(PdfName.Name)
          val subtype = fontVal.getAsName(new PdfName("Subtype"))
          val subtype2 = fontVal.getAsName(PdfName.Subtype2)

          if (subtype == PdfName.Type0) {
            println("Type0")
            // println(fonts.formatting.formatObject(fontVal, reader))
          } else if (subtype == PdfName.Type1) {
            // println("Type1")
            val fontDescriptor = fontVal.getAsDictionary(PdfName.FontDescriptor)
            if (fontDescriptor!=null) {
              val ff2 = fontDescriptor.getAsStream(PdfName.FontFile2)
              val ff3 = fontDescriptor.getAsStream(PdfName.FontFile3)
              if (ff2!=null) {
                // println(s"fontfile2: len = ${ff2.getBytes.length}")

              } else if (ff3!=null) {
                // println(s"fontfile3: len = ${ff3.getBytes.length}")

              }
            }
          } else if (subtype == PdfName.Type3) {
            // println("Type3")
            val charProcs = fontVal.getAsDictionary(PdfName.CharProcs)
            charProcs.keySet().foreach { charProcKey  =>
              val charProc = charProcs.getAsStream(charProcKey)
              val cpBytes= charProc.getBytes
              val l = cpBytes.length
              // println(s"glyph ${charProcKey}: len = ${l}")
            }
          } else {

          }

        }

      }

    } catch {
      case ex: IOException =>
        throw new Exception("Cannot extract characters from PDF file", ex)
      case ex: Throwable =>
        throw new Exception("Invalid PDF file", ex)
    }

    ""
  }

}
