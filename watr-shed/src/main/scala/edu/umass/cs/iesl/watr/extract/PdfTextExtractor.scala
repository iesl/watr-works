package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.canvas.parser.PdfCanvasProcessor
import java.io.{InputStream, IOException}

import _root_.com.itextpdf
import itextpdf.kernel.pdf._

import utils.IdGenerator
import spindex._
import GeometricFigure._
import TypeTags._
import scala.collection.mutable

import scalaz.{@@}

object util {

  implicit class RicherFloat(val d: Float) extends AnyVal {
    def nan = java.lang.Float.isNaN(d)
    def inf = java.lang.Float.isInfinite(d)
  }
  implicit class RicherDouble(val d: Double) extends AnyVal {
    def nan = java.lang.Double.isNaN(d)
    def inf = java.lang.Double.isInfinite(d)
  }
}

case class GeometryTranslation(
  transX: (Double) => Double,
  transY: (Double) => Double
)

class PdfTextExtractor(
  charsToDebug: Set[Int] = Set(),
  componentIdGen: IdGenerator[RegionID]
) {
  import scala.collection.JavaConversions._

  val bboxNames = List[PdfName](
    PdfName.CropBox,
    PdfName.TrimBox,
    PdfName.MediaBox,
    PdfName.BleedBox
  )
  def getBestBoundingBox(pdfPage: PdfPage): Array[Double] = {
    val parent = pdfPage.getPdfObject.getAsDictionary(PdfName.Parent)
    val bestBox = bboxNames
      .map({n => List(
        pdfPage.getPdfObject.getAsArray(n),
        parent.getAsArray(n)
      )})
      .flatten
      .filterNot(_ == null)
      .headOption.getOrElse { sys.error("no bounding box (media/crop/trim/etc) found in pdfobjects!") }

    val nums: Array[Double] = bestBox.toArray.map(_.asInstanceOf[PdfNumber].doubleValue())

    nums
  }

  def getReportedPageGeometry(pageId: Int@@PageID, pdfPage: PdfPage, reader: PdfReader): (PageGeometry, GeometryTranslation) = {

    val nums: Array[Double] = getBestBoundingBox(pdfPage)

    val lval = nums(0)
    val bval = nums(1)
    val rval = nums(2)
    val tval = nums(3)

    def xtrans(x: Double): Double = {
      x - lval
    }
    def ytrans(y: Double): Double = {
      (tval - y)
    }

    val left = xtrans(lval)
    val top = ytrans(tval)
    val right = xtrans(rval)
    val bottom = ytrans(bval)

    val bounds = LTBounds(
      left = left.toDouble,
      top = top.toDouble,
      width = right.toDouble,
      height = bottom.toDouble
    )

    (PageGeometry(pageId, bounds, None),
      GeometryTranslation(xtrans, ytrans))

  }


  var pagesInfo: List[(PageAtoms, PageGeometry)] = List()

  def extractCharacters(stream: InputStream): Unit = {
    try {
      val reader = new PdfReader(stream)
      val document = new PdfDocument(reader)

      for (pageNumber <- 1 to document.getNumberOfPages) {
        // val tagStructureContext = pdfPage.getDocument.getTagStructureContext

        val pageId = PageID(pageNumber-1)

        val pdfPage = document.getPage(pageNumber)


        val resources = pdfPage.getResources

        val currCharBuffer: mutable.ArrayBuffer[PageAtom] = mutable.ArrayBuffer[PageAtom]()
        val (pageGeometry, geomTrans) = getReportedPageGeometry(pageId, pdfPage, reader)

        val extractor = new CharExtractionListener(
          reader, charsToDebug,
          componentIdGen, currCharBuffer, pdfPage, pageId, pageGeometry, geomTrans
        )

        val parser = new PdfCanvasProcessor(extractor);
        parser.processPageContent(pdfPage)


        val pageCharAtoms = Seq[PageAtom](currCharBuffer:_*)

        val pageChars = PageAtoms(pageId, pageCharAtoms)

        pagesInfo = pagesInfo :+ (
          (pageChars, pageGeometry)
        )

        parser.reset()

      }

    } catch {
      case ex: IOException =>
        throw new Exception("Cannot extract characters from PDF file", ex)
      case ex: Throwable =>
        throw new Exception("Invalid PDF file", ex)
    }
  }
  import com.itextpdf.kernel.font._
  import com.itextpdf.io.font._
  import com.itextpdf.io.font._
  // import com.itextpdf.kernel.font.IDocFontProgram

  def extractGlyphs(stream: InputStream): Unit = {
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

          if (pdfFont.getFontProgram.isInstanceOf[Type1Font]) {
            val fontProgram = pdfFont.getFontProgram.asInstanceOf[Type1Font]
            if (fontProgram.isBuiltInFont()) {
              println("builtin font")
            } else {
              println("pdf stream bytes")
              val stream = fontProgram.getFontStreamBytes
            }
            // val fontProgram = pdfFont.getFontProgram.asInstanceOf[Type1Font].getFontStreamBytes()



          }


          // val ftype = fontVal.getAsString(PdfName.Type)
          // val baseFont = fontVal.getAsString(PdfName.BaseFont)
          // val fname = fontVal.getAsString(PdfName.Name)

          val subtype = fontVal.getAsName(new PdfName("Subtype"))
          val subtype2 = fontVal.getAsName(PdfName.Subtype2)
          if (subtype == PdfName.Type0) {
            println("Type0")
            println(fonts.formatting.formatObject(fontVal, reader))
          } else if (subtype == PdfName.Type1) {
            println("Type1")
            val fontDescriptor = fontVal.getAsDictionary(PdfName.FontDescriptor)
            if (fontDescriptor!=null) {
              val ff2 = fontDescriptor.getAsStream(PdfName.FontFile2)
              val ff3 = fontDescriptor.getAsStream(PdfName.FontFile3)
              if (ff2!=null) {
                println(s"fontfile2: len = ${ff2.getBytes.length}")

              } else if (ff3!=null) {
                println(s"fontfile3: len = ${ff3.getBytes.length}")

              }
            }
          } else if (subtype == PdfName.Type3) {
            println("Type3")
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

  // def processAlternativeColorSpace(resources: PdfDictionary): Unit = {
  //   val csDictionary = resources.getAsDict(PdfName.COLORSPACE)
  //   if (csDictionary == null) {
  //     return
  //   }
  //   for (csName <- csDictionary.getKeys()) {
  //     if (csDictionary.getAsArray(csName) != null) {
  //       csDictionary.put(csName, PdfName.DEVICEGRAY)
  //     }
  //   }
  // }

}
