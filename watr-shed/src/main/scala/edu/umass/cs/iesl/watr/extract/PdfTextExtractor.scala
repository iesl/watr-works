package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.canvas.parser.PdfCanvasProcessor
import java.io.{InputStream, IOException}

import _root_.com.itextpdf
import itextpdf.kernel.pdf._

// import scala.collection.JavaConversions._
// import scala.collection.JavaConverters._
import utils.IdGenerator
import spindex._
import TypeTags._
import scala.collection.mutable
// import textboxing.{TextBoxing => TB}

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

// import UnicodeUtil._

class PdfTextExtractor(
  charsToDebug: Set[Int] = Set(),
  componentIdGen: IdGenerator[RegionID]
) {
  val DEFAULT_FRONT_PAGES_LIMIT = 20
  val DEFAULT_BACK_PAGES_LIMIT = 20
  val frontPagesLimit = DEFAULT_FRONT_PAGES_LIMIT
  val backPagesLimit = DEFAULT_BACK_PAGES_LIMIT

  // val fontDict = mutable.HashMap[String, DocumentFont]()

  // var bxDocumentCreator:MyBxDocumentCreator = _

  // def getZoneRecords() = bxDocumentCreator.getZoneRecords

  def getReportedPageGeometry(pageId: Int@@PageID, pdfPage: PdfPage): PageGeometry = {
    // val borders = if (pageRectangle.hasBorders) {
    //   Some(Borders(
    //     bleft = pageRectangle.getBorderWidthLeft.toDouble,
    //     bbottom = pageRectangle.getBorderWidthBottom.toDouble,
    //     btop = pageRectangle.getBorderWidthTop.toDouble,
    //     bright = pageRectangle.getBorderWidthRight.toDouble
    //   ))
    // } else None

    // println(s"""| Extracting page Bounds
    //             |  Top       ${pageRectangle.getTop}
    //             |  Bottom    ${pageRectangle.getBottom}
    //             |  Left      ${pageRectangle.getLeft}
    //             |  Right     ${pageRectangle.getRight}
    //             |  Width     ${pageRectangle.getWidth}
    //             |  Height    ${pageRectangle.getHeight}
    //             |  Border    ${pageRectangle.getBorder}
    //             |  Borders   ${pageRectangle.hasBorders}
    //             |""".stripMargin)

    // PageRectangle coords have origin @ lower left (normal cartesian origin)
    val pageRectangle  = pdfPage.getPageSize
    val bounds = LTBounds(
      left = pageRectangle.getLeft.toDouble,
      top = pageRectangle.getBottom.toDouble, // ???
      width = pageRectangle.getWidth.toDouble,
      height = pageRectangle.getHeight.toDouble
    )

    PageGeometry(pageId, bounds, None)

  }


  var pagesInfo: List[(PageAtoms, PageGeometry)] = List()

  def extractCharacters(stream: InputStream): Unit = {
    try {
      val reader = new PdfReader(stream)
      val document = new PdfDocument(reader)


      // parser.processContent(x$1: Array[Byte], x$2: PdfResources)
      // val processor = new PdfContentStreamProcessor(documentCreator)

      // {
      //   formatting.followIndirect = false

      //   import ammonite.ops._
      //   val tf = cwd / s"pdf-trailer.txt"
      //   val cf = cwd / s"pdf-catalog.txt"
      //   rm(tf)
      //   rm(cf)

      //   val nn1 = reader.getNamedDestinationFromNames
      //   val nn2 = reader.getNamedDestinationFromStrings
      //   println("getNamedDestinationFromNames")
      //   println(nn1.keys.mkString(", "))
      //   println("getNamedDestinationFromStrings")
      //   println(nn2.keys.mkString(", "))

      //   val objf = cwd / s"pdf-name-objs.txt"
      //   rm(objf)

      //   nn2.foreach { case (name, pdfobj) =>
      //     val fmt = formatting.formatObject(pdfobj, reader)
      //     write.append(objf, fmt.toString())
      //     write.append(objf, "\n\n")
      //   }

      //   val trailer = formatting.formatDictionary(reader.getTrailer, reader)
      //   val catalog = formatting.formatDictionary(reader.getCatalog, reader)
      //   write.append(tf, trailer.toString())
      //   write.append(cf, catalog.toString())

      //   formatting.followIndirect = true
      // }


      for (pageNumber <- 1 to document.getNumberOfPages) {
        // val tagStructureContext = pdfPage.getDocument.getTagStructureContext





        val pageId = PageID(pageNumber-1)

        val pdfPage = document.getPage(pageNumber)
        val pageSize = pdfPage.getPageSize

        // val structTreeRoot = pdfPage.getDocument.getStructTreeRoot
        // println("struct root role")
        // println(structTreeRoot.getRole)
        // println("struct root role map")
        // println(formatting.formatObject(structTreeRoot.getRoleMap, reader: PdfReader))
        // val kids = structTreeRoot.getKids
        // val mcrs = structTreeRoot.getPageMarkedContentReferences(pdfPage)

        // kids.foreach({k =>
        //   println(renderElemTree(k))
        // })

        // mcrs.foreach({ mcr =>

        //   def fmtmcr(e: PdfMcr) = {
        //     s"""| findMcrByMcid ${mcr.getMcid}
        //         |   getKids       = ${e.getKids        }
        //         |   getMcid       = ${e.getMcid        }
        //         |   getParent     = ${e.getParent      }
        //         |   getRole       = ${e.getRole        }
        //         |   getPdfObject  = ${e.getPdfObject   }
        //         |   isFlushed     = ${e.isFlushed      }
        //         |""".stripMargin
        //   }
        //   println(fmtmcr(mcr))
        // })



        val resources = pdfPage.getResources


        {
          // import ammonite.ops._
          // val of = cwd / s"pdf-resources.txt"
          // if (pageNumber == 1) {
          //   rm(of)
          // }

          // val op = formatting.formatDictionary(resources.getPdfObject, reader)

          // write.append(of, s"PAGE ${pageNumber}\n\n")
          // write.append(of, op.toString())
          // write.append(of, s"\n\nEND PAGE ${pageNumber}\n")
        }

        val currCharBuffer: mutable.ArrayBuffer[PageAtom] = mutable.ArrayBuffer[PageAtom]()

        val extractor = new CharExtractionListener(
          reader, charsToDebug,
          componentIdGen, currCharBuffer, pdfPage, pageId
        )

        val parser = new PdfCanvasProcessor(extractor);
        parser.processPageContent(pdfPage)

        val pageGeometry = getReportedPageGeometry(pageId, pdfPage)

        // WTF?
        pdfPage.getPageSize
        pdfPage.getPageSizeWithRotation
        pdfPage.getArtBox
        pdfPage.getTrimBox
        pdfPage.getCropBox
        pdfPage.getMediaBox

        // val autoTagging = tagStructureContext.getAutoTaggingPointer


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
