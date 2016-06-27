package edu.umass.cs.iesl.watr
package extract

import java.io.{InputStream, IOException}

import scala.collection.JavaConversions._
import scala.collection.mutable

import com.itextpdf.text.Rectangle
import com.itextpdf.text.exceptions.InvalidPdfException
import com.itextpdf.text.pdf._
import com.itextpdf.text.pdf.parser.{Vector => PVector, RenderListener, _}
import util._
import utils._
// import watrmarks._
import spindex._
import TypeTags._

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

  // def formatBounds(bounds: BxBounds): String = {
  //   val x = bounds.getX
  //   val y = bounds.getY
  //   val w = bounds.getWidth
  //   val h = bounds.getHeight
  //   def fmt = (d: Double) => f"${d}%1.2f"
  //   s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
  // }

  // def listBounds(bounds: BxBounds): List[Double] = {
  //   val x = bounds.getX
  //   val y = bounds.getY
  //   val w = bounds.getWidth
  //   val h = bounds.getHeight
  //   // List(fmt(x), fmt(y), fmt(w), fmt(h))
  //   List(x, y, w, h)
  // }
}


import UnicodeUtil._

class MyBxDocumentCreator(
  fontDict: mutable.Map[String, DocumentFont],
  reader: PdfReader,
  charsToDebug: Set[Int] = Set(),
  componentIdGen: IdGenerator[RegionID]
) extends RenderListener {

  var currCharBuffer: mutable.ArrayBuffer[PageAtom] = mutable.ArrayBuffer[PageAtom]()

  var pageRectangle: Rectangle = _
  var pageId: Int@@PageID = _


  def processNewBxPage(_pageId: Int@@PageID, _pageRectangle: Rectangle): Unit = {
    pageId = _pageId
    pageRectangle = _pageRectangle
  }

  override def beginTextBlock(): Unit = {
    // println("\nblock\n")
  }


  def computeTextBounds(charTri: TextRenderInfo): Option[LTBounds] = {
    val ascentStart = charTri.getAscentLine().getStartPoint()
    val descentStart = charTri.getDescentLine().getStartPoint()

    val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble
    val absoluteCharBottom: Double = descentStart.get(PVector.I2).toDouble

    val charLeft = absoluteCharLeft - pageRectangle.getLeft()
    val charBottom = absoluteCharBottom - pageRectangle.getBottom() // in math coords


    var charHeight = ascentStart.get(PVector.I2).toDouble - descentStart.get(PVector.I2)
    var charWidth = charTri.getDescentLine().getLength().toDouble

    if (charHeight.nan || charHeight.inf) {
      println(s"warning: char height is NaN or Inf")
      charHeight = 0
    }

    if (charWidth.nan || (charWidth.inf)) {
      println(s"warning: char width is NaN or Inf")
      charWidth = 0
    }

    if (absoluteCharLeft < pageRectangle.getLeft()
      || absoluteCharLeft + charWidth > pageRectangle.getRight()
      || absoluteCharBottom < pageRectangle.getBottom()
      || absoluteCharBottom + charHeight > pageRectangle.getTop()) {
      None
    } else {
      // if (bounds.getX().nan || bounds.getX().inf
      //   || bounds.getY().nan || bounds.getY().inf
      //   || bounds.getHeight().nan || bounds.getHeight().inf
      //   || bounds.getWidth().nan || bounds.getWidth().inf) {
      //   // skip
      //   println(s"skipping text w/bbox= nan|inf: ${text}")
      // } else {

      val y = pageRectangle.getHeight() - charBottom - charHeight

      Some(LTBounds(
        left=charLeft,
        top=y,
        width=charWidth,
        height=charHeight
      ))
    }
  }

  def transformRawChar(ch: Char): String = {
    if (ch <= ' ') ""
    else ch.toString
  }

  def outputCharDebugInfo():Unit = {
    // if(charsToDebug contains charIndex) {
    //   println(s"""Char bounds ${charBounds.prettyPrint}""")
    // }
    // if (!charsToDebug.isEmpty) {
    //   if(charsToDebug contains charIndex) {
    //     println(s"Outputting char info #${charIndex}")
    //     println(s"  text = ${text}")
    //     DocumentFontInfo.outputCharInfo(charTri, reader)
    //     DocumentFontInfo.reportFontInfo(charTri.getFont)
    //     println(s"-------------------------------------\n\n")
    //   } else {
    //     if (charsToDebug.min - 10 < charIndex && charIndex < charsToDebug.max + 10) {
    //       println(s" renderText(${text} ${charIndex})")
    //     }
    //   }
    // }
  }

  override def renderText(trix: TextRenderInfo): Unit = {
    for {
      charTri <- trix.getCharacterRenderInfos()
      rawChar = charTri.getText().charAt(0)
      subChars = maybeSubChar(rawChar)
      bakedChar = transformRawChar(rawChar)
      charBounds = computeTextBounds(charTri)
    } {

      val wonkyChar = if (rawChar.toString != bakedChar.toString) Some(rawChar.toInt) else None
      // skip spaces
      if (!wonkyChar.exists(_ == 32)) {

        val charBox = charBounds.map(bnds =>
          CharAtom(
            TargetRegion(
              componentIdGen.nextId,
              pageId,
              bnds
            ),
            bakedChar,
            subChars.map(_.mkString).getOrElse(""),
            wonkyCharCode = wonkyChar
          )
        ).getOrElse ({
          val msg = s"ERROR bounds are invalid"
          sys.error(msg)
        })


        // if (wonkyChar.isDefined || subChars.isDefined) {
        //   println(s"char: ${charBox}")
        // }
        DocumentFontInfo.outputCharInfo(charTri, reader)
        // DocumentFontInfo.reportFontInfo(charTri.getFont)
        // DocumentFontInfo.addFontInfo(charTri.getFont
        // val fullFontName = charTri.getFont().getFullFontName()(0)(3)
        // chunk.setFontName(fullFontName)

        currCharBuffer.append(charBox)
      }
    }
  }


  override def endTextBlock(): Unit = {
  }

  override def renderImage(iri: ImageRenderInfo): Unit = {
    // TODO figure out why this isn't working (img type not supported...)
    // val img = iri.getImage
    // val bimg = img.getBufferedImage

    // val x = bimg.getMinX.toDouble
    // val y = bimg.getMinY.toDouble
    // val w = bimg.getWidth.toDouble
    // val h = bimg.getHeight.toDouble

    // val bounds = LTBounds(
    //   x - pageRectangle.getLeft,
    //   pageRectangle.getHeight - y - pageRectangle.getBottom - h,
    //   w, h
    // )

    // val imgRegion = ImgAtom(
    //     TargetRegion(
    //       componentIdGen.nextId,
    //       PageID(0),
    //       bounds
    //     )
    //   )

    // currCharBuffer.append(imgRegion)
  }

}


class XITextCharacterExtractor(
  charsToDebug: Set[Int] = Set(),
  componentIdGen: IdGenerator[RegionID]
) {
  val DEFAULT_FRONT_PAGES_LIMIT = 20
  val DEFAULT_BACK_PAGES_LIMIT = 20
  val frontPagesLimit = DEFAULT_FRONT_PAGES_LIMIT
  val backPagesLimit = DEFAULT_BACK_PAGES_LIMIT

  val fontDict = mutable.HashMap[String, DocumentFont]()

  var bxDocumentCreator:MyBxDocumentCreator = _

  // def getZoneRecords() = bxDocumentCreator.getZoneRecords

  def getReportedPageGeometry(pageId: Int@@PageID, pageRectangle: Rectangle): PageGeometry = {
    val borders = if (pageRectangle.hasBorders) {
      Some(Borders(
        bleft = pageRectangle.getBorderWidthLeft.toDouble,
        bbottom = pageRectangle.getBorderWidthBottom.toDouble,
        btop = pageRectangle.getBorderWidthTop.toDouble,
        bright = pageRectangle.getBorderWidthRight.toDouble
      ))
    } else None

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
    val bounds = LTBounds(
      left = pageRectangle.getLeft.toDouble,
      top = pageRectangle.getBottom.toDouble, // ???
      width = pageRectangle.getWidth.toDouble,
      height = pageRectangle.getHeight.toDouble
    )

    PageGeometry(pageId, bounds, borders)

  }


  var pagesInfo: List[(PageAtoms, PageGeometry)] = List()

  def extractCharacters(stream: InputStream): Unit = {
    try {
      val reader = new PdfReader(stream)

      val documentCreator = new MyBxDocumentCreator(
        fontDict, reader, charsToDebug,
        componentIdGen
      )

      val processor = new PdfContentStreamProcessor(documentCreator)

      {
        formatting.followIndirect = false

        import ammonite.ops._
        val tf = cwd / s"pdf-trailer.txt"
        val cf = cwd / s"pdf-catalog.txt"
        rm(tf)
        rm(cf)

        val nn1 = reader.getNamedDestinationFromNames
        val nn2 = reader.getNamedDestinationFromStrings
        println("getNamedDestinationFromNames")
        println(nn1.keys.mkString(", "))
        println("getNamedDestinationFromStrings")
        println(nn2.keys.mkString(", "))

        val objf = cwd / s"pdf-name-objs.txt"
        rm(objf)

        nn2.foreach { case (name, pdfobj) =>
          val fmt = formatting.formatObject(pdfobj, reader)
          write.append(objf, fmt.toString())
          write.append(objf, "\n\n")
        }

        val trailer = formatting.formatDictionary(reader.getTrailer, reader)
        val catalog = formatting.formatDictionary(reader.getCatalog, reader)
        write.append(tf, trailer.toString())
        write.append(cf, catalog.toString())

        formatting.followIndirect = true
      }

      for (pageNumber <- 1 to reader.getNumberOfPages) {

        val pageId = PageID(pageNumber-1)

        val pageSize = reader.getPageSize(pageNumber)

        documentCreator.processNewBxPage(pageId, pageSize)


        val pageResources = reader.getPageResources(pageNumber)

        val resources = reader.getPageN(pageNumber).getAsDict(PdfName.RESOURCES)


        {
          import ammonite.ops._
          val of = cwd / s"pdf-resources.txt"
          if (pageNumber == 1) {
            rm(of)
          }

          val op = formatting.formatDictionary(resources, reader)

          write.append(of, s"PAGE ${pageNumber}\n\n")
          write.append(of, op.toString())
          write.append(of, s"\n\nEND PAGE ${pageNumber}\n")
        }

        processAlternativeFontNames(resources)
        processAlternativeColorSpace(resources)

        processor.reset()
        processor.processContent(ContentByteUtils.getContentBytesForPage(reader, pageNumber), resources)

        val pageGeometry = getReportedPageGeometry(pageId, pageSize)

        val pageCharAtoms = Seq[PageAtom](documentCreator.currCharBuffer:_*)

        val pageChars = PageAtoms(pageId, pageCharAtoms)

        pagesInfo = pagesInfo :+ (
          (pageChars, pageGeometry)
        )

        documentCreator.currCharBuffer.clear()


      }

      // TODO these steps (filter/remove dups) don't seem necessary yet, and if they are,
      //   can probably be better handled using spatial index
      // filterComponents(removeDuplicateChunks(documentCreator.document))
      bxDocumentCreator = documentCreator

      // documentCreator.document
      /// Dummy return value

    } catch {
      case ex: InvalidPdfException =>
        throw new Exception("Invalid PDF file", ex)
      case ex: IOException =>
        throw new Exception("Cannot extract characters from PDF file", ex)
    }
  }

  val ALT_TO_STANDART_FONTS = Map[String, PdfName](
    "CourierNew" -> PdfName.COURIER,
    "CourierNew,Bold" -> PdfName.COURIER_BOLD,
    "CourierNew,BoldItalic" -> PdfName.COURIER_BOLDOBLIQUE,
    "CourierNew,Italic" -> PdfName.COURIER_OBLIQUE,
    "Arial" -> PdfName.HELVETICA,
    "Arial,Bold" -> PdfName.HELVETICA_BOLD,
    "Arial,BoldItalic" -> PdfName.HELVETICA_BOLDOBLIQUE,
    "Arial,Italic" -> PdfName.HELVETICA_OBLIQUE,
    "TimesNewRoman" -> PdfName.TIMES_ROMAN,
    "TimesNewRoman,Bold" -> PdfName.TIMES_BOLD,
    "TimesNewRoman,BoldItalic" -> PdfName.TIMES_BOLDITALIC,
    "TimesNewRoman,Italic" -> PdfName.TIMES_ITALIC
  )

  def processAlternativeFontNames(resources: PdfDictionary): Unit = {
    val fontsDictionary = resources.getAsDict(PdfName.FONT)

    if (fontsDictionary == null) {
      return
    }
    for (pdfFontName <- fontsDictionary.getKeys()) {
      if (!(fontsDictionary.get(pdfFontName).isInstanceOf[PRIndirectReference])) {
        return
      } else {
        val indRef = fontsDictionary.get(pdfFontName).asInstanceOf[PRIndirectReference]
        val fontDictionary = PdfReader.getPdfObjectRelease(indRef).asInstanceOf[PdfDictionary]

        val baseFont = fontDictionary.getAsName(PdfName.BASEFONT)
        if (baseFont != null) {
          val fontName = PdfName.decodeName(baseFont.toString())
          if (fontDictionary.getAsArray(PdfName.WIDTHS) == null && ALT_TO_STANDART_FONTS.containsKey(fontName)) {
            fontDictionary.put(PdfName.BASEFONT, ALT_TO_STANDART_FONTS(fontName))
          }
        }
      }
    }
  }

  def processAlternativeColorSpace(resources: PdfDictionary): Unit = {
    val csDictionary = resources.getAsDict(PdfName.COLORSPACE)
    if (csDictionary == null) {
      return
    }
    for (csName <- csDictionary.getKeys()) {
      if (csDictionary.getAsArray(csName) != null) {
        csDictionary.put(csName, PdfName.DEVICEGRAY)
      }
    }
  }

}
