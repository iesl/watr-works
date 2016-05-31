package edu.umass.cs.iesl
package watr
package extract

import java.io.{InputStream, IOException}

import scala.collection.JavaConversions._
import scala.collection.mutable

import com.itextpdf.text.Rectangle
import com.itextpdf.text.exceptions.InvalidPdfException
import com.itextpdf.text.pdf._
import com.itextpdf.text.pdf.parser.{Vector => PVector, RenderListener, _}
import _root_.pl.edu.icm.cermine
import cermine.structure.model._
import cermine.structure.model.BxBounds
import cermine.exception.AnalysisException
import cermine.structure.CharacterExtractor
import cermine.structure.model._
import cermine.structure.tools.BxBoundsBuilder
import util._
import watrmarks._

object util {

  implicit class RicherFloat(val d: Float) extends AnyVal {
    def nan = java.lang.Float.isNaN(d)
    def inf = java.lang.Float.isInfinite(d)
  }
  implicit class RicherDouble(val d: Double) extends AnyVal {
    def nan = java.lang.Double.isNaN(d)
    def inf = java.lang.Double.isInfinite(d)
  }

  def formatBounds(bounds: BxBounds): String = {
    val x = bounds.getX
    val y = bounds.getY
    val w = bounds.getWidth
    val h = bounds.getHeight
    def fmt = (d: Double) => f"${d}%1.2f"
    s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
  }

  def listBounds(bounds: BxBounds): List[Double] = {
    val x = bounds.getX
    val y = bounds.getY
    val w = bounds.getWidth
    val h = bounds.getHeight
    // List(fmt(x), fmt(y), fmt(w), fmt(h))
    List(x, y, w, h)
  }
}


import UnicodeUtil._

class MyBxDocumentCreator(
  fontDict: mutable.Map[String, DocumentFont],
  reader: PdfReader,
  charsToDebug: Set[Int] = Set(),
  charIdGen: IdGenerator[CharID],
  pageIdGen: IdGenerator[PageID]
) extends RenderListener {


  var currCharBuffer: mutable.ArrayBuffer[CharBox] = mutable.ArrayBuffer[CharBox]()
  var currPageChars = PageChars(pageIdGen.nextId, currCharBuffer)

  var zoneRecords: ZoneRecords = ZoneRecords(
    id = "", target = "",
    List[PageGeometry](),
    List[Zone](),
    List[ZoneAndLabel](),
    List[PageChars]()
  )


  def getZoneRecords() = zoneRecords

  val document = new BxDocument()
  var actPage: BxPage = _

  val boundsBuilder = new BxBoundsBuilder()

  var pageRectangle: Rectangle = _

  var pageNumber = -1



  def processNewBxPage(_pageRectangle: Rectangle): Unit = {

    if (actPage != null) {
      actPage.setBounds(boundsBuilder.getBounds())
      boundsBuilder.clear()
    }
    actPage = new BxPage()
    document.addPage(actPage)
    pageNumber += 1


    pageRectangle = _pageRectangle

    val borders = if (pageRectangle.hasBorders) {
      Some(watrmarks.Borders(
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
    val bounds = watrmarks.LTBounds(
      left = pageRectangle.getLeft.toDouble,
      top = pageRectangle.getBottom.toDouble, // ???
      width = pageRectangle.getWidth.toDouble,
      height = pageRectangle.getHeight.toDouble
    )

    zoneRecords = zoneRecords.copy(
      pageGeometries = zoneRecords.pageGeometries :+ PageGeometry(PageID(pageNumber), bounds, borders),
      chars = zoneRecords.chars :+ currPageChars
    )

    currCharBuffer = mutable.ArrayBuffer[CharBox]()
    currPageChars = PageChars(pageIdGen.nextId, currCharBuffer)
  }

  override def beginTextBlock(): Unit = {
    // println("\nblock\n")
  }

  // var count = 0

  def computeTextBounds(charTri: TextRenderInfo): Option[LBBounds] = {
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

      Some(LBBounds(
        left=charLeft,
        bottom=y,
        width=charWidth,
        height=charHeight
      ))
    }
  }

  def transformRawChar(ch: Char): String = {
    if (ch == ' ') ""
    else if (ch < ' ') ""
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
    //     CermineFontInfo.outputCharInfo(charTri, reader)
    //     CermineFontInfo.reportFontInfo(charTri.getFont)
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
      charBounds <- computeTextBounds(charTri)
    } {

      val wonkyChar = if (rawChar.toString != bakedChar.toString) Some(rawChar.toInt) else None


      val charBox = CharBox(
        charIdGen.nextId,
        bakedChar,
        charBounds.toLTBounds,
        subChars.map(_.mkString).getOrElse(""),
        wonkyCharCode = wonkyChar
      )

      if (wonkyChar.isDefined || subChars.isDefined) {
        println(s"char: ${charBox}")
      }
      // CermineFontInfo.outputCharInfo(charTri, reader)
      // CermineFontInfo.reportFontInfo(charTri.getFont)

      currCharBuffer.append(charBox)

      // val chunk = new BxChunk(bounds, text)
      // val fullFontName = charTri.getFont().getFullFontName()(0)(3)
      // chunk.setFontName(fullFontName)
      // actPage.addChunk(chunk)
      // boundsBuilder.expand(bounds)

      // CermineFontInfo.addFontInfo(charTri.getFont
    }

  }


  override def endTextBlock(): Unit = {
  }

  override def renderImage(iri: ImageRenderInfo): Unit = {
    // val img = iri.getImage
  }




}


class XITextCharacterExtractor(
  charsToDebug: Set[Int] = Set(),
  charIdGen: IdGenerator[CharID],
  pageIdGen: IdGenerator[PageID]
) extends CharacterExtractor {
  val DEFAULT_FRONT_PAGES_LIMIT = 20
  val DEFAULT_BACK_PAGES_LIMIT = 20
  val frontPagesLimit = DEFAULT_FRONT_PAGES_LIMIT
  val backPagesLimit = DEFAULT_BACK_PAGES_LIMIT

  val fontDict = mutable.HashMap[String, DocumentFont]()

  var bxDocumentCreator:MyBxDocumentCreator = _


  def getZoneRecords() = bxDocumentCreator.getZoneRecords

  override def extractCharacters(stream: InputStream): BxDocument = {
    try {
      val reader = new PdfReader(stream)

      val documentCreator = new MyBxDocumentCreator(
        fontDict, reader, charsToDebug,
        charIdGen, pageIdGen
      )

      val processor = new PdfContentStreamProcessor(documentCreator)


      for (pageNumber <- 1 to reader.getNumberOfPages) {

        if (frontPagesLimit > 0 && backPagesLimit > 0 && pageNumber > frontPagesLimit
          && pageNumber < reader.getNumberOfPages() - 1 - backPagesLimit) {
          // continue
        } else {

          val pageSize = reader.getPageSize(pageNumber)

          documentCreator.processNewBxPage(reader.getPageSize(pageNumber))

          val pageResources = reader.getPageResources(pageNumber)

          val resources = reader.getPageN(pageNumber).getAsDict(PdfName.RESOURCES)

          processAlternativeFontNames(resources)
          processAlternativeColorSpace(resources)

          processor.reset()
          processor.processContent(ContentByteUtils.getContentBytesForPage(reader, pageNumber), resources)
        }
      }

      // TODO these steps (filter/remove dups) don't seem necessary yet, and if they are,
      //   can probably be better handled using spatial index
      // filterComponents(removeDuplicateChunks(documentCreator.document))
      bxDocumentCreator = documentCreator

      documentCreator.document

    } catch {
      case ex: InvalidPdfException =>
        throw new AnalysisException("Invalid PDF file", ex)
      case ex: IOException =>
        throw new AnalysisException("Cannot extract characters from PDF file", ex)
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

  import scala.collection.mutable

  def removeDuplicateChunks(document: BxDocument): BxDocument = {
    for (page <- document) {
      val chunks = mutable.ArrayBuffer[BxChunk]()
      val filteredChunks = mutable.ArrayBuffer[BxChunk]()

      def intToChunksMap = mutable.HashMap[Integer, mutable.Set[BxChunk]]()
      def chunkSet = mutable.Set[BxChunk]()

      val chunkMap = mutable.HashMap[Integer, mutable.HashMap[Integer, mutable.Set[BxChunk]]]()

      for (chunk <- chunks) {
        val x = chunk.getX().toInt
        val y = chunk.getY().toInt
        var duplicate = false

        // duplicateSearch:
        try {
          for (
            i <- x - 1 to x + 1;
            j <- y - 1 to y + 1
          ) {
            if (chunkMap.contains(i) && chunkMap(i).contains(j)) {

              for (ch <- chunkMap(i)(j)) {
                if (chunk.toText().equals(ch.toText()) && chunk.getBounds().isSimilarTo(ch.getBounds(), 1)) {
                  duplicate = true
                  // break duplicateSearch
                  println(s"duplicate: ${chunk.toText()}")
                  throw new Throwable()
                }
              }
            }
          }

        } catch {
          case break: Throwable =>
        }

        if (!duplicate) {
          filteredChunks.add(chunk)
          val x = chunk.getX().toInt
          val y = chunk.getY().toInt

          if (!chunkMap.contains(x)) {
            chunkMap.put(x, intToChunksMap)
          }
          if (!chunkMap(x).contains(y)) {
            chunkMap(x).put(y, chunkSet)
          }
          chunkMap(x)(y).add(chunk)
        }
      }
      page.setChunks(filteredChunks)
    }
    document
  }

  val CHUNK_DENSITY_LIMIT = 15
  val PAGE_GRID_SIZE = 10

  def filterComponents(document: BxDocument): BxDocument = {
    for (page <- document) {
      val bounds = new BxBoundsBuilder()
      // val chunks = Lists.newArrayList(page.getChunks())
      // val chunks = page.getChunks
      val chunks = mutable.ArrayBuffer(page.getChunks.toList: _*)
      for (ch <- chunks) {
        bounds.expand(ch.getBounds())
      }

      val density = 100.0 * chunks.size / (bounds.getBounds().getWidth() * bounds.getBounds().getHeight())
      if (density.nan || density < CHUNK_DENSITY_LIMIT) {
        //continue
      } else {

        val map = mutable.HashMap[String, mutable.ArrayBuffer[BxChunk]]()
        for (ch <- chunks) {
          val x = (ch.getX() / PAGE_GRID_SIZE).toInt
          val y = (ch.getY() / PAGE_GRID_SIZE).toInt
          val key = Integer.toString(x) + " " + Integer.toString(y)
          if (map.contains(key) == null) {
            map.put(key, mutable.ArrayBuffer[BxChunk]())
          }
          map(key).add(ch)
        }

        for (list <- map.values()) {
          if (list.size() > CHUNK_DENSITY_LIMIT) {
            for (ch <- list) {
              chunks.remove(ch)
            }
          }
        }
        page.setChunks(chunks)
      }
    }
    document
  }

}

  // override def renderText(trix: TextRenderInfo): Unit = {
  //   for (charTri <- trix.getCharacterRenderInfos()) {
  //     val text = charTri.getText()

  //     val ch = charTri.getText().charAt(0)
  //     maybeSubChar(ch)

  //     if (ch <= ' '
  //       || text.matches("^[\uD800-\uD8FF]$")
  //       || text.matches("^[\uDC00-\uDFFF]$")
  //       || text.matches("^[\uFFF0-\uFFFF]$")) {
  //       // no-op
  //       val displayable = text.getBytes.map({b => b.toInt}).mkString(", ")
  //       // println(s"skipping character(s) w/bytes= [${displayable}]")
  //     } else {
  //       val ascentStart = charTri.getAscentLine().getStartPoint()
  //       val descentStart = charTri.getDescentLine().getStartPoint()

  //       val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble
  //       val absoluteCharBottom: Double = descentStart.get(PVector.I2).toDouble

  //       val charLeft = absoluteCharLeft - pageRectangle.getLeft()
  //       val charBottom = absoluteCharBottom - pageRectangle.getBottom() // in math coords


  //       var charHeight = ascentStart.get(PVector.I2).toDouble - descentStart.get(PVector.I2)
  //       var charWidth = charTri.getDescentLine().getLength().toDouble

  //       if (charHeight.nan || charHeight.inf) {
  //         println(s"warning: char height is NaN or Inf")
  //         charHeight = 0
  //       }

  //       if (charWidth.nan || (charWidth.inf)) {
  //         println(s"warning: char width is NaN or Inf")
  //         charWidth = 0
  //       }

  //       if (absoluteCharLeft < pageRectangle.getLeft()
  //         || absoluteCharLeft + charWidth > pageRectangle.getRight()
  //         || absoluteCharBottom < pageRectangle.getBottom()
  //         || absoluteCharBottom + charHeight > pageRectangle.getTop()) {
  //         // no-op
  //         println(s"skipping text w/bbox out of page bounds: ${text}")
  //       } else {
  //         page0Chars += 1

  //         val x = charLeft
  //         val y = pageRectangle.getHeight() - charBottom - charHeight

  //         val bounds = new BxBounds(
  //           x, y, charWidth, charHeight
  //         )

  //         if(charsToDebug contains charIndex) {
  //           println(
  //             s"""|Char bounds
  //                 | x      ${x}
  //                 | y      ${y}
  //                 | width  ${charWidth}
  //                 | height ${charHeight}
  //                 |""".stripMargin
  //           )
  //         }

  //         if (bounds.getX().nan || bounds.getX().inf
  //           || bounds.getY().nan || bounds.getY().inf
  //           || bounds.getHeight().nan || bounds.getHeight().inf
  //           || bounds.getWidth().nan || bounds.getWidth().inf) {
  //           // skip
  //           println(s"skipping text w/bbox= nan|inf: ${text}")
  //         } else {
  //           charIndex = charIndex+1

  //           if (!charsToDebug.isEmpty) {
  //             if(charsToDebug contains charIndex) {
  //               println(s"Outputting char info #${charIndex}")
  //               println(s"  text = ${text}")
  //               CermineFontInfo.outputCharInfo(charTri, reader)
  //               CermineFontInfo.reportFontInfo(charTri.getFont)
  //               println(s"-------------------------------------\n\n")
  //             } else {
  //               if (charsToDebug.min - 10 < charIndex && charIndex < charsToDebug.max + 10) {
  //                 println(s" renderText(${text} ${charIndex})")
  //               }
  //             }
  //           }
  //           val chunk = new BxChunk(bounds, text)
  //           val fullFontName = charTri.getFont().getFullFontName()(0)(3)
  //           chunk.setFontName(fullFontName)
  //           actPage.addChunk(chunk)
  //           boundsBuilder.expand(bounds)

  //           addFontInfo(charTri.getFont)
  //         }
  //       }
  //     }
  //   }
  // }
