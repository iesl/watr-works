package edu.umass.cs.iesl.watr
package ext

// import com.google.common.collect.Lists
import com.itextpdf.text.Rectangle
import com.itextpdf.text.pdf.parser.ImageRenderInfo
import com.itextpdf.text.pdf.parser.TextRenderInfo
import java.io.InputStream
import com.itextpdf.text.exceptions.InvalidPdfException
import com.itextpdf.text.pdf.PRIndirectReference
import com.itextpdf.text.pdf.PdfDictionary
import com.itextpdf.text.pdf.PdfName
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.DocumentFont
import com.itextpdf.text.pdf.parser._
import java.io.IOException
import java.io.InputStream
import pl.edu.icm.cermine.exception.AnalysisException
import pl.edu.icm.cermine.structure.model._
import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
import pl.edu.icm.cermine.structure.CharacterExtractor

import scala.collection.JavaConversions._
import com.itextpdf.text.pdf.parser.{Vector => PVector}

import _root_.pl.edu.icm.cermine.structure.ITextCharacterExtractor


import com.itextpdf.text.pdf.parser.RenderListener

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

import util._
import spatialindex.{SpatialIndex, regions}
import scala.collection.mutable

case class SpatialPageInfo(
  sindex: SpatialIndex = SpatialIndex.create(),
  charRegions: mutable.Map[Long, Char] = mutable.Map(),
  pageBounds: watrmarks.TextBounds
) {
  var _nextId = 0L
  def nextId = {
    _nextId += 1
    _nextId
  }

  val labels = mutable.Map[Long, String]()

  def insert(ch: Char, x: Double, y: Double, w: Double, h: Double): Unit = {
    val nid = nextId
    sindex.insert(nid, regions.bbox(x=x , y=y, w=w, h=h))
    charRegions(nid) = ch
  }

  def labelBbox(bbox: spatialindex.Bounds, label: String): Unit = {
    val nid = nextId
    val data = sindex.insert(nid, bbox)
    val _ = labels.put(nid, label)
  }

}

class MyBxDocumentCreator(
  spatialPageInfo: mutable.ArrayBuffer[SpatialPageInfo],
  fontDict: mutable.Map[String, DocumentFont]
) extends RenderListener {



  // val spatialIndex = SpatialIndex.create()
  // val charRegions = mutable.Map[Long, Char]()

  val document = new BxDocument()
  var actPage: BxPage = _

  val boundsBuilder = new BxBoundsBuilder()

  var pageRectangle: Rectangle = _

  def processNewBxPage(_pageRectangle: Rectangle): Unit = {
    if (actPage != null) {
      actPage.setBounds(boundsBuilder.getBounds())
      boundsBuilder.clear()
    }
    actPage = new BxPage()
    document.addPage(actPage)

    pageRectangle = _pageRectangle
    spatialPageInfo.append(SpatialPageInfo(
      pageBounds = watrmarks.TextBounds(
        left = pageRectangle.getLeft,
        bottom = pageRectangle.getBottom,
        width = pageRectangle.getWidth,
        height = pageRectangle.getHeight
      )))

  }

  override def beginTextBlock(): Unit = {
    // println("\nblock\n")
  }

  def formatBounds(bounds: BxBounds): String = {
    val x = bounds.getX
    val y = bounds.getY
    val w = bounds.getWidth
    val h = bounds.getHeight

    def fmt = (d: Double) => f"${d}%1.2f"

    s"""(${fmt(x)}, ${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
  }

  def outputCharInfo(tri: TextRenderInfo): Unit = {
    val font = tri.getFont()
    // val text2 = font.getUnicodeEquivalent(tri.getMcid)
    val pdfstring = tri.getPdfString

    val pdfstrInf = s"""|
                        | getBytes         ${pdfstring.getBytes            } => Array[Byte]
                        | getEncoding      ${pdfstring.getEncoding         } => String
                        | getOriginalBytes ${pdfstring.getOriginalBytes    } => Array[Byte]
                        | isHexWriting     ${pdfstring.isHexWriting        } => Boolean
                        | toString         ${pdfstring.toString            } => String
                        | toUnicodeString  ${pdfstring.toUnicodeString     } => String
                        | canBeInObjStm    ${pdfstring.canBeInObjStm       } => Boolean
                        | getIndRef        ${pdfstring.getIndRef           } => PRIndirectReference
                        | isArray          ${pdfstring.isArray             } => Boolean
                        | isBoolean        ${pdfstring.isBoolean           } => Boolean
                        | isDictionary     ${pdfstring.isDictionary        } => Boolean
                        | isIndirect       ${pdfstring.isIndirect          } => Boolean
                        | isName           ${pdfstring.isName              } => Boolean
                        | isNull           ${pdfstring.isNull              } => Boolean
                        | isNumber         ${pdfstring.isNumber            } => Boolean
                        | isStream         ${pdfstring.isStream            } => Boolean
                        | isString         ${pdfstring.isString            } => Boolean
                        | length           ${pdfstring.length              } => Int
                        | type             ${pdfstring.`type`              } => Int
                        |
                        |""".stripMargin

    val d0 = font.getDifferences
    val d1 = font.getUnicodeDifferences

    val dictKvs = font.getFontDictionary.getKeys.map{ key =>
      key.toString() + ": " + font.getFontDictionary.get(key)
    }.mkString("\n")



    val asdf = tri.getAscentLine
    val inf = s"""|
                  | Text              ${tri.getText             }
                  |
                  | diffs             ${d0.length}
                  | unidiffs          ${d1.length}
                  |
                  | Font              ${tri.getFont             }              => DocumentFont
                  | Mcid              ${tri.getMcid             }              => Integer
                  | PdfString         ${tri.getPdfString        }              => PdfString
                  | Rise              ${tri.getRise             }              => Float
                  | TextRenderMode    ${tri.getTextRenderMode   }              => Int
                  |
                  | ${pdfstrInf}
                  |
                  | ${dictKvs}
                  |""".stripMargin
    println(inf)

      // | UnscaledBaseline  ${tri.getUnscaledBaseline }              => LineSegment
      // | SingleSpaceWidth  ${tri.getSingleSpaceWidth }              => Float
      // | StrokeColor       ${tri.getStrokeColor      }              => BaseColor
      // | AscentLine        ${tri.getAscentLine       }              => LineSegment
      // | Baseline          ${tri.getBaseline         }              => LineSegment
      // | DescentLine       ${tri.getDescentLine      }              => LineSegment
      // | FillColor         ${tri.getFillColor        }              => BaseColor
  }


  override def renderText(tri: TextRenderInfo): Unit = {
    for (charTri <- tri.getCharacterRenderInfos()) {
      val text = charTri.getText()
      outputCharInfo(charTri)
      // print(s"${text}")


      val ch = charTri.getText().charAt(0)
      if (ch <= ' '
        || text.matches("^[\uD800-\uD8FF]$")
        || text.matches("^[\uDC00-\uDFFF]$")
        || text.matches("^[\uFFF0-\uFFFF]$")) {
      } else {

        val absoluteCharLeft: Double = charTri.getDescentLine().getStartPoint().get(PVector.I1).toDouble
        val absoluteCharBottom: Double = charTri.getDescentLine().getStartPoint().get(PVector.I2).toDouble

        val charLeft = absoluteCharLeft - pageRectangle.getLeft()
        val charBottom = absoluteCharBottom - pageRectangle.getBottom()

        var charHeight = charTri.getAscentLine().getStartPoint().get(PVector.I2).toDouble - charTri.getDescentLine().getStartPoint().get(PVector.I2)
        var charWidth = charTri.getDescentLine().getLength().toDouble

        if (charHeight.nan || charHeight.inf) {
          charHeight = 0
        }

        if (charWidth.nan || (charWidth.inf)) {
          charWidth = 0
        }

        if (absoluteCharLeft < pageRectangle.getLeft()
          || absoluteCharLeft + charWidth > pageRectangle.getRight()
          || absoluteCharBottom < pageRectangle.getBottom()
          || absoluteCharBottom + charHeight > pageRectangle.getTop()) {
        } else {

          val x = charLeft
          val y = pageRectangle.getHeight() - charBottom - charHeight

          val bounds = new BxBounds(
            x, y, charWidth, charHeight
          )


          if (bounds.getX().nan || bounds.getX().inf
            || bounds.getY().nan || bounds.getY().inf
            || bounds.getHeight().nan || bounds.getHeight().inf
            || bounds.getWidth().nan || bounds.getWidth().inf) {
            // continue...
          } else {
            val chunk = new BxChunk(bounds, text)
            val fullFontName = tri.getFont().getFullFontName()(0)(3)
            chunk.setFontName(fullFontName)
            actPage.addChunk(chunk)
            boundsBuilder.expand(bounds)

            addFontInfo(tri.getFont)


            spatialPageInfo.last.insert(ch, x, y, charWidth, charHeight)
            // println(s"adding chunk for ${text} ${formatBounds(bounds)}")
          }

        }
      }
    }
  }

  override def endTextBlock(): Unit = {
  }

  override def renderImage(iri: ImageRenderInfo): Unit = {
    // val img = iri.getImage
  }




  def addFontInfo(font: DocumentFont): Unit = {
    // val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
    // val allNameEntries = font.getAllNameEntries.map(_.mkString("[", ",", "]")).mkString(", ")
    // val fontDictionary = font.getFontDictionary

    // val fontDictionaryKeys = fontDictionary.getKeys.map(_.toString()).mkString(",")
    val fontNameKey = font.getFullFontName.map(_.mkString("").trim).mkString("_").trim
    val _ = fontDict.put(fontNameKey, font)


    // debugReport(
    //   allNameEntries,
    //   // font.getCharBBox,
    //   // font.getFontDictionary,
    //   fontDictionaryKeys,
    //   // font.getFontMatrix,
    //   fontFullname,
    //   font.getFullFontStream,
    //   // font.getKerning,
    //   font.getPostscriptFontName,
    //   // font.getWidth,
    //   // font.getWidth,
    //   font.hasKernPairs,
    //   font.isVertical,
    //   // font.getAscent,
    //   // font.getAscentPoint,
    //   // font.getCidCode,
    //   // font.getCodePagesSupported.mkString(", "),
    //   // font.getCompressionLevel,
    //   // font.getDescent,
    //   // font.getDescentPoint,
    //   // font.getDifferences.mkString(", "),
    //   font.getEncoding,
    //   font.getFontType,
    //   font.getSubfamily,
    //   // font.getUnicodeDifferences.mkString(", "),
    //   // font.getUnicodeEquivalent,
    //   // font.getWidthPoint,
    //   // font.getWidthPoint,
    //   // font.getWidthPointKerned,
    //   // font.getWidths.mkString(", "),
    //   font.isDirectTextToByte,
    //   font.isEmbedded,
    //   font.isFontSpecific,
    //   font.isForceWidthsOutput,
    //   font.isSubset
    // )

    // charExists            (Int)           => Boolean
    // convertToBytes        (String)        => Array[Byte]
    // getAllNameEntries     ()              => Array[Array[String]]
    // getCharBBox           (Int)           => Array[Int]
    // getFamilyFontName     ()              => Array[Array[String]]
    // getFontDescriptor     (Int, Float)    => Float
    // getFontDictionary     ()              => PdfDictionary
    // getFontMatrix         ()              => Array[Double]
    // getFullFontName       ()              => Array[Array[String]]
    // getFullFontStream     ()              => PdfStream
    // getKerning            (Int, Int)      => Int
    // getPostscriptFontName ()              => String
    // getWidth              (String)        => Int
    // getWidth              (Int)           => Int
    // hasKernPairs          ()              => Boolean
    // isVertical            ()              => Boolean
    // setKerning            (Int, Int, Int) => Boolean
    // setPostscriptFontName (String)        => Unit


    // com.itextpdf.text.pdf.BaseFont
    // ---------------------------
    // addSubsetRange        (Array[Int])    => Unit
    // correctArabicAdvance  ()              => Unit
    // getAscent             (String)        => Int
    // getAscentPoint        (String, Float) => Float
    // getCidCode            (Int)           => Int
    // getCodePagesSupported ()              => Array[String]
    // getCompressionLevel   ()              => Int
    // getDescent            (String)        => Int
    // getDescentPoint       (String, Float) => Float
    // getDifferences        ()              => Array[String]
    // getEncoding           ()              => String
    // getFontType           ()              => Int
    // getSubfamily          ()              => String
    // getUnicodeDifferences ()              => Array[Char]
    // getUnicodeEquivalent  (Int)           => Int
    // getWidthPoint         (String, Float) => Float
    // getWidthPoint         (Int, Float)    => Float
    // getWidthPointKerned   (String, Float) => Float
    // getWidths             ()              => Array[Int]
    // isDirectTextToByte    ()              => Boolean
    // isEmbedded            ()              => Boolean
    // isFontSpecific        ()              => Boolean
    // isForceWidthsOutput   ()              => Boolean
    // isSubset              ()              => Boolean
    // setCharAdvance        (Int, Int)      => Boolean
    // setCompressionLevel   (Int)           => Unit
    // setDirectTextToByte   (Boolean)       => Unit
    // setFontDescriptor     (Int, Float)    => Unit
    // setForceWidthsOutput  (Boolean)       => Unit
    // setSubset             (Boolean)       => Unit
  }








}


class XITextCharacterExtractor() extends CharacterExtractor {
  val DEFAULT_FRONT_PAGES_LIMIT = 20
  val DEFAULT_BACK_PAGES_LIMIT = 20
  val frontPagesLimit = DEFAULT_FRONT_PAGES_LIMIT
  val backPagesLimit = DEFAULT_BACK_PAGES_LIMIT

  val spatialInfo = mutable.ArrayBuffer[SpatialPageInfo]()
  val fontDict = mutable.HashMap[String, DocumentFont]()

  override def extractCharacters(stream: InputStream): BxDocument = {
    try {
      val documentCreator = new MyBxDocumentCreator(
        spatialInfo, fontDict
      )

      val reader = new PdfReader(stream)
      val processor = new PdfContentStreamProcessor(documentCreator)

      for (pageNumber <- 1 to reader.getNumberOfPages) {

        if (frontPagesLimit > 0 && backPagesLimit > 0 && pageNumber > frontPagesLimit
          && pageNumber < reader.getNumberOfPages() - 1 - backPagesLimit) {
          // continue
        } else {

          documentCreator.processNewBxPage(reader.getPageSize(pageNumber))

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
