package edu.umass.cs.iesl.watr
package ext

import java.io.InputStream
import com.itextpdf.text.Rectangle
import com.itextpdf.text.exceptions.InvalidPdfException
import com.itextpdf.text.pdf._
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
import util._
import watrmarks.SpatialPageInfo
import scala.collection.mutable


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


class MyBxDocumentCreator(
  spatialPageInfo: mutable.ArrayStack[SpatialPageInfo],
  fontDict: mutable.Map[String, DocumentFont],
  reader: PdfReader
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

    spatialPageInfo.push(SpatialPageInfo(
      pageBounds = watrmarks.TextBounds(
        left = pageRectangle.getLeft.toDouble,
        bottom = pageRectangle.getBottom.toDouble,
        width = pageRectangle.getWidth.toDouble,
        height = pageRectangle.getHeight.toDouble
      )))

  }

  import better.files._
  val output = "asdf".toFile
  val chars = mutable.ArrayBuffer[String]()

  override def beginTextBlock(): Unit = {
    // println("\nblock\n")
  }

  def formatBounds(bounds: BxBounds): String = {
    val x = bounds.getX
    val y = bounds.getY
    val w = bounds.getWidth
    val h = bounds.getHeight
    def fmt = (d: Double) => f"${d}%1.2f"
    s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
  }

  def outputCharInfo(tri: TextRenderInfo): Unit = {
    val font = tri.getFont()
    // val text2 = font.getUnicodeEquivalent(tri.getMcid)
    val pdfstring = tri.getPdfString
    val ffs = font.getFullFontStream

    // val dictKvs = "Type,Subtype,Name/BaseFont".split(",").map{key =>
    //   font.getFontDictionary.get(new PdfName(s"/$key"))
    // }.mkString("\n")

    val asString = tri.getText.toCharArray().map{c =>
      Char.char2int(c).toString
    }.mkString

    val bs = pdfstring.getBytes.map(Byte.byte2int(_)).mkString(",")
    val obs = pdfstring.getOriginalBytes.map(Byte.byte2int(_)).mkString(",")

    val pdfstrInf = s"""|${tri.getText}:  '${asString}'
                        |    getBytes         ${bs} => Array[Byte]
                        |    getEncoding      ${pdfstring.getEncoding} => String
                        |    getOriginalBytes ${obs} => Array[Byte]
                        |    isHexWriting     ${pdfstring.isHexWriting        } => Boolean
                        |    toString         ${pdfstring.toString            } => String
                        |    toUnicodeString  ${pdfstring.toUnicodeString     } => String
                        |    canBeInObjStm    ${pdfstring.canBeInObjStm       } => Boolean
                        |    getIndRef        ${pdfstring.getIndRef           } => PRIndirectReference
                        |    isArray          ${pdfstring.isArray             } => Boolean
                        |    isBoolean        ${pdfstring.isBoolean           } => Boolean
                        |    isDictionary     ${pdfstring.isDictionary        } => Boolean
                        |    isIndirect       ${pdfstring.isIndirect          } => Boolean
                        |    isName           ${pdfstring.isName              } => Boolean
                        |    isNull           ${pdfstring.isNull              } => Boolean
                        |    isNumber         ${pdfstring.isNumber            } => Boolean
                        |    isStream         ${pdfstring.isStream            } => Boolean
                        |    isString         ${pdfstring.isString            } => Boolean
                        |    length           ${pdfstring.length              } => Int
                        |    type             ${pdfstring.`type`              } => Int
                        |
                        |""".stripMargin

    val d0 = font.getDifferences
    val d1 = font.getUnicodeDifferences
    val dict  = font.getFontDictionary

    val dictKvs = font.getFontDictionary.getKeys.map{ key =>
      "    " +key.toString() + ": " + font.getFontDictionary.get(key)
    }.mkString("\n")


    val diffStr = d0.mkString(",")
    val udiffStr = d1.mkString(",")

    val unicodeEquiv = font.getUnicodeEquivalent(0)
      // |    diffs             ${diffStr}
      // |    unidiffs          ${udiffStr}

    object formatting {
      import watrmarks.TB._

      def formatPdfObject(obj: PdfObject): String = {
        renderBox(formatObject(obj)).mkString("\n")
      }

      def formatObject(obj: PdfObject): Box = {
        if (obj.isName) {
          s"name:$obj".box
        } else if (obj.isIndirect()) {
          val xo = obj.asInstanceOf[PdfIndirectReference]
          val xo2 = obj.asInstanceOf[PdfIndirectObject]
          val sub = reader.getPdfObject(xo.getNumber)
          "indirect:" beside formatObject(sub)
        } else if (obj.isStream()) {
          val xo = obj.asInstanceOf[PdfStream]
          // new java.io.OutputStreamWriter(new java.io.StringWriter())
          val sw = new java.io.StringWriter()

          val baos = new java.io.ByteArrayOutputStream()
          xo.writeContent(baos)
          val s = baos.toByteArray().mkString("[", ",", "]")
          // val rlen = xo.getRawLength
          // val bytes = if (rlen>0 && xo.getBytes != null) {
          //   xo.getBytes.mkString("")
          // } else "[]"

          val other = vcat(center2)(List(
            "bytes" besideS s
          ))

          "stream:" atop indent(3)(
            other atop vcat(center1)(
              xo.getKeys.toList.map{ key =>
                val dval = xo.get(key)

                val valBox = if (dval.isIndirect()) {
                  val xo = dval.asInstanceOf[PdfIndirectReference]
                  val sub = reader.getPdfObject(xo.getNumber)
                  formatObject(sub)
                } else if (dval.isStream()) {
                  formatObject(dval)
                } else {
                  formatObject(dval)
                }
                s"${key}:".box beside valBox
              }
            ))
        } else if (obj.isDictionary) {
          val xo = obj.asInstanceOf[PdfDictionary]

          "dict:" atop indent(3)(
            vcat(center1)(
              xo.getKeys.toList.map{ key =>
                val dval = xo.get(key)

                val valBox = if (dval.isIndirect()) {
                  val xo = dval.asInstanceOf[PdfIndirectReference]
                  val sub = reader.getPdfObject(xo.getNumber)
                  formatObject(sub)
                } else if (dval.isStream()) {
                  formatObject(dval)
                } else {
                  formatObject(dval)
                }
                s"${key}:".box beside valBox
              }
            )
          )
        } else {
          s"<${obj}>".box
        }
      }
    }

    val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
    val charProcs = dict.getAsDict(new PdfName("CharProcs"))
    val charProcInf = if (charProcs != null) {
      formatting.formatPdfObject(charProcs).toString()
    } else "<no CharProcs>"


    val fontinf = s"""|${tri.getText}
                      |    Font              ${fontFullname}      => DocumentFont
                      |       Unic.Equiv     ${unicodeEquiv}      => Int
                      |    Mcid              ${tri.getMcid}              => Integer
                      |    PdfString         ${tri.getPdfString}              => PdfString
                      |    TextRenderMode    ${tri.getTextRenderMode}              => Int
                      | ${dictKvs}
                      | ${charProcInf}
                      |""".stripMargin

    def formatLineVector(ls: PVector): String = {
      s"""[${ls.get(0)} ${ls.get(1)}, ${ls.get(2)}}}]"""
    }

    def formatLineSegment(ls: LineSegment): String = {
      s""" ${formatLineVector(ls.getStartPoint)} -> ${formatLineVector(ls.getEndPoint)} ${ls.getLength}}"""

    }



    // val bbinf = s"""|${tri.getText}:  '${asString}'
    //                 |    Rise              ${tri.getRise}                                => Float
    //                 |    AscentLine        ${formatLineSegment(tri.getAscentLine)}       => LineSegment
    //                 |    Baseline          ${formatLineSegment(tri.getBaseline)}         => LineSegment
    //                 |    Baseline (uns)    ${formatLineSegment(tri.getUnscaledBaseline)} => LineSegment
    //                 |    DescentLine       ${formatLineSegment(tri.getDescentLine)}      => LineSegment
    //                 |    Mcid              ${tri.getMcid}              => Integer
    //                 |    PdfString         ${tri.getPdfString}              => PdfString
    //                 |""".stripMargin

    // println(bbinf)


    // if (chars.length > 20) {
    //   output << chars.mkString
    //   chars.clear()
    // }

    // chars.append(tri.getText)

    // val _ = output << pdfstrInf
    // output << fontinf

    // | SingleSpaceWidth  ${tri.getSingleSpaceWidth }                 => Float
    // | StrokeColor       ${tri.getStrokeColor      }              => BaseColor
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

        val ascentStart = charTri.getAscentLine().getStartPoint()
        val descentStart = charTri.getDescentLine().getStartPoint()

        val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble
        val absoluteCharBottom: Double = descentStart.get(PVector.I2).toDouble

        val charLeft = absoluteCharLeft - pageRectangle.getLeft()
        val charBottom = absoluteCharBottom - pageRectangle.getBottom()


        var charHeight = ascentStart.get(PVector.I2).toDouble - descentStart.get(PVector.I2)
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
          // skip...
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

  val spatialInfo = mutable.ArrayStack[SpatialPageInfo]()
  val fontDict = mutable.HashMap[String, DocumentFont]()


  override def extractCharacters(stream: InputStream): BxDocument = {
    try {
      val reader = new PdfReader(stream)

      val documentCreator = new MyBxDocumentCreator(
        spatialInfo, fontDict, reader
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
