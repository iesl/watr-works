package edu.umass.cs.iesl.watr
package ext

// import com.google.common.collect.Lists;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.parser.ImageRenderInfo
import com.itextpdf.text.pdf.parser.TextRenderInfo
import java.io.InputStream
import com.itextpdf.text.exceptions.InvalidPdfException;
import com.itextpdf.text.pdf.PRIndirectReference;
import com.itextpdf.text.pdf.PdfDictionary;
import com.itextpdf.text.pdf.PdfName;
import com.itextpdf.text.pdf.PdfReader;
import com.itextpdf.text.pdf.parser._
import java.io.IOException;
import java.io.InputStream;
import pl.edu.icm.cermine.exception.AnalysisException;
import pl.edu.icm.cermine.structure.model._
import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder;

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

class MyBxDocumentCreator extends RenderListener {

  val document = new BxDocument();
  var actPage: BxPage = null

  val boundsBuilder = new BxBoundsBuilder();

  var pageRectangle: Rectangle = null

  def processNewBxPage(pageRectangle: Rectangle): Unit = {
    if (actPage != null) {
      actPage.setBounds(boundsBuilder.getBounds());
      boundsBuilder.clear();
    }
    actPage = new BxPage();
    document.addPage(actPage);

    this.pageRectangle = pageRectangle;
  }

  override def beginTextBlock(): Unit = {
  }

  override def renderText(tri: TextRenderInfo): Unit = {
    for (charTri <- tri.getCharacterRenderInfos()) {
      val text = charTri.getText();
      println(s" tri = ${text}")
      val ch = charTri.getText().charAt(0);
      if (ch <= ' '
        || text.matches("^[\uD800-\uD8FF]$")
        || text.matches("^[\uDC00-\uDFFF]$")
        || text.matches("^[\uFFF0-\uFFFF]$")) {
      } else {

        val absoluteCharLeft: Double = charTri.getDescentLine().getStartPoint().get(PVector.I1).toDouble
        val absoluteCharBottom: Double = charTri.getDescentLine().getStartPoint().get(PVector.I2).toDouble

        val charLeft = absoluteCharLeft - pageRectangle.getLeft();
        val charBottom = absoluteCharBottom - pageRectangle.getBottom();

        var charHeight = charTri.getAscentLine().getStartPoint().get(PVector.I2).toDouble - charTri.getDescentLine().getStartPoint().get(PVector.I2);
        var charWidth = charTri.getDescentLine().getLength().toDouble

        // if (Float.isNaN(charHeight) || Float.isInfinite(charHeight)) {
        if (charHeight.nan || charHeight.inf) {
          charHeight = 0;
        }

        if (charWidth.nan || (charWidth.inf)) {
          charWidth = 0;
        }

        if (absoluteCharLeft < pageRectangle.getLeft()
          || absoluteCharLeft + charWidth > pageRectangle.getRight()
          || absoluteCharBottom < pageRectangle.getBottom()
          || absoluteCharBottom + charHeight > pageRectangle.getTop()) {
          // continue
        } else {

          val bounds = new BxBounds(charLeft, pageRectangle.getHeight() - charBottom - charHeight,
            charWidth, charHeight);

          if (bounds.getX().nan || bounds.getX().inf
            || bounds.getY().nan || bounds.getY().inf
            || bounds.getHeight().nan || bounds.getHeight().inf
            || bounds.getWidth().nan || bounds.getWidth().inf) {
          } else {

            val chunk = new BxChunk(bounds, text);
            val fullFontName = tri.getFont().getFullFontName()(0)(3)
            chunk.setFontName(fullFontName);
            actPage.addChunk(chunk);
            boundsBuilder.expand(bounds);
          }

        }
      }
    }
  }

  override def endTextBlock(): Unit = {
  }

  override def renderImage(iri: ImageRenderInfo): Unit = {
  }

}
class XITextCharacterExtractor() extends ITextCharacterExtractor {
  val DEFAULT_FRONT_PAGES_LIMIT = 20;
  val DEFAULT_BACK_PAGES_LIMIT = 20;
  val frontPagesLimit = DEFAULT_FRONT_PAGES_LIMIT;
  val backPagesLimit = DEFAULT_BACK_PAGES_LIMIT;

  override def extractCharacters(stream: InputStream): BxDocument = {
    try {
      val documentCreator = new MyBxDocumentCreator();

      val reader = new PdfReader(stream);
      val processor = new PdfContentStreamProcessor(documentCreator);

      for (pageNumber <- 1 to reader.getNumberOfPages) {

        if (frontPagesLimit > 0 && backPagesLimit > 0 && pageNumber > frontPagesLimit
          && pageNumber < reader.getNumberOfPages() - 1 - backPagesLimit) {
          // continue;
        } else {

          documentCreator.processNewBxPage(reader.getPageSize(pageNumber));

          val resources = reader.getPageN(pageNumber).getAsDict(PdfName.RESOURCES);
          processAlternativeFontNames(resources);
          processAlternativeColorSpace(resources);

          processor.reset();
          processor.processContent(ContentByteUtils.getContentBytesForPage(reader, pageNumber), resources);
        }
      }

      filterComponents(removeDuplicateChunks(documentCreator.document));

    } catch {
      case ex: InvalidPdfException =>
        throw new AnalysisException("Invalid PDF file", ex);
      case ex: IOException =>
        throw new AnalysisException("Cannot extract characters from PDF file", ex);
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
    val fontsDictionary = resources.getAsDict(PdfName.FONT);

    if (fontsDictionary == null) {
      return ;
    }
    for (pdfFontName <- fontsDictionary.getKeys()) {
      if (!(fontsDictionary.get(pdfFontName).isInstanceOf[PRIndirectReference])) {
        return ;
      } else {

        val indRef = fontsDictionary.get(pdfFontName).asInstanceOf[PRIndirectReference]
        val fontDictionary = PdfReader.getPdfObjectRelease(indRef).asInstanceOf[PdfDictionary]

        val baseFont = fontDictionary.getAsName(PdfName.BASEFONT);
        if (baseFont != null) {
          val fontName = PdfName.decodeName(baseFont.toString());
          if (fontDictionary.getAsArray(PdfName.WIDTHS) == null && ALT_TO_STANDART_FONTS.containsKey(fontName)) {
            fontDictionary.put(PdfName.BASEFONT, ALT_TO_STANDART_FONTS(fontName));
          }
        }
      }
    }
  }

  def processAlternativeColorSpace(resources: PdfDictionary): Unit = {
    val csDictionary = resources.getAsDict(PdfName.COLORSPACE);
    if (csDictionary == null) {
      return ;
    }
    for (csName <- csDictionary.getKeys()) {
      if (csDictionary.getAsArray(csName) != null) {
        csDictionary.put(csName, PdfName.DEVICEGRAY);
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
        val x = chunk.getX().toInt;
        val y = chunk.getY().toInt;
        var duplicate = false;

        // duplicateSearch:
        try {
          for (
            i <- x - 1 to x + 1;
            j <- y - 1 to y + 1
          ) {
            if (chunkMap.contains(i) && chunkMap(i).contains(j)) {

              for (ch <- chunkMap(i)(j)) {
                if (chunk.toText().equals(ch.toText()) && chunk.getBounds().isSimilarTo(ch.getBounds(), 1)) {
                  duplicate = true;
                  // break duplicateSearch;
                  throw new Throwable()
                }
              }
            }
          }

        } catch {
          case break: Throwable =>
        }

        if (!duplicate) {
          filteredChunks.add(chunk);
          val x = chunk.getX().toInt;
          val y = chunk.getY().toInt;

          if (!chunkMap.contains(x)) {
            chunkMap.put(x, intToChunksMap)
          }
          if (!chunkMap(x).contains(y)) {
            chunkMap(x).put(y, chunkSet)
          }
          chunkMap(x)(y).add(chunk);
        }
      }
      page.setChunks(filteredChunks);
    }
    document
  }

  val CHUNK_DENSITY_LIMIT = 15;
  val PAGE_GRID_SIZE = 10;

  def filterComponents(document: BxDocument): BxDocument = {
    for (page <- document) {
      val bounds = new BxBoundsBuilder();
      // val chunks = Lists.newArrayList(page.getChunks());
      // val chunks = page.getChunks
      val chunks = mutable.ArrayBuffer(page.getChunks.toList: _*)
      for (ch <- chunks) {
        bounds.expand(ch.getBounds());
      }

      val density = 100.0 * chunks.size / (bounds.getBounds().getWidth() * bounds.getBounds().getHeight());
      if (density.nan || density < CHUNK_DENSITY_LIMIT) {
        //continue;
      } else {

        val map = mutable.HashMap[String, mutable.ArrayBuffer[BxChunk]]();
        for (ch <- chunks) {
          val x = (ch.getX() / PAGE_GRID_SIZE).toInt
          val y = (ch.getY() / PAGE_GRID_SIZE).toInt
          val key = Integer.toString(x) + " " + Integer.toString(y);
          if (map.contains(key) == null) {
            map.put(key, mutable.ArrayBuffer[BxChunk]());
          }
          map(key).add(ch);
        }

        for (list <- map.values()) {
          if (list.size() > CHUNK_DENSITY_LIMIT) {
            for (ch <- list) {
              chunks.remove(ch);
            }
          }
        }
        page.setChunks(chunks);
      }
    }
    document;
  }

}
