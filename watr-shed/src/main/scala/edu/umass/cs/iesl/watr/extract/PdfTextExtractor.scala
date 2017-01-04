package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.canvas.parser.PdfCanvasProcessor
import java.io.InputStream 

import _root_.com.itextpdf
import itextpdf.kernel.pdf._

import utils.IdGenerator
import geometry._
import GeometricFigure._
//import TypeTags._
import scala.collection.mutable
import scala.collection.JavaConversions._
import extract.fonts._

import scalaz.@@

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
  componentIdGen: IdGenerator[RegionID],
  glyphDefs: Seq[SplineFont.Dir] = Seq()
) {
  // map font-name,encoding-index -> glyph-hash
  // map glyph-hash -> unicode/bbox

  val glyphList = glyphDefs.map({sdir =>
    val fontName = sdir.prop[FontProp.FontName]
    sdir.glyphs.flatMap({glyph =>
      val splines = glyph.get[GlyphProp.SplineSet]
      val enc = glyph.prop[GlyphProp.Encoding]
      val encNums = enc.v.trim.split(" ").map(_.toInt)
      val encIndex = encNums(2)
      splines.map(sp =>
        ((fontName.v.trim, encIndex), GlyphProp.splineSetHash(sp).unwrap)
      )
    })
  })

  val glyphMap = glyphList.flatten.toMap


  val bboxNames = List[PdfName](
    PdfName.CropBox,
    PdfName.TrimBox,
    PdfName.MediaBox,
    PdfName.BleedBox
  )

  def getBestBoundingBox(pdfObject: PdfDictionary): Array[Double] = {
    bboxNames
      .map(pdfObject.getAsArray(_))
      .filterNot(_ == null)
      .headOption
      .map(_.toArray.map(_.asInstanceOf[PdfNumber].doubleValue()))
      .getOrElse({
        val parent = pdfObject.getAsDictionary(PdfName.Parent)
        if (parent!=null) {
          getBestBoundingBox(parent)
        } else {
          sys.error("no bounding box (media/crop/trim/etc) found in pdfobjects!")
        }
      })
  }

  def getBestBoundingBox(pdfPage: PdfPage): Array[Double] = {
    getBestBoundingBox(pdfPage.getPdfObject)
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

    (PageGeometry(pageId, bounds),
      GeometryTranslation(xtrans, ytrans))

  }

  import ammonite.{ops => fs}, fs._
  import java.nio.{file => nio}


  def extractCharacters(pdfPath: Path): List[(Seq[PageAtom], PageGeometry)] = {
    var instr: InputStream = null
    try {
      instr = nio.Files.newInputStream(pdfPath.toNIO)
      val reader = new PdfReader(instr)
      val document = new PdfDocument(reader)

      val pageIndexes = for (pageNumber <- 1 to document.getNumberOfPages) yield {
        // val tagStructureContext = pdfPage.getDocument.getTagStructureContext

        val pageId = PageID(pageNumber-1)

        val pdfPage = document.getPage(pageNumber)


        val resources = pdfPage.getResources

        val currCharBuffer: mutable.ArrayBuffer[PageAtom] = mutable.ArrayBuffer[PageAtom]()
        val (pageGeometry, geomTrans) = getReportedPageGeometry(pageId, pdfPage, reader)

        val extractor = new CharExtractionListener(
          reader, charsToDebug,
          componentIdGen,
          currCharBuffer,
          pdfPage,
          pageId,
          pageGeometry,
          geomTrans,
          glyphMap
        )

        val parser = new PdfCanvasProcessor(extractor);
        parser.processPageContent(pdfPage)


        val pageAtoms = Seq[PageAtom](currCharBuffer:_*)

        parser.reset()

        (pageAtoms, pageGeometry)
      }
      pageIndexes.toList
    } catch {
      case f: Throwable => throw f 
    } finally {
      if (instr != null) instr.close()
    }
  }

}
