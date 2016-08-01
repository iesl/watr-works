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
import scala.collection.JavaConversions._

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
  componentIdGen: IdGenerator[RegionID]
) {

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

    (PageGeometry(pageId, bounds),
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

}
