package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.canvas.parser.PdfCanvasProcessor
import java.io.InputStream

import _root_.com.itextpdf
import itextpdf.kernel.pdf._
import utils.IdGenerator
import geometry._

import TypeTags._
import scala.collection.mutable
import scala.collection.JavaConversions._
import ammonite.{ops => fs}, fs._
import java.nio.{file => nio}


object PdfTextExtractor {
  def extractChars(stableId: String@@DocumentID, pdfPath: Path): Seq[(Seq[CharAtom], PageGeometry)] = {
    val charExtractor = new PdfTextExtractor(
      Set[Int](), IdGenerator[CharID]()
    )

    charExtractor.extractCharacters(stableId, pdfPath)
  }

}

class PdfTextExtractor(
  charsToDebug: Set[Int] = Set(),
  charIdGen: IdGenerator[CharID]
) {

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

  def getReportedPageGeometry(pageId: Int@@PageNum, pdfPage: PdfPage, reader: PdfReader): (PageGeometry, GeometryTranslation) = {

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

    val bounds = LTBounds.Doubles(
      left = left,
      top = top,
      width = right,
      height = bottom
    )

    (PageGeometry(pageId, bounds),
      GeometryTranslation(xtrans, ytrans))

  }


  def extractCharacters(stableId: String@@DocumentID, pdfPath: Path): List[(Seq[CharAtom], PageGeometry)] = {
    var instr: InputStream = null
    val pages = mutable.ListBuffer[(Seq[CharAtom], PageGeometry)]()
    try {
      instr = nio.Files.newInputStream(pdfPath.toNIO)
      val reader = new PdfReader(instr)
      val document = new PdfDocument(reader)


      for (pageNumber <- 1 to document.getNumberOfPages) {

        val pageId = PageNum(pageNumber-1)

        val pdfPage = document.getPage(pageNumber)

        val currCharBuffer: mutable.ArrayBuffer[CharAtom] = mutable.ArrayBuffer[CharAtom]()
        val (pageGeometry, geomTrans) = getReportedPageGeometry(pageId, pdfPage, reader)

        val extractor = new CharExtractionListener(
          reader, stableId, charsToDebug,
          charIdGen,
          currCharBuffer,
          pdfPage,
          pageId,
          pageGeometry,
          geomTrans
        )

        val pageAtoms = try {
          val parser = new PdfCanvasProcessor(extractor);
          parser.processPageContent(pdfPage)


          val pageAtoms = Seq[CharAtom](currCharBuffer:_*)

          val charCount = extractor.totalCharCount
          val maxExceeded = charCount > extractor.MAX_EXTRACTED_CHARS_PER_PAGE

          if (maxExceeded) {
            println(s"Max chars per page limit exceeded: extracted only ${pageAtoms.length} of ${charCount} chars")
          } else {
            println(s"Extracted ${pageAtoms.length} chars out of ${charCount} glyphs")

          }

          parser.reset()

          (pageAtoms, pageGeometry)
        } catch {
          case f: Throwable => println(s"ERROR extractCharacters(page: ${pageNumber}): ${f}: ${f.getMessage} ${f.getCause()}")

            (List(), pageGeometry)
        } finally {
          if (instr != null) instr.close()
        }

        pages.append(pageAtoms)
      }

    } catch {
      case f: Throwable =>
        println(s"ERROR extractCharacters(): ${f}: ${f.getMessage} ${f.getCause()}")
        f.printStackTrace()
        List()
    } finally {
      if (instr != null) instr.close()
    }

    pages.toList
  }

}
