package edu.umass.cs.iesl.watr
package extract

// import com.itextpdf.kernel.pdf.canvas.parser.PdfCanvasProcessor
// import java.io.InputStream

// import _root_.com.itextpdf
// import itextpdf.kernel.pdf._
// import utils.IdGenerator
// import geometry._

// import TypeTags._
// import scala.collection.mutable
// import scala.collection.JavaConverters
// import ammonite.{ops => fs}, fs._
// import java.nio.{file => nio}


// object PdfTextExtractor {

//   def extractPages(stableId: String@@DocumentID, pdfPath: Path): List[(Seq[ExtractedItem], PageGeometry)] = {
//     val charExtractor = new PdfTextExtractor()

//     charExtractor.extractPages(stableId, pdfPath)
//   }


// }

// class PdfTextExtractor() {

//   val bboxNames = List[PdfName](
//     PdfName.CropBox,
//     PdfName.TrimBox,
//     PdfName.MediaBox,
//     PdfName.BleedBox
//   )

//   def getBestBoundingBox(pdfObject: PdfDictionary): Array[Double] = {
//     bboxNames
//       .map(pdfObject.getAsArray(_))
//       .filterNot(_ == null)
//       .headOption
//       .map{ x =>
//         JavaConverters.asScalaIterator(x.iterator()).toArray
//           .map(_.asInstanceOf[PdfNumber].doubleValue())
//       }
//       .getOrElse{
//         val parent = pdfObject.getAsDictionary(PdfName.Parent)
//         if (parent!=null) {
//           getBestBoundingBox(parent)
//         } else {
//           sys.error("no bounding box (media/crop/trim/etc) found in pdfobjects!")
//         }
//       }
//   }

//   def getBestBoundingBox(pdfPage: PdfPage): Array[Double] = {
//     getBestBoundingBox(pdfPage.getPdfObject)
//   }

//   def getReportedPageGeometry(pageId: Int@@PageNum, pdfPage: PdfPage, reader: PdfReader): (PageGeometry, GeometryTranslation) = {

//     val nums: Array[Double] = getBestBoundingBox(pdfPage)

//     val lval = nums(0)
//     val bval = nums(1)
//     val rval = nums(2)
//     val tval = nums(3)

//     def xtrans(x: Double): Double = {
//       x - lval
//     }
//     def ytrans(y: Double): Double = {
//       (tval - y)
//     }

//     val left = xtrans(lval)
//     val top = ytrans(tval)
//     val right = xtrans(rval)
//     val bottom = ytrans(bval)

//     val bounds = LTBounds.Doubles(
//       left = left,
//       top = top,
//       width = right,
//       height = bottom
//     )


//     (PageGeometry(pageId, bounds),
//       GeometryTranslation(xtrans, ytrans))

//   }


//   def extractPages(stableId: String@@DocumentID, pdfPath: Path): List[(Seq[ExtractedItem], PageGeometry)] = {
//     var instr: InputStream = null
//     val pages = mutable.ListBuffer[(Seq[ExtractedItem], PageGeometry)]()
//     val charIdGen = IdGenerator[CharID]()

//     try {
//       instr = nio.Files.newInputStream(pdfPath.toNIO)
//       val reader = new PdfReader(instr)
//       val document = new PdfDocument(reader)

//       val numOfPages = document.getNumberOfPages

//       println(s"Extracting ${numOfPages} pages.")
//       for (pageNumber <- 1 to numOfPages) {
//         print(s".${pageNumber}")

//         val pageNum = PageNum(pageNumber-1)

//         val pdfPage = document.getPage(pageNumber)

//         val (pageGeometry, geomTrans) = getReportedPageGeometry(pageNum, pdfPage, reader)

//         val extractor = new RunTrackingListener(
//           reader,
//           charIdGen,
//           pdfPage,
//           pageNum,
//           pageGeometry,
//           geomTrans
//         )

//         try {
//           val parser = new PdfCanvasProcessor(extractor)
//           parser.processPageContent(pdfPage)


//           val pageItems = extractor.getPageItems()

//           val charCount = extractor.totalCharCount
//           val maxExceeded = charCount > extractor.MAX_EXTRACTED_CHARS_PER_PAGE

//           if (maxExceeded) {
//             println(s"Max chars per page limit exceeded: extracted only ${pageItems.length} of ${charCount} chars")
//           }

//           parser.reset()

//           pages.append((pageItems, pageGeometry))
//         } catch {
//           case f: Throwable =>
//             println(s"ERROR extractCharacters(page: ${pageNumber}): ${f}: ${f.getMessage} ${f.getCause()}")
//             // f.printStackTrace()
//         } finally {
//           if (instr != null) instr.close()
//         }

//       }

//     } catch {
//       case f: Throwable =>
//         println(s"ERROR extractCharacters(): ${f}: ${f.getMessage} ${f.getCause()}")
//         // f.printStackTrace()
//     } finally {
//       if (instr != null) instr.close()
//       println()
//     }

//     pages.toList
//   }


// }
// -  def computeTextBounds(charTri: TextRenderInfo): Option[LTBounds] = {
// -    val fontProgramEmbedded = charTri.getFont.getFontProgram
// -
// -    val fontProgram = fontProgramEmbedded
// -
// -    val fontMetrics = fontProgram.getFontMetrics
// -
// -    val ascentStart = charTri.getAscentLine().getStartPoint()
// -    val descentStart = charTri.getDescentLine().getStartPoint()
// -
// -    val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble
// -
// -    val ascentY = ascentStart.get(PVector.I2).toDouble
// -    val descentY = descentStart.get(PVector.I2).toDouble
// -
// -    var absCharBottom: Double = descentY
// -    var charHeight = ascentY - descentY
// -
// -    if (charHeight < 0 ) {
// -      charHeight = - charHeight
// -      absCharBottom = ascentY
// -    }
// -
// -    val charLeft = geomTranslation.transX(absoluteCharLeft)
// -    val charBottom = geomTranslation.transY(absCharBottom)
// -
// -    var charWidth = charTri.getDescentLine().getLength().toDouble
// -
// -    if (charWidth.toInt == 0) {
// -      // figure out the exact dimensions of this glyph...
// -      // In glyph space:
// -
// -      val pdfString = charTri.getPdfString
// -      // val decoded = charTri.getFont.decode(pdfString)
// -      val bs = pdfString.getValueBytes.map(Byte.byte2int(_) & 0xFF)
// -      val glyphCode = bs(0)
// -
// -      val fontBbox = fontMetrics.getBbox
// -      val glyphWidths = fontMetrics.getGlyphWidths
// -
// -      if (glyphWidths!=null && glyphWidths.length > glyphCode) {
// -        charWidth = glyphWidths(glyphCode).toDouble
// -      } else {
// -        if (fontBbox != null) {
// -          val y0 = fontBbox(1)
// -          val y1 = fontBbox(3)
// -          charWidth = (y1 - y0).toDouble
// -        }
// -      }
// -    }
// -
// -
// -
// -
// -    if (charHeight.nan || charHeight.inf || charHeight.toInt==0) {
// -      None
// -    } else if (charWidth.nan || charWidth.inf || charWidth.toInt==0) {
// -      None
// -    } else {
// -      val charTop = charBottom - charHeight
// -
// -      Some(LTBounds.Doubles(
// -        left=charLeft,
// -        top=charTop,
// -        width=charWidth,
// -        height=charHeight
// -      ))
// -    }
// -  }
