package edu.umass.cs.iesl.watr
package extract

import java.awt.{Shape}
import java.awt.geom._
import java.awt.geom.AffineTransform

import scala.collection.mutable

import ammonite.{ops => fs}
import ammonite.ops._
import org.apache.fontbox.util.BoundingBox
import org.apache.pdfbox.contentstream._
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.graphics.image.PDImage
import org.apache.pdfbox.pdmodel.font._
import org.apache.pdfbox.util.{Matrix, Vector}
// import scala.collection.JavaConverters._



import TypeTags._
import geometry._
import utils._

case class GlyphProps(
  finalGlyphBounds: Option[Shape],
  finalFontBounds: Shape,
  finalAffineTrans: AffineTransform
) {
  // strRepr: Option[String],

  // lazy val affineTransform: AffineTransform = {
  //   val at  = textRenderingMatrix.createAffineTransform()
  //   at.concatenate(fontMatrix.createAffineTransform())
  //   at
  // }

}

class PdfBoxTextExtractor(
  page: PDPage,
  charIdGen: IdGenerator[CharID],
  pageNum: Int@@PageNum,
  fontDefs: FontDefs
) extends PDFGraphicsStreamEngine(page) {
  import ExtractedItem._
  import PdfBoxExtractorMain._
  import FontDefs._
  // Limit the # of chars that can be extracted per page to prevent pathological cases (e.g., embedded charts using symbol font-based dots)
  val MAX_EXTRACTED_CHARS_PER_PAGE = 10000

  var totalCharCount = 0

  var extractedItems = List[ExtractedItem]()

  def getPageItems(): Seq[ExtractedItem] = {
    extractedItems.reverse
  }

  protected def debugPrintPageItems(rows: List[List[ExtractedItem]]): Unit = {
    val rowStrs = rows.map { row =>
      val runId = row.headOption.map(_.charProps.charRunId).getOrElse(-1)
      val chars = row.map(_.strRepr()).mkString
      s"${runId} >> $chars"
    }

    val pageStr = rowStrs.mkString("\n  ", "\n  ", "\n")

    println(pageStr)
  }

  def addPathItem(item: ExtractedItem.PathItem): Unit = {
    extractedItems = item :: extractedItems
  }

  def addImgItem(item: ExtractedItem.ImgItem): Unit = {
    // println(s"addImgItem: ${item}")
    extractedItems = item :: extractedItems
  }

  def addCharItem(charAtom: ExtractedItem.CharItem): Unit = {
    extractedItems = charAtom :: extractedItems
  }

  def stashChar(c: Char): Unit = {
    // addCharItem(CharItem(charIdGen.nextId, charBounds, c))
  }

  def traceFunc()(implicit enc: sourcecode.Enclosing): Unit = {
    println(s"running ${enc.value}")
  }

  override def appendRectangle(p0: Point2D, p1: Point2D, p2: Point2D, p3: Point2D): Unit = {
    p0.getX()
    p0.getY()
    p1.getX()
    p1.getY()
    p2.getX()
    p2.getY()
    p3.getX()
    p3.getY()

    // traceFunc()
  }

  override def clip(x1: Int): Unit = {
    // traceFunc()
  }

  override def closePath(): Unit = {
    // traceFunc()
  }

  override def curveTo(x1: Float,x2: Float,x3: Float,x4: Float,x5: Float,x6: Float): Unit = {
    // traceFunc()
  }

    //  def ctmToLTBounds(ctm:  Matrix): LTBounds = {
    // -    val x1 = ctm.get(6).toDouble
    // -    val y1 = ctm.get(7).toDouble
    // -    val x2 = x1 + ctm.get(0)
    // -    val y2 = y1 + ctm.get(4)
    // -    val w = x2 - x1
    // -    val h = y2 - y1
    // -
    // -    val left = geomTranslation.transX(x1)
    // -    val top = geomTranslation.transY(y2)
    // -
    // -    val width = math.max(w, 0.1)
    // -    val height = math.max(h, 0.1)
    // -
    // -    LTBounds.Doubles(left, top, width, height)
    // -  }

  override def drawImage(image: PDImage): Unit = {
    val ctm = getGraphicsState.getCurrentTransformationMatrix()
    val x1 = ctm.getValue(2, 0).toDouble
    val y1 = ctm.getValue(2, 1).toDouble
    val x2 = x1 + ctm.getValue(0, 0)
    val y2 = y1 + ctm.getValue(1, 1)

    val width = x2 - x1
    val height = y2 - y1

    val left = x1 - cropBox.getLowerLeftX
    val top = cropBox.getUpperRightY - y2

    val imgBounds = LTBounds.Doubles(left, top, width, height)

    val item = ExtractedItem.ImgItem(
      charIdGen.nextId,
      imgBounds
    )

    addImgItem(item)

    // traceFunc()
  }

  override def endPath(): Unit = {
    // traceFunc()
  }

  override def fillAndStrokePath(x1: Int): Unit = {
    // traceFunc()
  }

  override def fillPath(x1: Int): Unit = {
    // traceFunc()
  }

  override def getCurrentPoint(): Point2D = {
    // traceFunc()
    // if you want to build paths, you'll need to keep track of this
    new Point2D.Float(0, 0)
  }

  override def lineTo(x1: Float,x2: Float): Unit = {
    // traceFunc()
  }

  override def moveTo(x1: Float,x2: Float): Unit = {
    // traceFunc()
  }

  override def shadingFill(x1: org.apache.pdfbox.cos.COSName): Unit = {
    // traceFunc()
  }

  override def strokePath(): Unit = {
    // traceFunc()
  }


  override def showTextString(string: Array[Byte]): Unit = {
    super.showTextString(string)
  }


  def printSubs(): Unit = {
    // val hexOutput = cs.map(_.toInt.toHexString).mkString(", ")
    // val hexIn = unicode.head.toInt.toHexString
    // println(s"subbed unicode: ${unicode} => ${cs.mkString(','.toString())}")
    // println(s"              : ${hexIn} => ${hexOutput}")
  }

  def isCombiningMark(c: Char): Boolean = {
    val cint = c.toInt
    lazy val ext1 = 0x1AB0 <= cint && cint  <= 0x1AFF
    lazy val ext2 = 0x1DC0 <= cint && cint  <= 0x1DFF
    lazy val ext3 = 0x20D0 <= cint && cint  <= 0x20FF
    lazy val ext4 = 0xFE20 <= cint && cint  <= 0xFE2F

    (0x300 <= cint && cint <= 0x36F
      || ext1 || ext2 || ext3 || ext4
    )
  }

  private def getFontBounds(font: PDFont): PDRectangle = {
    val bbox = font.getBoundingBox
    val xmin = bbox.getLowerLeftX
    val ymin = bbox.getLowerLeftY
    val xmax = bbox.getUpperRightX
    val ymax = bbox.getUpperRightY
    val w = xmax - xmin
    val h = ymax - ymin
    new PDRectangle(xmin, ymin, w, h)
  }

  var pageSpaceTransform: AffineTransform = new AffineTransform()
  // var flipAT: AffineTransform = new AffineTransform()
  // var rotateAT: AffineTransform = new AffineTransform()
  // var transAT: AffineTransform = new AffineTransform()
  private def transformShapeOnPage(shape: Shape): Shape = {
    // var sh = shape
    // sh = flipAT.createTransformedShape(sh)
    // sh = rotateAT.createTransformedShape(sh)
    // transAT.createTransformedShape(sh)
    pageSpaceTransform.createTransformedShape(shape)
  }

  override protected def showGlyph(
    textRenderingMatrix: Matrix,
    pdFont: PDFont,
    code: Int,
    unicode: String,
    displacement: Vector
  ): Unit = {
    val isSpace = code == 32 && unicode.head == ' '


    if (!isSpace) {

      super.showGlyph(textRenderingMatrix, pdFont, code, unicode, displacement)

      val glyphProps = calculateGlyphBounds(textRenderingMatrix, pdFont, code)

      fontDefs.addFont(pdFont, glyphProps)

      glyphProps.finalGlyphBounds.foreach { finalGlyphBounds =>
        val glyphShape = transformShapeOnPage(finalGlyphBounds)
        val pdFontShape = transformShapeOnPage(glyphProps.finalFontBounds)

        val charBounds = glyphShape.getBounds2D().toLTBounds
        val charFontBounds = pdFontShape.getBounds2D().toLTBounds

        def appendChar(strRepr: String): Unit = {
          strRepr.foreach { c =>
            fontDefs.addMetricEvidence(pdFont, c, charBounds, glyphProps)
          }
          addCharItem(CharItem(charIdGen.nextId, charBounds, charFontBounds, strRepr, getFontName(pdFont)))
        }

        if (unicode == null) {
          appendChar(s"¿${code};")
        } else {

          unicode.foreach { ch =>
            UnicodeUtil.maybeSubChar(ch) match {
              case Left(c)   => if (isCombiningMark(c)) stashChar(c) else appendChar(c.toString())
              case Right(cs) => cs.foreach{ c => if (isCombiningMark(c)) stashChar(c) else appendChar(c.toString()) }
            }
          }

        }
      }
    }
  }


  // def  calculateGlyphBounds(textRenderingMatrix: Matrix,  font: PDFont,  code: Int) : (Shape, Shape, Option[String]) = {
  def  calculateGlyphBounds(textRenderingMatrix: Matrix,  font: PDFont,  code: Int) : GlyphProps = {

    val finalRenderMatrix = textRenderingMatrix.clone()
    finalRenderMatrix.concatenate(font.getFontMatrix)
    val affineTr = finalRenderMatrix.createAffineTransform()

    def trans(p: GeneralPath): Shape = {
      affineTr.createTransformedShape(p.getBounds2D)
    }


    var initGlyphProps = GlyphProps(
      None,
      trans(getFontBounds(font).toGeneralPath()),
      affineTr
    )

    if (font.isInstanceOf[PDType3Font]) {

      val t3Font : PDType3Font =  font.asInstanceOf[PDType3Font]
      val charProc : PDType3CharProc = t3Font.getCharProc(code)


      if (charProc != null) {
        val fontBBox : BoundingBox = t3Font.getBoundingBox()
        val glyphBBox : PDRectangle = charProc.getGlyphBBox()
        if (glyphBBox != null) {
          // PDFBOX-3850: glyph bbox could be larger than the font bbox
          glyphBBox.setLowerLeftX(Math.max(fontBBox.getLowerLeftX(), glyphBBox.getLowerLeftX()))
          glyphBBox.setUpperRightX(Math.min(fontBBox.getUpperRightX(), glyphBBox.getUpperRightX()))

          glyphBBox.setLowerLeftY(Math.max(fontBBox.getLowerLeftY(), glyphBBox.getLowerLeftY()))
          glyphBBox.setUpperRightY(Math.min(fontBBox.getUpperRightY(), glyphBBox.getUpperRightY()))

          val gpath = glyphBBox.toGeneralPath() // .getBounds2D
          initGlyphProps = initGlyphProps.copy(
            finalGlyphBounds = Some(trans(gpath))
          )
        }
      }
    } else if (font.isInstanceOf[PDVectorFont]) {


      if (font.isInstanceOf[PDTrueTypeFont]) {
        val ttFont: PDTrueTypeFont = font.asInstanceOf[PDTrueTypeFont]
        val unitsPerEm: Int= ttFont.getTrueTypeFont().getHeader().getUnitsPerEm()
        affineTr.scale(1000d / unitsPerEm, 1000d / unitsPerEm)
      }

      if (font.isInstanceOf[PDType0Font]) {
        val t0font : PDType0Font=  font.asInstanceOf[PDType0Font]
        if (t0font.getDescendantFont().isInstanceOf[PDCIDFontType2]) {
          val unitsPerEm: Int = t0font.getDescendantFont().asInstanceOf[PDCIDFontType2]
            .getTrueTypeFont()
            .getHeader()
            .getUnitsPerEm()
          affineTr.scale(1000d / unitsPerEm, 1000d / unitsPerEm)
        }
      }

      val gpath = font.asInstanceOf[PDVectorFont].getPath(code)

      initGlyphProps = initGlyphProps.copy(
        finalGlyphBounds = Some(trans(gpath))
      )
    } else if (font.isInstanceOf[PDSimpleFont]) {
      val simpleFont : PDSimpleFont = font.asInstanceOf[PDSimpleFont]

      // these two lines do not always work, e.g. for the TT fonts in file 032431.pdf
      // which is why PDVectorFont is tried first.
      val name: String = simpleFont.getEncoding().getName(code)

      val gpath = simpleFont.getPath(name)
      initGlyphProps = initGlyphProps.copy(
        finalGlyphBounds = Some(trans(gpath))
      )
    } else {
      println("Unknown font class: " + font.getClass())
    }

    initGlyphProps
  }


  def findPageSpaceTransforms(pdPage: PDPage): AffineTransform = {
    // flip y-axis
    val flipTr = new AffineTransform()
    // flipTr.translate(0, pdPage.getBBox().getHeight().toDouble)
    flipTr.translate(0, cropBox.getHeight().toDouble)
    flipTr.scale(1, -1)


    // page may be rotated
    val rotateTr = new AffineTransform()
    val rotation : Int = pdPage.getRotation()
    if (rotation != 0) {
      val mediaBox: PDRectangle = cropBox
      rotation match {
        case 90 =>
          rotateTr.translate(mediaBox.getHeight().toDouble, 0)
        case 270 =>
          rotateTr.translate(0, mediaBox.getWidth().toDouble)
        case 180 =>
          rotateTr.translate(mediaBox.getWidth().toDouble, mediaBox.getHeight().toDouble)
        case rot =>
          println(s"Page rotation of ${rot} encountered")
      }
      rotateTr.rotate(Math.toRadians(rotation.toDouble))
    }

    // cropbox
    val transTr = AffineTransform.getTranslateInstance(-cropBox.getLowerLeftX().toDouble, cropBox.getLowerLeftY().toDouble)

    flipTr.concatenate(rotateTr)
    flipTr.concatenate(transTr)

    flipTr
  }
  def stripPage(document: PDDocument, page: Int): Unit = {
    // val pdfRenderer : PDFRenderer= new PDFRenderer(document)
    // image = pdfRenderer.renderImage(page, SCALE)

    val pdPage : PDPage = document.getPage(page)

    pageSpaceTransform = findPageSpaceTransforms(pdPage)


    processPage(pdPage)

  }

  def getPageGeometry(): PageGeometry = {
    PageGeometry(pageNum, cropBox.toLTBounds)
  }
}

object PdfBoxExtractorMain {

  implicit class RicherRectangle2D(val self: Rectangle2D) extends AnyVal {
    def toLTBounds(): LTBounds = {
      val h = self.getHeight
      val w = self.getWidth
      val left = self.getMinX
      val top = self.getMinY
      LTBounds.Doubles(left, top, w, h)
    }
  }
  implicit class RicherPDRectangle(val self: PDRectangle) extends AnyVal {
    def toLTBounds(): LTBounds = {
      val l = self.getLowerLeftX
      // val bottom = self.getLowerLeftY
      val t = self.getUpperRightY
      val w = self.getWidth
      val h = self.getHeight
      LTBounds.Floats(l, t-h, w, h)
    }
  }

  var cropBox : PDRectangle = null

  def extractPages(stableId: String@@DocumentID, pdfPath: Path): (List[(Seq[ExtractedItem], PageGeometry)], FontDefs) = {
    var document: PDDocument = null
    val pages = mutable.ListBuffer[(Seq[ExtractedItem], PageGeometry)]()
    val charIdGen = IdGenerator[CharID]()

    var currPageGeometry: PageGeometry = null

    var fontDefs: FontDefs = null

    try {
      document = PDDocument.load(pdfPath.toIO)

      val numOfPages = document.getNumberOfPages
      fontDefs = new FontDefs(numOfPages)
      print(s"Extracting fonts: ")


      // for { page <- 0 until numOfPages } {
      //   print(s" ${page};")
      //   val pdfPage = document.getPage(page)
      //   val pageResources = pdfPage.getResources
      //   val pageFontNames = pageResources.getFontNames
      //   for { fontName <- pageFontNames.iterator().asScala } {
      //     val pdFont = pageResources.getFont(fontName)
      //     fontDefs.addFont(pdFont)
      //   }
      // }

      for { page <- 0 until numOfPages } {
        println(s"Extracting page ${page}")
        val pdfPage = document.getPage(page)

        cropBox = pdfPage.getMediaBox   // getBBox getArtBox getTrimBox getCropBox getBleedBox getMediaBox

        currPageGeometry = PageGeometry(
          PageNum(page),
          cropBox.toLTBounds()
        )


        val extractor = new PdfBoxTextExtractor(
          pdfPage,
          charIdGen,
          PageNum(page),
          fontDefs
        )

        extractor.stripPage(document, page)

        val items = extractor.getPageItems()

        (items, currPageGeometry)
        pages.append((items, currPageGeometry))
      }

    } catch {
      case t: Throwable =>
        println(s"Error: ${t.getMessage}")
        t.printStackTrace()
        pages.append((Seq(), currPageGeometry))

    } finally {
      document.close()
    }

    (pages.toList, fontDefs)

  }

  def main(args: Array[String]): Unit = {
    println("Hello")

    extractPages(DocumentID("dummy"), fs.pwd / RelPath(args(0)))

  }

}


// protected def printFontInfo(textRenderingMatrix: Matrix, font: PDFont, code: Int, unicode: String, displacement: Vector): Unit = {
//   import utils.Debugging._

//   val at: AffineTransform = textRenderingMatrix.createAffineTransform()
//   at.concatenate(font.getFontMatrix().createAffineTransform())

//   println("~"*40)

//   val fdesc = font.getFontDescriptor

//   log(fdesc.getStemH)
//   log( fdesc.getStemV   )
//   log( fdesc.getAscent  )
//   log( fdesc.getDescent )

//   log(code)
//   log(unicode)
//   log(displacement)

//   log( font.getType )
//   log( font.getName )
//   // log( font.isDamaged() )
//   log( font.getSubType )
//   // log( font.getCOSObject )
//   log( font.getSpaceWidth )
//   log( font.getFontMatrix )
//   log( font.getBoundingBox )
//   // log( font.getPositionVector(code) )
//   log( font.getFontDescriptor )
//   log( font.isVertical() )
//   log( font.isDamaged() )
//   log( font.isEmbedded() )
//   log( font.isStandard14() )

//   val bbox = font.getBoundingBox

//   val xmin = bbox.getLowerLeftX
//   val ymin = bbox.getLowerLeftY
//   val xmax = bbox.getUpperRightX
//   val ymax = bbox.getUpperRightY
//   val w = xmax - xmin
//   val h = ymax - ymin

//   // at.transform(bbox)
//   // val fontBBox = new PDRectangle((float) minX, (float) minY, (float) (maxX - minX), (float) (maxY - minY))
//   // val fontBBox = new PDRectangle(x, y, w, h)
//   val fontBBox = new PDRectangle(xmin, ymin, w, h)
//   val fontPath = fontBBox.toGeneralPath().getBounds2D

//   val pTrans = at.createTransformedShape(fontPath)

//   log (pTrans.getBounds2D)

// }
