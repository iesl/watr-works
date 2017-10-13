package edu.umass.cs.iesl.watr
package extract

import java.awt.{Graphics2D, Shape}
import java.awt.geom._

import scala.collection.mutable

import ammonite.{ops => fs}
import ammonite.ops._
import org.apache.fontbox.util.BoundingBox
import org.apache.pdfbox.contentstream._
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font._
import org.apache.pdfbox.util.{Matrix, Vector}

import TypeTags._
import geometry._
import utils._


class PdfBoxTextExtractor(
  page: PDPage,
  charIdGen: IdGenerator[CharID],
  pageNum: Int@@PageNum
) extends PDFGraphicsStreamEngine(page) {
  import ExtractedItem._
  import PdfBoxExtractorMain._
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
    extractedItems = item :: extractedItems
  }

  def addCharItem(charAtom: ExtractedItem.CharItem): Unit = {
    // println(s"addCharItem ${charAtom}: bottom=${charAtom.bbox.bottom.pp()} / ${charAtom.fontBbox.bottom.pp }")
    extractedItems = charAtom :: extractedItems
  }

  def stashChar(c: Char): Unit = {
    // addCharItem(CharItem(charIdGen.nextId, charBounds, c))
  }

  override def appendRectangle(x1: Point2D,x2: Point2D,x3: Point2D,x4: Point2D): Unit = {
  }

  override def clip(x1: Int): Unit = {
  }

  override def closePath(): Unit = {
  }

  override def curveTo(x1: Float,x2: Float,x3: Float,x4: Float,x5: Float,x6: Float): Unit = {
  }

  override def drawImage(x1: org.apache.pdfbox.pdmodel.graphics.image.PDImage): Unit = {
  }

  override def endPath(): Unit = {
  }

  override def fillAndStrokePath(x1: Int): Unit = {
  }

  override def fillPath(x1: Int): Unit = {
  }

  override def getCurrentPoint(): Point2D = {
    //     // if you want to build paths, you'll need to keep track of this like PageDrawer does
    new Point2D.Float(0, 0)
  }

  override def lineTo(x1: Float,x2: Float): Unit = {
  }

  override def moveTo(x1: Float,x2: Float): Unit = {
  }

  override def shadingFill(x1: org.apache.pdfbox.cos.COSName): Unit = {
  }

  override def strokePath(): Unit = {
  }


  override def showTextString(string: Array[Byte]): Unit = {
    super.showTextString(string)
  }

  var flipAT: AffineTransform = new AffineTransform()
  var rotateAT: AffineTransform = new AffineTransform()
  var transAT: AffineTransform = new AffineTransform()
  var g2d: Graphics2D =  null

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

  protected def printFontInfo(textRenderingMatrix: Matrix,
    font: PDFont,
    code: Int,
    unicode: String,
    displacement: Vector
  ): Unit = {

    import utils.Debugging._

    val at: AffineTransform = textRenderingMatrix.createAffineTransform()
    at.concatenate(font.getFontMatrix().createAffineTransform())



    println("~"*40)

    val fdesc = font.getFontDescriptor

    log(fdesc.getStemH)
    log( fdesc.getStemV   )
    log( fdesc.getAscent  )
    log( fdesc.getDescent )

    log(code)
    log(unicode)
    log(displacement)

    log( font.getType )
    log( font.getName )
    // log( font.isDamaged() )
    log( font.getSubType )
    // log( font.getCOSObject )
    log( font.getSpaceWidth )
    log( font.getFontMatrix )
    log( font.getBoundingBox )
    // log( font.getPositionVector(code) )
    log( font.getFontDescriptor )
    log( font.isVertical() )
    log( font.isDamaged() )
    log( font.isEmbedded() )
    log( font.isStandard14() )

    val bbox = font.getBoundingBox

    val xmin = bbox.getLowerLeftX
    val ymin = bbox.getLowerLeftY
    val xmax = bbox.getUpperRightX
    val ymax = bbox.getUpperRightY
    val w = xmax - xmin
    val h = ymax - ymin

    // at.transform(bbox)
    // val fontBBox = new PDRectangle((float) minX, (float) minY, (float) (maxX - minX), (float) (maxY - minY))
    // val fontBBox = new PDRectangle(x, y, w, h)
    val fontBBox = new PDRectangle(xmin, ymin, w, h)
    val fontPath = fontBBox.toGeneralPath().getBounds2D

    val pTrans = at.createTransformedShape(fontPath)

    log (pTrans.getBounds2D)

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

  private def transformShapeOnPage(shape: Shape): Shape = {
    var sh = shape
    sh = flipAT.createTransformedShape(sh)
    sh = rotateAT.createTransformedShape(sh)
    transAT.createTransformedShape(sh)
  }

  override protected def showGlyph(
    textRenderingMatrix: Matrix,
    font: PDFont,
    code: Int,
    unicode: String,
    displacement: Vector
  ): Unit = {
    super.showGlyph(textRenderingMatrix, font, code, unicode, displacement)

    // getFontBounds(font)

    var (glyphShape, fontShape, strRepr) = calculateGlyphBounds(textRenderingMatrix, font, code)

    if (glyphShape != null) {
      glyphShape = transformShapeOnPage(glyphShape)
      fontShape = transformShapeOnPage(fontShape)

      val charBounds = glyphShape.getBounds2D().toLTBounds
      val charFontBounds = fontShape.getBounds2D().toLTBounds

      def isSpace() = code == 32 && unicode.head == ' '

      def appendChar(c: Char): Unit = {
        addCharItem(CharItem(charIdGen.nextId, charBounds, charFontBounds, c.toString))
      }

      if (unicode == null) {
        s"¿${code}".foreach { appendChar(_) }
      } else {

        unicode.foreach { ch =>
          if (!isSpace()) {
            UnicodeUtil.maybeSubChar(ch) match {
              case Left(c)  =>
                if (isCombiningMark(c)) stashChar(c) else appendChar(c)

              case Right(cs) =>
                cs.foreach{ c =>
                  if (isCombiningMark(c)) stashChar(c) else appendChar(c)
                }
            }

          }
        }
      }
    }
  }


  def  calculateGlyphBounds(textRenderingMatrix: Matrix,  font: PDFont,  code: Int) : (Shape, Shape, Option[String]) = {
    var path : GeneralPath= null
    var strRepr: Option[String] = None

    val at: AffineTransform = textRenderingMatrix.createAffineTransform()

    at.concatenate(font.getFontMatrix().createAffineTransform())

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

          path = glyphBBox.toGeneralPath()
        }
      } else {
        strRepr = Some(s"t3font:${code}")
      }
    } else if (font.isInstanceOf[PDVectorFont]) {


      val vectorFont : PDVectorFont =  font.asInstanceOf[PDVectorFont]

      path = vectorFont.getPath(code)

      if (font.isInstanceOf[PDTrueTypeFont]) {
        val ttFont: PDTrueTypeFont = font.asInstanceOf[PDTrueTypeFont]
        val unitsPerEm: Int= ttFont.getTrueTypeFont().getHeader().getUnitsPerEm()
        at.scale(1000d / unitsPerEm, 1000d / unitsPerEm)
      }

      if (font.isInstanceOf[PDType0Font]) {
        val t0font : PDType0Font=  font.asInstanceOf[PDType0Font]
        if (t0font.getDescendantFont().isInstanceOf[PDCIDFontType2]) {
          val unitsPerEm: Int = t0font.getDescendantFont().asInstanceOf[PDCIDFontType2]
            .getTrueTypeFont()
            .getHeader()
            .getUnitsPerEm()
          at.scale(1000d / unitsPerEm, 1000d / unitsPerEm)
        }
      }
    } else if (font.isInstanceOf[PDSimpleFont]) {
      val simpleFont : PDSimpleFont = font.asInstanceOf[PDSimpleFont]

      // these two lines do not always work, e.g. for the TT fonts in file 032431.pdf
      // which is why PDVectorFont is tried first.
      val name: String = simpleFont.getEncoding().getName(code)

      // path = simpleFont.getBoundingBox()
      path = simpleFont.getPath(name)
      strRepr = Some(s"simpleFont:${code}/${name}")
    } else {
      println("Unknown font class: " + font.getClass())
    }

    val fontBounds = getFontBounds(font)
    val fontBoundsTrans = at.createTransformedShape(fontBounds.toGeneralPath().getBounds2D)

    if (path == null) {
      (null, fontBoundsTrans, strRepr)
    } else {
      val glyphBoundsTrans = at.createTransformedShape(path.getBounds2D())

      (glyphBoundsTrans, fontBoundsTrans, strRepr)
    }
  }


  def stripPage(document: PDDocument, page: Int): Unit = {
    // val pdfRenderer : PDFRenderer= new PDFRenderer(document)
    // image = pdfRenderer.renderImage(page, SCALE)

    val pdPage : PDPage = document.getPage(page)

    // flip y-axis
    flipAT = new AffineTransform()
    flipAT.translate(0, pdPage.getBBox().getHeight().toDouble)
    flipAT.scale(1, -1)

    // page may be rotated
    rotateAT = new AffineTransform()
    val rotation : Int = pdPage.getRotation()
    if (rotation != 0) {
      val mediaBox : PDRectangle = pdPage.getMediaBox()
      rotation match {
        case 90 =>
          rotateAT.translate(mediaBox.getHeight().toDouble, 0)
        case 270 =>
          rotateAT.translate(0, mediaBox.getWidth().toDouble)
        case 180 =>
          rotateAT.translate(mediaBox.getWidth().toDouble, mediaBox.getHeight().toDouble)
        case _ =>
      }
      rotateAT.rotate(Math.toRadians(rotation.toDouble))
    }

    // cropbox
    transAT = AffineTransform.getTranslateInstance(-cropBox.getLowerLeftX().toDouble, cropBox.getLowerLeftY().toDouble)

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

  def extractPages(stableId: String@@DocumentID, pdfPath: Path): List[(Seq[ExtractedItem], PageGeometry)] = {
    var document: PDDocument = null
    val pages = mutable.ListBuffer[(Seq[ExtractedItem], PageGeometry)]()
    val charIdGen = IdGenerator[CharID]()

    var currPageGeometry: PageGeometry = null

    try {
      document = PDDocument.load(pdfPath.toIO)

      val numOfPages = document.getNumberOfPages

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
          PageNum(page)
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

    pages.toList

  }

  def main(args: Array[String]): Unit = {
    println("Hello")

    extractPages(DocumentID("dummy"), fs.pwd / RelPath(args(0)))

  }

}
