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
// import geometry.syntax._
import utils._
// import ExactFloats._



class CustomGraphicsStreamEngine(
  page: PDPage,
  charIdGen: IdGenerator[CharID],
  pageNum: Int@@PageNum
) extends PDFGraphicsStreamEngine(page) {
  import ExtractedItem._
  // Limit the # of chars that can be extracted per page to prevent pathological cases (e.g., embedded charts using symbol font-based dots)
  val MAX_EXTRACTED_CHARS_PER_PAGE = 10000
  var totalCharCount = 0

  var lastItem: ExtractedItem = null

  var runLists = List[List[ExtractedItem]]()

  def getPageItems(): Seq[ExtractedItem] = {
    var currCharRunId = 0
    def nextRunId() = {
      currCharRunId = currCharRunId + 1
      currCharRunId
    }

    val lineRows = runLists.reverse.map{ rline =>
      val runId = nextRunId()

      rline.foreach { item =>
        item.charProps.charRunId = runId
      }

      val line = rline.reverse

      line.head.charProps.isRunBegin = true
      line
    }

    // debugPrintPageItems(lineRows)

    lineRows.flatten
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
    runLists = List(item) :: runLists
    lastItem = item
  }

  def addImgItem(imgItem: ExtractedItem.ImgItem): Unit = {
    runLists = List(imgItem) :: runLists
    // pageImages = imgItem :: pageImages
    lastItem = imgItem
  }


  def addCharItem(charAtom: ExtractedItem.CharItem): Unit = {
    // println(s"addCharItem ${charAtom}: bottom=${charAtom.bbox.bottom.pp()}")
    if (lastItem==null || lastItem.bbox.bottom != charAtom.bbox.bottom) {
      runLists = List(charAtom) :: runLists
    } else {
      runLists = (charAtom :: runLists.head) :: runLists.tail
    }
    lastItem = charAtom
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
    new Point2D.Float(0, 0);
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
    super.showTextString(string);
  }

  var flipAT: AffineTransform = new AffineTransform();
  var rotateAT: AffineTransform = new AffineTransform();
  var transAT: AffineTransform = new AffineTransform();
  var g2d: Graphics2D =  null


  override protected def showGlyph(
    textRenderingMatrix: Matrix,
    font: PDFont,
    code: Int,
    unicode: String,
    displacement: Vector
  ): Unit = {
    super.showGlyph(textRenderingMatrix, font, code, unicode, displacement);
    var (cyanShape: Shape, strRepr) = calculateGlyphBounds(textRenderingMatrix, font, code);

    if (cyanShape != null) {
      cyanShape = flipAT.createTransformedShape(cyanShape);
      cyanShape = rotateAT.createTransformedShape(cyanShape);
      cyanShape = transAT.createTransformedShape(cyanShape);

      // g2d.setColor(Color.CYAN);
      // g2d.draw(cyanShape);
      val bounds = cyanShape.getBounds2D

      val h = bounds.getHeight
      val w = bounds.getWidth
      val left = bounds.getMinX
      val top = bounds.getMinY

      val charBounds = LTBounds.Doubles(left, top, w, h)
      val nextId = charIdGen.nextId

      val str = if (unicode != null) {
        unicode
      } else {
        strRepr.map{r =>
          s"(${r})"
        } getOrElse {
          "░"
        }

      }

      val charAtom = CharItem(
        nextId,
        charBounds,
        str
      )
      addCharItem(charAtom)
    }
  }


  // this calculates the real (except for type 3 fonts) individual glyph bounds
  def  calculateGlyphBounds(textRenderingMatrix: Matrix,  font: PDFont,  code: Int) : (Shape, Option[String]) = {
    var path : GeneralPath= null;
    var strRepr: Option[String] = None

    val at: AffineTransform = textRenderingMatrix.createAffineTransform();

    at.concatenate(font.getFontMatrix().createAffineTransform());

    if (font.isInstanceOf[PDType3Font]) {
      // It is difficult to calculate the real individual glyph bounds for type 3 fonts
      // because these are not vector fonts, the content stream could contain almost anything
      // that is found in page content streams.
      val t3Font : PDType3Font =  font.asInstanceOf[PDType3Font];
      val charProc : PDType3CharProc = t3Font.getCharProc(code);
      if (charProc != null) {
        val fontBBox : BoundingBox = t3Font.getBoundingBox();
        val glyphBBox : PDRectangle = charProc.getGlyphBBox();
        if (glyphBBox != null) {
          // PDFBOX-3850: glyph bbox could be larger than the font bbox
          glyphBBox.setLowerLeftX(Math.max(fontBBox.getLowerLeftX(), glyphBBox.getLowerLeftX()));
          glyphBBox.setUpperRightX(Math.min(fontBBox.getUpperRightX(), glyphBBox.getUpperRightX()));

          glyphBBox.setLowerLeftY(Math.max(fontBBox.getLowerLeftY(), glyphBBox.getLowerLeftY()));
          glyphBBox.setUpperRightY(Math.min(fontBBox.getUpperRightY(), glyphBBox.getUpperRightY()));

          // glyphBBox.setLowerLeftY(fontBBox.getLowerLeftY());
          // glyphBBox.setUpperRightY(fontBBox.getUpperRightY());
          // glyphBBox.setLowerLeftY(glyphBBox.getLowerLeftY());
          // glyphBBox.setUpperRightY(glyphBBox.getUpperRightY());

          path = glyphBBox.toGeneralPath();
        }
      } else {
        strRepr = Some(s"t3font:${code}")
      }
    } else if (font.isInstanceOf[PDVectorFont]) {


      val vectorFont : PDVectorFont =  font.asInstanceOf[PDVectorFont]

      path = vectorFont.getPath(code);

      if (font.isInstanceOf[PDTrueTypeFont]) {
        val ttFont : PDTrueTypeFont = font.asInstanceOf[PDTrueTypeFont];
        val unitsPerEm : Int= ttFont.getTrueTypeFont().getHeader().getUnitsPerEm();
        at.scale(1000d / unitsPerEm, 1000d / unitsPerEm);
      }

      if (font.isInstanceOf[PDType0Font]) {
        val t0font : PDType0Font=  font.asInstanceOf[PDType0Font];
        if (t0font.getDescendantFont(). isInstanceOf[PDCIDFontType2]) {
          val unitsPerEm: Int = t0font.getDescendantFont().asInstanceOf[PDCIDFontType2]
            .getTrueTypeFont()
            .getHeader()
            .getUnitsPerEm();
          at.scale(1000d / unitsPerEm, 1000d / unitsPerEm);
        }
      }
    } else if (font.isInstanceOf[PDSimpleFont]) {
      val simpleFont : PDSimpleFont = font.asInstanceOf[PDSimpleFont];

      // these two lines do not always work, e.g. for the TT fonts in file 032431.pdf
      // which is why PDVectorFont is tried first.
      val name: String = simpleFont.getEncoding().getName(code);
      simpleFont.getType
      // simpleFont.getPath(x$1: String)

      // path = simpleFont.getBoundingBox()
      path = simpleFont.getPath(name);
      strRepr = Some(s"simpleFont:${code}/${name}")
    } else {
      // shouldn't happen, please open issue in JIRA
      System.out.println("Unknown font class: " + font.getClass());
    }
    if (path == null) {
       null
    } else {
      (at.createTransformedShape(path.getBounds2D()), strRepr)
    }
  }

  var cropBox : PDRectangle = null

  def stripPage(document: PDDocument, page: Int): Unit = {
    // val pdfRenderer : PDFRenderer= new PDFRenderer(document);
    // image = pdfRenderer.renderImage(page, SCALE);

    val pdPage : PDPage = document.getPage(page);
    cropBox  = pdPage.getCropBox()

    // flip y-axis
    flipAT = new AffineTransform();
    flipAT.translate(0, pdPage.getBBox().getHeight().toDouble);
    flipAT.scale(1, -1);

    // page may be rotated
    rotateAT = new AffineTransform();
    val rotation : Int = pdPage.getRotation();
    if (rotation != 0) {
      val mediaBox : PDRectangle = pdPage.getMediaBox();
      rotation match {
        case 90 =>
          rotateAT.translate(mediaBox.getHeight().toDouble, 0);
        case 270 =>
          rotateAT.translate(0, mediaBox.getWidth().toDouble);
        case 180 =>
          rotateAT.translate(mediaBox.getWidth().toDouble, mediaBox.getHeight().toDouble);
        case _ =>
      }
      rotateAT.rotate(Math.toRadians(rotation.toDouble));
    }

    // cropbox
    transAT = AffineTransform.getTranslateInstance(-cropBox.getLowerLeftX().toDouble, cropBox.getLowerLeftY().toDouble);

    // g2d = image.createGraphics();
    // g2d.setStroke(new BasicStroke(0.1f));
    // g2d.scale(SCALE, SCALE);


    // setStartPage(page + 1);
    // setEndPage(page + 1);

    // Writer dummy = new OutputStreamWriter(new ByteArrayOutputStream());
    // writeText(document, dummy);
    processPage(pdPage)

    // beads in green
    // g2d.setStroke(new BasicStroke(0.4f));
    // List<PDThreadBead> pageArticles = pdPage.getThreadBeads();
    // for (PDThreadBead bead : pageArticles)
    // {
    //     PDRectangle r = bead.getRectangle();
    //     Shape s = r.toGeneralPath().createTransformedShape(transAT);
    //     s = flipAT.createTransformedShape(s);
    //     s = rotateAT.createTransformedShape(s);
    //     g2d.setColor(Color.green);
    //     g2d.draw(s);
    // }

    // g2d.dispose();

    // String imageFilename = filename;
    // int pt = imageFilename.lastIndexOf('.');
    // imageFilename = imageFilename.substring(0, pt) + "-marked-" + (page + 1) + ".png";
    // ImageIO.write(image, "png", new File(imageFilename));
  }
  import PdfBoxExtractorMain._

  def getPageGeometry(): PageGeometry = {
    // val bounds = cropBox
    // val x = bounds.getLowerLeftX
    // val y = bounds.getUpperRightY
    // val w = bounds.getWidth
    // val h = bounds.getHeight
    // PageGeometry(pageNum, LTBounds.Floats(x, y, w, h))
    PageGeometry(pageNum, cropBox.toLTBounds)
  }
}

object PdfBoxExtractorMain {

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

        currPageGeometry = PageGeometry(
          PageNum(page),
          pdfPage.getCropBox.toLTBounds()
        )

        // utils.Debugging.log(pdfPage.getBBox().toLTBounds())
        // utils.Debugging.log(pdfPage.getArtBox.toLTBounds)
        // utils.Debugging.log(pdfPage.getTrimBox.toLTBounds)
        // utils.Debugging.log(pdfPage.getCropBox.toLTBounds)
        // utils.Debugging.log(pdfPage.getBleedBox.toLTBounds)
        // utils.Debugging.log(pdfPage.getMediaBox.toLTBounds)

        val extractor = new CustomGraphicsStreamEngine(
          pdfPage,
          charIdGen,
          PageNum(page)
        )
        extractor.stripPage(document, page)

        val items = extractor.getPageItems()
        // val pageGeometry = extractor.getPageGeometry()
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
