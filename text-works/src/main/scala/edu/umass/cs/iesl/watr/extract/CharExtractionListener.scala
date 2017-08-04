package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.PdfPage
import geometry._

import scala.collection.mutable
import scala.collection.JavaConversions._

import _root_.com.itextpdf
import itextpdf.kernel.geom.{Vector => PVector, Subpath, IShape, Point => IPoint, Matrix}
import itextpdf.kernel.pdf.canvas.parser.listener.IEventListener
import itextpdf.kernel.pdf.canvas.parser.EventType
import itextpdf.kernel.pdf.canvas.parser.data._
import itextpdf.kernel.pdf.PdfReader

import fonts._
import utils.IdGenerator
import utils.EnrichNumerics._
import TypeTags._
import utils.ExactFloats._

import textboxing.{TextBoxing => TB}, TB._

object PdfPageObjectOutput {

  import com.itextpdf.kernel.pdf.tagging.IPdfStructElem

  import TB._

  def fmtelem(e: IPdfStructElem) = {
    s"""struct role = ${e.getRole}""".box
  }

  def renderElemLoc(elem: IPdfStructElem, l:Int=0): Box = {
    val path = renderElemPath(elem)
    val tree = renderElemTree(elem, 0)

    val pboxes = path.reverse.zipWithIndex.map({case (p, i) =>
      indent(i*3)("^" + p)
    })
    val pathBox = vcat(left)(pboxes.toList)

    val treeBox = indent(path.length*3)(
      tree
    )

    pathBox atop treeBox
  }

  def renderElemPath(elem: IPdfStructElem): Seq[Box] = {
    if (elem == null) Seq() else {
      fmtelem(elem) +: renderElemPath(elem.getParent)
    }
  }

  def renderElemTree(elem: IPdfStructElem, l:Int=0): Box = {
    val p = indent(l*3)(
      fmtelem(elem)
    )
    val ks = vcat(left)({
      val kids = elem.getKids
      if (kids!= null) {
        kids.toList.map(renderElemTree(_, l+1))
      } else {
        List()
      }
    })

    p atop ks
  }


}

case class GeometryTranslation(
  transX: (Double) => Double,
  transY: (Double) => Double
)
class CharExtractionListener(
  reader: PdfReader,
  stableId: String@@DocumentID,
  charsToDebug: Set[Int] = Set(),
  charIdGen: IdGenerator[CharID],
  currCharBuffer: mutable.ArrayBuffer[PageItem],
  pdfPage: PdfPage,
  pageNum: Int@@PageNum,
  pageGeometry: PageGeometry,
  geomTranslation:GeometryTranslation
) extends IEventListener {

  override def getSupportedEvents(): java.util.Set[EventType] ={
    Set(
      EventType.RENDER_TEXT,
      EventType.RENDER_PATH,
      EventType.CLIP_PATH_CHANGED,
      EventType.RENDER_IMAGE
    )
  }


  override def eventOccurred(data: IEventData,  eventType: EventType): Unit = {
    if (eventType.equals(EventType.RENDER_TEXT)) {
      val tri = data.asInstanceOf[TextRenderInfo]
      renderText(tri)

    } else if (eventType.equals(EventType.RENDER_PATH)) {
      val renderInfo = data.asInstanceOf[PathRenderInfo]
      renderPath(renderInfo)

    } else if (eventType.equals(EventType.RENDER_IMAGE)) {
      val tri = data.asInstanceOf[ImageRenderInfo]
      renderImage(tri)
    }
  }

  def fallbackRep(charTri: TextRenderInfo): (String, Option[Int]) = {
    val pdfString = charTri.getPdfString
    val valueBytes = pdfString.getValueBytes.map(Byte.byte2int(_))

    ((for (b <- valueBytes) yield s"¿$b;").mkString ->
      Option(valueBytes(0)))
  }


  val charWindow = mutable.MutableList[String]()
  var __triggerText = "pathways" // "ansferredinto100" // Start debug logging when this text is found
  var (__curr: Int, __start:Int, __enable:Boolean) = (0, 150, false)
  var __verbose:Boolean = false

  var (__len: Int, __skip:Int) = (100, 20)

  // Limit the # of chars that can be extracted per page to prevent pathological cases (e.g., embedded charts using symbol font-based dots)
  val MAX_EXTRACTED_CHARS_PER_PAGE = 10000
  var totalCharCount = 0

  def renderText(charTris: TextRenderInfo): Unit = {

    if (__enable) { __curr += 1 }

    for (charTri <- charTris.getCharacterRenderInfos) {
      totalCharCount += 1

      if (totalCharCount < MAX_EXTRACTED_CHARS_PER_PAGE) {
        if (__verbose) {
          println(s"renderText:charTri.mcid=${charTri.getMcid}")
        }

        val (stringRep: String, code: Option[Int]) = if (!charTri.getText.isEmpty) {
          if (__verbose) {
            println(s"   for text(): ${charTri.getText}")
          }
          val t = charTri.getText()
            .map{ c =>
              if (__verbose) {
                val maybsub = UnicodeUtil.maybeSubChar(c)
                println(s"    c=${c}, asint=${c.toInt}, maybeSub: ${maybsub.map(_.toInt)}")
              }
              UnicodeUtil.maybeSubChar(c).filter(_ > ' ').mkString
            }
            .mkString
          (t, None)
        } else {
          // TODO reinstate glyph hash lookups
          fallbackRep(charTri)
        }

        if (__enable) { charWindow += stringRep }

        if (!stringRep.isEmpty && !code.exists(_ <= 32)) {

          computeTextBounds(charTri).map { charBounds =>
            val nextId = charIdGen.nextId
            if (charBounds.width <= 0 || charBounds.height <= 0) {
              println(s"bad bbox: ${charBounds}")
              fonts.DocumentFontInfo.outputCharInfo(charTri, reader, force=true)
            }

            val charAtom = CharAtom(
              nextId,
              PageRegion(
                StablePage(stableId, pageNum),
                charBounds
              ),
              stringRep,
              code
            )

            currCharBuffer.append(charAtom)

            if (__enable) {
              if (!__triggerText.isEmpty()) {
                val currCharWindow = charWindow.takeRight(20).mkString
                if (currCharWindow.endsWith(__triggerText)) {
                  __start = __curr
                  __verbose = true
                }

              }
              if (__start <= __curr &&  __curr < __start + __len) {
                if (__verbose) {
                  println(s"@${__curr}: ${charAtom}: wonky=${charAtom.wonkyCharCode}")
                  println(s"""text near: ${charWindow.takeRight(20).mkString}""")
                }
              } else {
                __verbose = false
              }

            }
          }
        }
      }
    }
  }

  def computeTextBounds(charTri: TextRenderInfo): Option[LTBounds] = {
    val fontProgramEmbedded = charTri.getFont.getFontProgram

    val fontProgram = fontProgramEmbedded

    val fontMetrics = fontProgram.getFontMetrics

    val ascentStart = charTri.getAscentLine().getStartPoint()
    val descentStart = charTri.getDescentLine().getStartPoint()

    val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble

    val ascentY = ascentStart.get(PVector.I2).toDouble
    val descentY = descentStart.get(PVector.I2).toDouble

    var absCharBottom: Double = descentY
    var charHeight = ascentY - descentY

    if (charHeight < 0 ) {
      charHeight = - charHeight
      absCharBottom = ascentY
    }

    val charLeft = geomTranslation.transX(absoluteCharLeft)
    val charBottom = geomTranslation.transY(absCharBottom)

    var charWidth = charTri.getDescentLine().getLength().toDouble

    if (charWidth.toInt == 0) {
      // figure out the exact dimensions of this glyph...
      // In glyph space:

      val pdfString = charTri.getPdfString
      // val decoded = charTri.getFont.decode(pdfString)
      val bs = pdfString.getValueBytes.map(Byte.byte2int(_) & 0xFF)
      val glyphCode = bs(0)

      val fontBbox = fontMetrics.getBbox
      val glyphWidths = fontMetrics.getGlyphWidths

      if (glyphWidths!=null && glyphWidths.length > glyphCode) {
        charWidth = glyphWidths(glyphCode).toDouble
      } else {
        if (fontBbox != null) {
          val y0 = fontBbox(1)
          val y1 = fontBbox(3)
          charWidth = (y1 - y0).toDouble
        }
      }
    }




    if (charHeight.nan || charHeight.inf || charHeight.toInt==0) {
      None
    } else if (charWidth.nan || charWidth.inf || charWidth.toInt==0) {
      None
    } else {
      val charTop = charBottom - charHeight

      Some(LTBounds.Doubles(
        left=charLeft,
        top=charTop,
        width=charWidth,
        height=charHeight
      ))
    }
  }

  def ctmToLTBounds(ctm:  Matrix): LTBounds = {
    val x1 = ctm.get(6).toDouble
    val y1 = ctm.get(7).toDouble
    val x2 = x1 + ctm.get(0)
    val y2 = y1 + ctm.get(4)
    val w = x2 - x1
    val h = y2 - y1

    val left = geomTranslation.transX(x1)
    val top = geomTranslation.transY(y2)

    val width = math.max(w, 0.1)
    val height = math.max(h, 0.1)

    LTBounds.Doubles(left, top, width, height)
  }

  def ctmToLTBoundsPdfSpace(ctm:  Matrix): LTBounds = {
    val x1 = ctm.get(6).toDouble
    val y1 = ctm.get(7).toDouble
    val x2 = x1 + ctm.get(0)
    val y2 = y1 + ctm.get(4)
    val w = x2 - x1
    val h = y2 - y1

    val left = x1 //  geomTranslation.transX(x1)
    val top = y2 // geomTranslation.transY(y2)

    LTBounds.Doubles(left, top, w, h)
  }

  def ipointToPoint(p:  IPoint): Point = {
    // val x = geomTranslation.transX(p.x)
    // val y = geomTranslation.transY(p.y)
    Point.Doubles(p.x, p.y)
  }

  def invertPointSpace(p:  Point): Point = {
    val x = geomTranslation.transX(p.x.asDouble())
    val y = geomTranslation.transY(p.y.asDouble)
    Point.Doubles(x, y)
  }

  import geometry.syntax._
    import utils.{RelativeDirection => Dir}

  def renderPath(renderInfo: PathRenderInfo): Unit = {
    // val op = renderInfo.getOperation
    val ctm = renderInfo.getCtm
    // val pathBounds = ctmToLTBounds(ctm)
    val pathTransVec = ctmToLTBoundsPdfSpace(ctm).toPoint(Dir.TopLeft)

    // val startPoint = pathBounds.toPoint(Dir.TopLeft)

    // println(s"renderPath: ")

    // var pathCurrPt = startPoint //  ipointToPoint(path.getCurrentPoint)

    val path = renderInfo.getPath
    // println(s"  path: getCurrentPoint=${path.getCurrentPoint}")


    val waypoints = for {
      subPath <- path.getSubpaths : Seq[Subpath]
      ishape  <- subPath.getSegments: Seq[IShape]
      ipoint   <- ishape.getBasePoints: Seq[IPoint]
    } yield {
      val p = ipointToPoint(ipoint)
      val trans = p.translate(pathTransVec)
      val tInvert = invertPointSpace(trans)
      // val endPoint = pathCurrPt.translate(p )
      // pathCurrPt = endPoint
      // println(s"      waypoint: $trans /inv= ${tInvert}")
      tInvert
    }

    val xsort = waypoints.sortBy(_.x)
    val ysort = waypoints.sortBy(_.y)
    val xmin = xsort.head.x
    val xmax = xsort.last.x
    val ymin = ysort.head.y
    val ymax = ysort.last.y
    val bounds = LTBounds(xmin, ymin, xmax-xmin, ymax-ymin)

    val pageRegion = PageRegion(
      StablePage(stableId, pageNum),
      bounds
    )

    currCharBuffer.append(
      PageItem.Path(
        pageRegion,
        waypoints
      )
    )
  }

  def renderImage(iri: ImageRenderInfo): Unit = {

    val ctm = iri.getImageCtm
    val imgBounds = ctmToLTBounds(ctm)
    // val bimgWidth = iri.getImage.getBufferedImage.getWidth
    // val bimgHeight = iri.getImage.getBufferedImage.getHeight
    // val imgWidth = iri.getImage.getWidth
    // val imgHeight = iri.getImage.getHeight
    // println("ctm:")
    // println(ctm)
    // println(s"    >> ${imgBounds} page: ${pageNum}")
    // println(s"    >> img  w:${imgWidth} h:${imgHeight}")
    // println(s"    >> img bw:${bimgWidth} h:${bimgHeight}")
    // println()

    val pageRegion = PageRegion(
      StablePage(stableId, pageNum),
      imgBounds
    )

    // println(s"Image at ${pageRegion}")

    val imgRegion = PageItem.ImageAtom(pageRegion)

    currCharBuffer.append(imgRegion)
  }
}
