package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.PdfPage
import geometry._

import scala.collection.JavaConverters._

import _root_.com.itextpdf
import itextpdf.kernel.pdf.canvas.parser.listener.IEventListener
import itextpdf.kernel.pdf.canvas.parser.EventType
import itextpdf.kernel.pdf.canvas.parser.data._
import itextpdf.kernel.pdf.PdfReader

import fonts._
import utils.IdGenerator
import utils.ExactFloats._
import TypeTags._
// import watrmarks.{StandardLabels => LB}


sealed trait ExtractedItem

object ExtractedItem {
  implicit class RicherExtractedItem(val self: CharItem) extends AnyVal {


    def isWonky: Boolean = self.wonkyCharCode.isDefined

    def isSpace: Boolean = self.wonkyCharCode.exists(_==32)
    def isControl: Boolean = self.wonkyCharCode.exists(_<32)
    def isNonPrintable: Boolean = self.wonkyCharCode.exists(_<=32)

  }

  case class CharItem(
    id: Int@@CharID,
    bbox: LTBounds,
    char: String,
    wonkyCharCode: Option[Int] = None
  ) extends ExtractedItem {
    var charProps: CharBioProp = CharBioProp.OutChar

    def modp[P <: CharBioProp](modf: P => CharBioProp): Unit = {
      charProps = modf(charProps.asInstanceOf[P])
    }

    def setp[P <: CharBioProp](modf: P => Unit): Unit = {
      modf(charProps.asInstanceOf[P])
    }
  }


  case class ImgItem(
    bbox: LTBounds
  ) extends ExtractedItem

  case class PathItem(
    bbox: LTBounds
  ) extends ExtractedItem

}

sealed trait CharBioProp

object CharBioProp {

  object OutChar extends CharBioProp

  class BegChar extends CharBioProp {
    var prevLineBeginId: Int@@CharID = CharID(0)
    var nextLineBeginId: Int@@CharID = CharID(0)
    var alignedLeftToPrev: Boolean = false
    var alignedLeftToNext: Boolean = false
  }

  class InsChar extends CharBioProp {
  }

  class LastChar extends CharBioProp {
    var prevLineBeginId: Int@@CharID = CharID(0)

  }

}

class ColumnTrackingListener(
  reader: PdfReader,
  charIdGen: IdGenerator[CharID],
  pdfPage: PdfPage,
  pageNum: Int@@PageNum,
  pageGeometry: PageGeometry,
  geomTranslation: GeometryTranslation
) extends  ExtractionBasics (geomTranslation) with IEventListener {

  import ExtractedItem._


  override def getSupportedEvents(): java.util.Set[EventType] ={
    Set(
      EventType.RENDER_TEXT,
      // EventType.RENDER_PATH,
      // EventType.CLIP_PATH_CHANGED,
      EventType.RENDER_IMAGE
    ).asJava
  }


  override def eventOccurred(data: IEventData,  eventType: EventType): Unit = {
    if (eventType.equals(EventType.RENDER_TEXT)) {
      val tri = data.asInstanceOf[TextRenderInfo]
      renderText(tri)

    } else if (eventType.equals(EventType.RENDER_PATH)) {
      // val renderInfo = data.asInstanceOf[PathRenderInfo]
      // renderPath(renderInfo)

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


  // Limit the # of chars that can be extracted per page to prevent pathological cases (e.g., embedded charts using symbol font-based dots)
  val MAX_EXTRACTED_CHARS_PER_PAGE = 10000
  var totalCharCount = 0

  var extractedItems = List[ExtractedItem]()
  var lineLists = List[List[CharItem]]()
  var newLineCandidates = List[CharItem]()

  def getPageItems(): Seq[ExtractedItem] = {
    extractedItems.reverse
  }

  var lastChar: CharItem = null

  def lastLineBegin(): CharItem = {
    newLineCandidates.head
  }

  def addImgItem(imgItem: ExtractedItem.ImgItem): Unit = {
    extractedItems = imgItem :: extractedItems
  }
  def addCharItem(charAtom: ExtractedItem.CharItem): Unit = {
    val charBounds = charAtom.bbox

    extractedItems = charAtom :: extractedItems

    if (lastChar==null) {
      charAtom.charProps = new CharBioProp.BegChar()

      lineLists = List(List(charAtom))
      newLineCandidates = charAtom :: newLineCandidates

    } else {

      val isDownLeftJump = (
        charBounds.left < lastChar.bbox.left && charBounds.bottom > lastChar.bbox.bottom)

      val isUpRightJump = (
        charBounds.left > lastChar.bbox.left && charBounds.bottom < lastChar.bbox.top)

      val alignsWithLastNewline = charBounds.left == lastLineBegin().bbox.left

      if (isDownLeftJump) {
        lastChar.charProps = new CharBioProp.LastChar {
          prevLineBeginId = lastLineBegin.id
        }

        if (alignsWithLastNewline) {
          charAtom.charProps = new CharBioProp.BegChar {
            alignedLeftToPrev = true
            prevLineBeginId = lastLineBegin().id
          }

          lastLineBegin.setp[CharBioProp.BegChar]{p =>
            p.alignedLeftToNext = true
            p.nextLineBeginId = charAtom.id
          }

        } else {
          charAtom.charProps = new CharBioProp.BegChar {
            alignedLeftToPrev = false
            prevLineBeginId = lastLineBegin().id
          }
        }

        lineLists = List(charAtom) :: lineLists
        newLineCandidates = charAtom :: newLineCandidates
      } else if (isUpRightJump) {

        lastChar.charProps = new CharBioProp.LastChar {
          prevLineBeginId = lastLineBegin.id
        }

        lineLists = List(charAtom) :: lineLists
        newLineCandidates = charAtom :: newLineCandidates

        charAtom.charProps = new CharBioProp.BegChar {
          alignedLeftToPrev = false
          prevLineBeginId = lastLineBegin().id
        }

        lastLineBegin.setp[CharBioProp.BegChar]{p =>
          p.alignedLeftToNext = false
          p.nextLineBeginId = charAtom.id
        }

      } else {
        charAtom.charProps = new CharBioProp.InsChar()
        lineLists = (charAtom +: lineLists.head) :: lineLists.tail
      }
    }

    lastChar = charAtom

  }

  def renderText(charTris: TextRenderInfo): Unit = {

    for (charTri <- charTris.getCharacterRenderInfos.asScala) {
      totalCharCount += 1

      if (totalCharCount < MAX_EXTRACTED_CHARS_PER_PAGE) {

        val (stringRep: String, code: Option[Int]) = if (!charTri.getText.isEmpty) {
          val t = charTri.getText()
            .map{ c =>
              UnicodeUtil.maybeSubChar(c).filter(_ > ' ').mkString
            }
            .mkString
          (t, None)
        } else {
          fallbackRep(charTri)
        }


        if (!stringRep.isEmpty && !code.exists(_ <= 32)) {

          computeTextBounds(charTri).map { charBounds =>
            val nextId = charIdGen.nextId

            val charAtom = CharItem(
              nextId,
              charBounds,
              stringRep,
              code
            )

            addCharItem(charAtom)

          }
        }
      }
    }
  }

  import geometry.syntax._
  import utils.{RelativeDirection => Dir}
  import itextpdf.kernel.geom.{
    Subpath, IShape,
    Point => IPoint
  }

  def renderPath(renderInfo: PathRenderInfo): Unit = {
    val ctm = renderInfo.getCtm
    val pathTransVec = ctmToLTBoundsPdfSpace(ctm).toPoint(Dir.TopLeft)

    val path = renderInfo.getPath

    val waypoints = for {
      subPath <- path.getSubpaths.asScala : Seq[Subpath]
      ishape  <- subPath.getSegments.asScala:  Seq[IShape]
      ipoint   <- ishape.getBasePoints.asScala:  Seq[IPoint]
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

    // val pageRegion = PageRegion(
    //   StablePage(stableId, pageNum),
    //   bounds
    // )

    // currCharBuffer.append(
    //   PageItem.Path(
    //     pageRegion,
    //     waypoints
    //   )
    // )
  }

  def renderImage(iri: ImageRenderInfo): Unit = {
    val ctm = iri.getImageCtm
    val imgBounds = ctmToLTBounds(ctm)

    val item = ExtractedItem.ImgItem(imgBounds)
    extractedItems = item :: extractedItems

  }
}
