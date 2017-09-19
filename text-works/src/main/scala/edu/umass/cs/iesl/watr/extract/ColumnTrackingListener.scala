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


sealed trait ExtractedItem {
  var charProps: CharBioProp = new CharBioProp.OutChar

  def id: Int@@CharID
  def bbox: LTBounds

  def strRepr(): String

}

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
    def strRepr(): String = char

    def modp[P <: CharBioProp](modf: P => CharBioProp): Unit = {
      charProps = modf(charProps.asInstanceOf[P])
    }

    def setp[P <: CharBioProp](modf: P => Unit): Unit = {
      modf(charProps.asInstanceOf[P])
    }
  }


  case class ImgItem(
    id: Int@@CharID,
    bbox: LTBounds
  ) extends ExtractedItem {
    def strRepr(): String = s"[image ${bbox.prettyPrint}]"

    charProps = new CharBioProp.OutChar {
      isImage = true
    }
  }

  case class PathItem(
    id: Int@@CharID,
    bbox: LTBounds
  ) extends ExtractedItem {
    def strRepr(): String = "[path]"
  }


  // case class BTEvent(
  //   id: Int@@CharID
  // ) extends ExtractedItem
}

sealed trait CharBioProp

object CharBioProp {

  class OutChar extends CharBioProp {
    var isImage: Boolean = false
    var isPath: Boolean =  false
  }

  class BegChar extends CharBioProp {
    var lineEndId: Int@@CharID = CharID(0)
    var prevLineBeginId: Int@@CharID = CharID(0)
    var nextLineBeginId: Int@@CharID = CharID(0)
    var alignedLeftToPrev: Boolean = false
    var alignedLeftToNext: Boolean = false

  }

  class InsChar extends CharBioProp {
  }

  class LastChar extends CharBioProp {
    var lineBeginId: Int@@CharID = CharID(0)
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

  var lastItem: ExtractedItem = null
  var lastLineStartItem: ExtractedItem = null

  var lineLists = List[List[ExtractedItem]]()

  def getPageItems(): Seq[ExtractedItem] = {
    val lineRows = lineLists.reverse.map{ rline =>
      val lineLast = rline.head
      rline.drop(1).foreach {c =>
        c.charProps = new CharBioProp.InsChar {}
      }
      val line = rline.reverse
      val lineFirst = line.head

      // Overwrite final Ins with BegChar
      lineFirst.charProps = new CharBioProp.BegChar {
        lineEndId = lineLast.id
      }
      lineLast.charProps = new CharBioProp.LastChar {
        lineBeginId = lineFirst.id
      }

      line
    }

    debugPrintPageItems(lineRows)

    val lines = lineRows.flatten

    lines  // ++ pageImages
  }

  private def debugPrintPageItems(rows: List[List[ExtractedItem]]): Unit = {
    val rowStrs = rows.map { row =>
      row.map(_.strRepr()).mkString
    }

    val pageStr = rowStrs.mkString("\n  ", "\n  ", "\n")

    println(pageStr)
  }

  def addImgItem(imgItem: ExtractedItem.ImgItem): Unit = {
    lineLists = List(imgItem) :: lineLists
    // pageImages = imgItem :: pageImages
    lastItem = imgItem
    lastLineStartItem = imgItem
  }


  def charIsLikelyLineChange(charAtom: ExtractedItem.CharItem): Boolean = {
    val charBounds = charAtom.bbox

    val lastWasImage = lastItem.isInstanceOf[ImgItem]

    // val isLargeLeftJump = charBounds.left <= lastLineStartItem.bbox.left

    val isLeftJump = (
      charBounds.left < lastItem.bbox.left)

    val isLeftOverstrike = (
      charBounds.left <= lastItem.bbox.left
        && charBounds.bottom > lastLineStartItem.bbox.bottom
        && charBounds.bottom < lastItem.bbox.bottom
    )


    // val isDownLeftJump = (
    //   charBounds.left < lastItem.bbox.left && charBounds.bottom > lastItem.bbox.bottom)

    val isUpRightJump = (
      charBounds.left > lastItem.bbox.left && charBounds.bottom < lastItem.bbox.top)

    // val alignsWithLastNewline = charBounds.left == lastLineBegin().bbox.left

    !isLeftOverstrike && {
      isLeftJump || isUpRightJump || lastWasImage
    }

  }



  def addCharItem(charAtom: ExtractedItem.CharItem): Unit = {



    if (lastItem==null) {
      lineLists = List(List(charAtom))
      lastLineStartItem = charAtom
    } else if (charIsLikelyLineChange(charAtom)) {
      lineLists = List(charAtom) :: lineLists
      lastLineStartItem = charAtom
    } else {
      val lastLine = lineLists.drop(1).headOption

      val lastLineBases: Seq[Int@@FloatRep] = 
        lastLine.toList.flatMap{ l => l.map(_.bbox.bottom) }

      val sharesBaseWithLastLine = lastLineBases.exists { _ == charAtom.bbox.bottom }
      if (sharesBaseWithLastLine) {
        val currLine = lineLists.head

        val joinedTwo = currLine ++ lastLine.get

        lineLists = (charAtom :: joinedTwo) :: lineLists.drop(2)

        lastLineStartItem = lineLists.head.last

      } else {
        lineLists = (charAtom :: lineLists.head) :: lineLists.tail
      }
    }
    lastItem = charAtom

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
        } else if (code.exists(_ < 32)) {

          computeTextBounds(charTri).map { charBounds =>
            val nextId = charIdGen.nextId

            val charAtom = CharItem(
              nextId,
              charBounds,
              "░",
              code
            )

            addCharItem(charAtom)

          }
        }
      }
    }
  }

  // import geometry.syntax._
  // import utils.{RelativeDirection => Dir}
  // import itextpdf.kernel.geom.{
  //   Subpath, IShape,
  //   Point => IPoint
  // }

  // def renderPath(renderInfo: PathRenderInfo): Unit = {
  //   val ctm = renderInfo.getCtm
  //   val pathTransVec = ctmToLTBoundsPdfSpace(ctm).toPoint(Dir.TopLeft)

  //   val path = renderInfo.getPath

  //   val waypoints = for {
  //     subPath <- path.getSubpaths.asScala : Seq[Subpath]
  //     ishape  <- subPath.getSegments.asScala:  Seq[IShape]
  //     ipoint   <- ishape.getBasePoints.asScala:  Seq[IPoint]
  //   } yield {
  //     val p = ipointToPoint(ipoint)
  //     val trans = p.translate(pathTransVec)
  //     val tInvert = invertPointSpace(trans)
  //     // val endPoint = pathCurrPt.translate(p )
  //     // pathCurrPt = endPoint
  //     // println(s"      waypoint: $trans /inv= ${tInvert}")
  //     tInvert
  //   }

  //   val xsort = waypoints.sortBy(_.x)
  //   val ysort = waypoints.sortBy(_.y)
  //   val xmin = xsort.head.x
  //   val xmax = xsort.last.x
  //   val ymin = ysort.head.y
  //   val ymax = ysort.last.y
  //   val bounds = LTBounds(xmin, ymin, xmax-xmin, ymax-ymin)

  //   // val pageRegion = PageRegion(
  //   //   StablePage(stableId, pageNum),
  //   //   bounds
  //   // )

  //   // currCharBuffer.append(
  //   //   PageItem.Path(
  //   //     pageRegion,
  //   //     waypoints
  //   //   )
  //   // )
  // }

  def renderImage(iri: ImageRenderInfo): Unit = {
    val ctm = iri.getImageCtm
    val imgBounds = ctmToLTBounds(ctm)

    addImgItem(ExtractedItem.ImgItem(
      charIdGen.nextId,
      imgBounds
    ))

  }
}
