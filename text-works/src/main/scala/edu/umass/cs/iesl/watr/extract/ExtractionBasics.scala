package edu.umass.cs.iesl.watr
package extract

import _root_.com.itextpdf

import itextpdf.kernel.geom.{
  Vector => PVector,
  Point => IPoint,
  Matrix
}

import itextpdf.kernel.pdf.canvas.parser.data._
import utils.EnrichNumerics._
import utils.ExactFloats._
import geometry._
import geometry.syntax._

abstract class ExtractionBasics(
  geomTranslation: GeometryTranslation
) {

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

}

sealed trait ExtractedItem {
  val charProps: CharProps = new CharProps {}

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

    // def modp[P <: CharBioProp](modf: P => CharBioProp): Unit = {
    //   charProps = modf(charProps.asInstanceOf[P])
    // }

    // def setp[P <: CharBioProp](modf: P => Unit): Unit = {
    //   modf(charProps.asInstanceOf[P])
    // }
  }


  case class ImgItem(
    id: Int@@CharID,
    bbox: LTBounds
  ) extends ExtractedItem {
    def strRepr(): String = s"[image ${bbox.prettyPrint}]"
    // charProps = new CharBioProp.OutChar {
    //   isImage = true
    // }
  }

  case class PathItem(
    id: Int@@CharID,
    bbox: LTBounds,
    waypoints: Seq[Point]
    // waypoints: Seq[Seq[Seq[Point]]]
  ) extends ExtractedItem {

    lazy val pp = waypoints.map(_.prettyPrint).take(4).mkString(", ")
    def strRepr(): String = s"[path ${bbox.prettyPrint}=[$pp]]"
  }


}



class CharProps()  {
  var charRunId: Int = -1
  var isRunBegin: Boolean = false
}

  // sealed trait CharClass
  // object CharClass {
  //   final case object ImageClass extends CharClass
  // }

// sealed trait CharBioProp
// object CharBioProp {


//   // class OutChar extends CharBioProp {
//   //   var isImage: Boolean = false
//   //   var isPath: Boolean =  false
//   // }

//   // class BegChar extends CharBioProp {
//   //   // var lineEndId: Int@@CharID = CharID(0)
//   //   // var prevLineBeginId: Int@@CharID = CharID(0)
//   //   // var nextLineBeginId: Int@@CharID = CharID(0)
//   //   // var alignedLeftToPrev: Boolean = false
//   //   // var alignedLeftToNext: Boolean = false

//   // }

//   // class InsChar extends CharBioProp {
//   // }

//   // class LastChar extends CharBioProp {
//   //   // var lineBeginId: Int@@CharID = CharID(0)
//   //   // var prevLineBeginId: Int@@CharID = CharID(0)

//   // }

// }
