package edu.umass.cs.iesl.watr
package extract

import geometry._
import geometry.syntax._
import utils.{RelativeDirection => Dir}
import scala.collection.mutable
import org.apache.pdfbox.pdmodel.font._
import utils.ExactFloats._
import java.awt.{Shape}
import java.awt.geom._
import org.apache.pdfbox.pdmodel.common.PDRectangle

import com.google.{common => guava}
import guava.{collect => gcol}
import TypeTags._

protected object ExtractionImplicits {
  implicit class RicherRectangle2D(val self: Rectangle2D) extends AnyVal {
    def toLTBounds(): LTBounds = {
      val left = self.getMinX
      val top = self.getMinY
      val h = self.getHeight
      val w = self.getWidth
      LTBounds.Doubles(left, top, w, h)
    }
  }

  implicit class RicherPDRectangle(val self: PDRectangle) extends AnyVal {
    def toLBBounds(): LBBounds = {
      val left = self.getLowerLeftX
      val top = self.getUpperRightY
      // val bottom = self.getLowerLeftY
      val w = self.getWidth
      val h = self.getHeight
      LTBounds.Floats(left, top, w, h).toLBBounds
    }
    // def xtoLTBounds(): LTBounds = {
    //   val left = self.getLowerLeftX
    //   val top = self.getUpperRightY
    //   val w = self.getWidth
    //   val h = self.getHeight
    //   // LTBounds.Floats(l, t-h, w, h)
    //   LTBounds.Floats(left, top, w, h)
    // }
  }

}

import ExtractionImplicits._

sealed trait ExtractedItem {

  def id: Int@@CharID
  def minBBox: LTBounds

  lazy val location: Point = minBBox.toPoint(Dir.BottomLeft)

  def strRepr(): String

}

object ExtractedItem {
  // implicit class RicherExtractedItem(val self: CharItem) extends AnyVal {}

  case class CharItem(
    id: Int@@CharID,
    char: String,
    fontName: String,
    glyphProps: GlyphProps
  ) extends ExtractedItem {
    def strRepr(): String = char
    lazy val fontBbox = glyphProps.fontBBox
    lazy val minBBox = glyphProps.glyphBBox
  }


  case class ImgItem(
    id: Int@@CharID,
    minBBox: LTBounds
  ) extends ExtractedItem {
    def strRepr(): String = s"[image ${minBBox.prettyPrint}]"
  }

  case class PathItem(
    id: Int@@CharID,
    minBBox: LTBounds,
    waypoints: Seq[Point]
  ) extends ExtractedItem {

    lazy val pp = waypoints.map(_.prettyPrint).take(4).mkString(", ")
    def strRepr(): String = s"[path ${minBBox.prettyPrint}=[$pp]]"
  }


}

case class GlyphProps(
  finalGlyphBounds: Option[Shape],
  finalFontBounds: Shape,
  finalAffineTrans: AffineTransform
) {


  lazy val fontBBox = finalFontBounds.getBounds2D().toLTBounds
  lazy val glyphBBox = finalGlyphBounds.map(_.getBounds2D().toLTBounds).getOrElse { fontBBox }
  lazy val scalingFactor: Int@@ScalingFactor = {
    val determinant = finalAffineTrans.getDeterminant
    val normDet = determinant * 1000 * 1000
    ScalingFactor(normDet.toInt)
  }

}

case class PageSpaceTransforms(
  flip: AffineTransform,
  rotate: AffineTransform,
  translate: AffineTransform
) {
  def transform(shape: Shape): Shape = {
    translate.createTransformedShape(
      rotate.createTransformedShape(
        flip.createTransformedShape(
          shape
        )))
  }
}


case class FontMetrics(
  ascent: Float,
  descent: Float,
  capline: Float
) {}


case class AsciiHeightRecord(
  heights: Array[Double] = Array.ofDim[Double](128)
)


case class FontProperties(
  name: String,
  fontType: String,
  declaredMetrics: FontMetrics,
  pageCount: Int
) {


  val alphaEvidence = Array.ofDim[Int](LetterFrequencies.Letters.length)
  val bigramEvidence = Array.ofDim[Int](LetterFrequencies.Bigrams.length)
  // val trigramEvidence = Array.ofDim[Int](LetterFrequencies.Trigrams.length)


  val glyphOccurrenceCounts = gcol.HashBasedTable.create[Int@@PageNum, Int@@ScalingFactor, Int]()

  val asciiHeightsPerScaleFactor = mutable.HashMap[Int@@ScalingFactor, AsciiHeightRecord]()

  def initGlyphEvidence(c: Char, glyphProps: GlyphProps, pageNum: Int@@PageNum): Unit = {

    val bbox = glyphProps.glyphBBox
    val height = bbox.height.asDouble()
    val scalingFactor = glyphProps.scalingFactor

    if (c.toInt < 128) {
      val rec = asciiHeightsPerScaleFactor.getOrElseUpdate(scalingFactor, AsciiHeightRecord())
      rec.heights(c.toInt) = height
    }

    val i = LetterFrequencies.Letters.indexOf(c)
    if (i >= 0) {
      alphaEvidence(i) += 1
    }

    // val priorCount = if (glyphOccurrenceCounts.contains(pageNum, scalingFactor)) {
    //   glyphOccurrenceCounts.get(pageNum, scalingFactor)
    // } else { 0 }

    // glyphOccurrenceCounts.put(pageNum, scalingFactor, priorCount+1)

  }

  def isNatLangFont(): Boolean = {
    val nonZeros = bigramEvidence.count(_ > 0)
    nonZeros > 3
  }

  private def nonZeroHeights(letters: String, asciiHeights: AsciiHeightRecord): Seq[(Char, Double)]= {
    letters.map { c =>
      val h = asciiHeights.heights(c.toInt)
      (c, h)
    }.filter(_._2 > 0d)
  }

  def inferredMetrics(): FontMetrics = {
    val caps = ('A'.toInt to 'Z'.toInt).map(_.toChar).mkString
    if (isNatLangFont()) {

      // asciiHeightsPerScaleFactor.foreach { case (scaleFactor, asciiHeightRec) =>
      //   println(s"At ScalingFactor = ${scaleFactor}")
      //   val midlineEv = nonZeroHeights("aeoru", asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg
      //   val ascentEv = nonZeroHeights("ldkh", asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg
      //   val descentEv = nonZeroHeights("ypqg", asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg
      //   val capEv = nonZeroHeights(caps, asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg

      //   println(s"    midline ev  : ${midlineEv}")
      //   println(s"    ascentEv ev : ${ascentEv}")
      //   println(s"    descentEv ev: ${descentEv}")
      //   println(s"    cap ev      : ${capEv}")
      // }


      declaredMetrics

    } else {

      declaredMetrics
    }
  }

  def getGlyphMetrics(glyphProps: GlyphProps): FontMetrics = {

    ???
  }
}

object FontDefs {
  def getQualifiedFontName(pdFont: PDFont): String = {
    s"""${pdFont.getName}::${pdFont.getType}"""
  }

  def getFontName(pdFont: PDFont): String = {
    pdFont.getName
  }

}

class FontDefs(pageCount: Int) {

  import FontDefs._

  val fontProperties = mutable.ArrayBuffer[FontProperties]()

  def getFont(fontName: String): Option[FontProperties] = {
    fontProperties.find(_.name == fontName)
  }

  def addFont(pdFont: PDFont): FontProperties = {

    val fname = getFontName(pdFont)

    if (!fontProperties.exists(_.name == fname)) {
      val fontDesc = pdFont.getFontDescriptor
      // if (fontDesc==null) {
      //   println(s"WT Font?? ${pdFont}, $fname")
      // }
      val props = FontProperties(
        fname,
        pdFont.getType,
        FontMetrics(
          0f, 0f, 0
          // fontDesc.getAscent,
          // fontDesc.getDescent,
          // fontDesc.getCapHeight
        ),
        pageCount
      )
      fontProperties.append(props)
    }
    getFont(pdFont.getName).get
  }

  def addGlyphEvidence(pdFont: PDFont, char: Char, glyphProps: GlyphProps, pageNum: Int@@PageNum): Unit = {
    val fontProps = addFont(pdFont)

    fontProps.initGlyphEvidence(char, glyphProps, pageNum)
  }



  def addNGramEvidence(fontName: String, pageNum: Int@@PageNum, char: Char, chars: Char*): Unit = {
    val maybeProps = getFont(fontName)
    if (maybeProps.isEmpty) {
      println(s"Missing font: ${fontName} for chars: ${chars.mkString}")
    }

    maybeProps.foreach {  props =>
      val ngram = (char +: chars).mkString

      // if (chars.length == 2) {
      //   val i = LetterFrequencies.Trigrams.indexOf(ngram)
      //   if (i >= 0) {
      //     props.trigramEvidence(i) += 1
      //   }
      // } else if (chars.length == 1) {
      if (chars.length == 1) {
        val i = LetterFrequencies.Bigrams.indexOf(ngram)
        if (i >= 0) {
          props.bigramEvidence(i) += 1
        }
      }
    }

  }



}
