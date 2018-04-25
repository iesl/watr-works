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

// import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.Show

import TypeTags._
import scalaz.syntax.equal._
import textboxing.{TextBoxing => TB}, TB._

import utils.GuavaHelpers

protected object ExtractionImplicits {

  implicit class RicherPoint2D(val self: Point2D) extends AnyVal {
    def toPoint(): Point = {
      Point.Doubles(self.getX(), self.getY())
    }
  }

  // implicit class RicherPoint2D(val self: Point2D.Float) extends AnyVal {
  //   def toPoint(): Point = {
  //     Point.Doubles(self.x.toDouble, self.y.toDouble)
  //   }
  // }

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
  }

  def makeScaledFontId(name: String, scalingFactor: Int@@ScalingFactor): String@@ScaledFontID = {
    ScaledFontID(s"${name}x${scalingFactor.unwrap}")
  }

  def splitScaledFontId(scaledFontId: String@@ScaledFontID): (String, Int@@ScalingFactor) = {
    val str = scaledFontId.unwrap
    val n = str.lastIndexOf("x")

    val (name, scaling0) = scaledFontId.unwrap.splitAt(n)
    val scaling = scaling0.drop(1).toInt

    (name, ScalingFactor(scaling.toInt))
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
    fontProps: FontProperties,
    glyphProps: GlyphProps
  ) extends ExtractedItem {
    def strRepr(): String = char

    lazy val fontBbox = glyphProps.fontBBox
    lazy val minBBox = glyphProps.glyphBBox

    lazy val scaledFontId: String@@ScaledFontID = {
      fontProps.getFontIdentifier(glyphProps.scalingFactor)
    }
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


/**
  *  Top - The maximum distance above the baseline for the tallest glyph in the font at a given text size.
  *  Ascent - The recommended distance above the baseline for singled spaced text.
  *  Midrise = (non-standard) the computed topline of chars without ascenders, e.g., eaomn
  *  Baseline = 0
  *  Descent - The recommended distance below the baseline for singled spaced text.
  *  Bottom - The maximum distance below the baseline for the lowest glyph in the font at a given text size.
  *
  **/

case class FontMetrics(
  top: Int@@FloatRep,
  ascent: Int@@FloatRep,
  midrise: Int@@FloatRep,
  descent: Int@@FloatRep,
  bottom: Int@@FloatRep
)

case class ScaledMetrics(
  scalingFactor: Int@@ScalingFactor,
  fontMetrics: Option[FontMetrics]
)

case class AsciiHeightRecord(
  heights: Array[Double] = Array.ofDim[Double](128)
)

case class FontProperties(
  name: String,
  fontType: String,
  pageCount: Int
) {

  val alphaEvidence = Array.ofDim[Int](LetterFrequencies.MostFrequentLetters.length)

  val asciiHeightsPerScaleFactor    = GuavaHelpers.initTable[Int@@ScalingFactor, Char, Double]()
  val asciiHeightsPerScaleFactorInv = GuavaHelpers.initTable[Int@@ScalingFactor, Int@@FloatRep, String]()
  val natLangGlyphOccurrenceCounts  = GuavaHelpers.initTable[Int@@PageNum, Int@@ScalingFactor, Int]()
  val totalGlyphOccurrenceCounts    = GuavaHelpers.initTable[Int@@PageNum, Int@@ScalingFactor, Int]()

  var docWideBigramCount = 0
  var docWideTrigramCount = 0

  def initGlyphEvidence(c: Char, glyphProps: GlyphProps, pageNum: Int@@PageNum, priorSimilarChars: Seq[ExtractedItem.CharItem]): Unit = {
    val bbox = glyphProps.glyphBBox
    val height = bbox.height.asDouble()
    val scalingFactor = glyphProps.scalingFactor

    val recentSimilarTextWindow = priorSimilarChars.map(_.char).mkString

    val hasBigram = LetterFrequencies.hasCommonBigram(recentSimilarTextWindow.take(2))
    val hasTrigram = LetterFrequencies.hasCommonTrigram(recentSimilarTextWindow.take(3))

    if (hasBigram) { docWideBigramCount += 1 }
    if (hasTrigram) { docWideTrigramCount += 1 }


    if (hasBigram || hasTrigram) {
      natLangGlyphOccurrenceCounts.modifyOrSet(pageNum, scalingFactor, _+1, 0)
    }

    totalGlyphOccurrenceCounts.modifyOrSet(pageNum, scalingFactor, _+1, 0)

    val shouldRecord = 32 < c && c < 128
    if (shouldRecord) {
      asciiHeightsPerScaleFactor.set(scalingFactor, c, height)
      asciiHeightsPerScaleFactorInv.modifyOrSet(
        scalingFactor,
        bbox.height,
        { letters =>
          if (letters.contains(c)) letters
          else (letters + c).sorted
        },
        c.toString()
      )

    }

    val i = LetterFrequencies.MostFrequentLetters.indexOf(c)
    if (i >= 0) {
      alphaEvidence(i) = alphaEvidence(i) + 1
    }
  }

  def isNatLangFont(): Boolean = {
    val nonZeros = alphaEvidence.count(_ > 0)
    val hasNgrams = docWideBigramCount > 1 && docWideTrigramCount > 1
    nonZeros > 3 && hasNgrams
  }

  def getScaledMetrics(scalingFactor: Int@@ScalingFactor): Option[ScaledMetrics] = {
    scaledMetrics().filter(_.scalingFactor === scalingFactor).headOption
  }

  def scaledMetrics(): Seq[ScaledMetrics] = {
    val midrisers = "acemnorszuvwx"
    val ascenders = "bdfhklti"
    val descenders = "gjpqy"

    if (isNatLangFont()) {
      val caps = LetterFrequencies.CapLetters
      asciiHeightsPerScaleFactor.getRows().map{ case (scalingFactor, charHeights) =>
        val sorted = charHeights.sortBy(_._1)
        val top = sorted.filter(ch => caps.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val midrise = sorted.filter(ch => midrisers.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val ascent = sorted.filter(ch => ascenders.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val descent = sorted.filter(ch => descenders.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val bottom = sorted.filter(ch => descenders.contains(ch._1)).headOption.map(_._2).getOrElse(0d)

        ScaledMetrics(
          scalingFactor,
          Some(FontMetrics(
            top.toFloatExact(),
            ascent.toFloatExact(),
            midrise.toFloatExact(),
            descent.toFloatExact(),
            bottom.toFloatExact()
          ))
        )
      }
    }
    else Seq()
  }


  def getScalingFactors(): Seq[Int@@ScalingFactor] = {
    totalGlyphOccurrenceCounts.columnKeys.toList.sorted
  }

  def getFontIdentifier(scalingFactor: Int@@ScalingFactor): String@@ScaledFontID = {
    makeScaledFontId(name, scalingFactor)
  }

  def getFontIdentifiers(): Seq[String@@ScaledFontID] = {
    getScalingFactors().map{  getFontIdentifier(_) }
  }

  implicit val ShowString = Show.shows[String] { s => s }

  def report(): TB.Box = {
    val letterFreqs = TB.hjoins(TB.center1,
      alphaEvidence.zip(LetterFrequencies.MostFrequentLetters)
        .map{ case (count, letter) =>
          s"| ${letter}" atop s"| ${count}"
        }
    )

    val charsPerHeightPerScale = asciiHeightsPerScaleFactorInv
      .addLabels("Scaling", "Height")
      .showBox()

    val heightPerScale = asciiHeightsPerScaleFactor
      .addLabels("Scaling", "Glyph")
      .showBox()

    val natGlyphPerPage = natLangGlyphOccurrenceCounts
      .addLabels("Page", "Scaling")
      .computeRowMarginals(0)(_ + _)
      .computeColMarginals(0)(_ + _)
      .showBox()

    val totalGlyphTable = totalGlyphOccurrenceCounts
      .addLabels("Page", "Scaling")
      .computeRowMarginals(0)(_ + _)
      .computeColMarginals(0)(_ + _)
      .showBox()

    s"${name}".hangIndent(vjoin(
      s"Common English Letter Counts",
      letterFreqs,
      vspace(1),
      "Char Heights/Scale".hangIndent(
        heightPerScale
      ),
      "Chars Per Height/Scale".hangIndent(
        charsPerHeightPerScale
      ),
      vspace(1),
      "Nat. Lang. Bi/Trigram counts per Page/Scale".hangIndent(
        natGlyphPerPage
      ),
      vspace(1),
      "Total Glyph Counts Per Page/Scale".hangIndent(
        totalGlyphTable
      ),
      vspace(3),
    ))


  }
}


class FontDefs(pageCount: Int) {

  val fontProperties = mutable.ArrayBuffer[FontProperties]()

  def getFont(fontName: String): Option[FontProperties] = {
    fontProperties.find(_.name == fontName)
  }

  def getScaledFont(scaledFontId: String@@ScaledFontID): Option[ScaledMetrics] = {
    val (fontName, scalingFactor) = splitScaledFontId(scaledFontId)
    val fontProps = fontProperties.find(_.name == fontName).head
    fontProps.getScaledMetrics(scalingFactor)
  }

  def getFontIdentifiers(): Seq[String@@ScaledFontID] = {
    fontProperties.flatMap { _.getFontIdentifiers }
  }

  def addFont(pdFont: PDFont): FontProperties = {
    val fname = pdFont.getName()

    if (!fontProperties.exists(_.name == fname)) {
      val props = FontProperties(
        fname,
        pdFont.getType,
        pageCount
      )
      fontProperties.append(props)
    }
    getFont(fname).get
  }


  def report(): TB.Box = {
    s"Font Definitions. Page Count: ${pageCount}".hangIndent(
      vjoins(fontProperties.map(_.report()))
    )

  }


}
