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
import textboxing.{TextBoxing => TB}, TB._

import utils.GuavaHelpers

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


case class FontMetrics(
  ascent: Float,
  descent: Float,
  capline: Float
) {}


case class AsciiHeightRecord(
  heights: Array[Double] = Array.ofDim[Double](128)
)

case class ScaledMetrics(
  scalingFactor: Int@@ScalingFactor,
  heightLowerCaseSmall: Double,
  heightLowerCaseLarge: Double,
  heightUpperCase: Double,
)

case class FontProperties(
  name: String,
  fontType: String,
  declaredMetrics: FontMetrics,
  pageCount: Int
) {


  val alphaEvidence = Array.ofDim[Int](LetterFrequencies.Letters.length)

  val asciiHeightsPerScaleFactor = gcol.HashBasedTable.create[Int@@ScalingFactor, Char, Double]()
  val asciiHeightsPerScaleFactorInv = gcol.HashBasedTable.create[Int@@ScalingFactor, Int@@FloatRep, String]()
  val natLangGlyphOccurrenceCounts = gcol.HashBasedTable.create[Int@@PageNum, Int@@ScalingFactor, Int]()
  val totalGlyphOccurrenceCounts = gcol.HashBasedTable.create[Int@@PageNum, Int@@ScalingFactor, Int]()
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
      val nglyphs = natLangGlyphOccurrenceCounts.get(pageNum, scalingFactor)
      natLangGlyphOccurrenceCounts.put(pageNum, scalingFactor, nglyphs + 1)
    }

    totalGlyphOccurrenceCounts.put(pageNum, scalingFactor,
      1 + totalGlyphOccurrenceCounts.get(pageNum, scalingFactor)
    )

    val shouldRecord =  ".-ABaiomldjk".contains(c)
    if (shouldRecord) {
      asciiHeightsPerScaleFactor.put(scalingFactor, c, height)
      val letters = asciiHeightsPerScaleFactorInv.get(scalingFactor, bbox.height)

      val update = if (letters==null) c.toString()
        else if (letters.contains(c)) letters
        else (letters + c).sorted

      asciiHeightsPerScaleFactorInv.put(scalingFactor, bbox.height, update)
    }

    val i = LetterFrequencies.Letters.indexOf(c)
    if (i >= 0) {
      alphaEvidence(i) = alphaEvidence(i) + 1
    }
  }

  def isNatLangFont(): Boolean = {
    val nonZeros = alphaEvidence.count(_ > 0)
    val hasNgrams = docWideBigramCount > 1 && docWideTrigramCount > 1
    nonZeros > 3 && hasNgrams
  }

  // private def nonZeroHeights(letters: String, asciiHeights: AsciiHeightRecord): Seq[(Char, Double)]= {
  //   letters.map { c =>
  //     val h = asciiHeights.heights(c.toInt)
  //     (c, h)
  //   }.filter(_._2 > 0d)
  // }
  // private val caps = ('A'.toInt to 'Z'.toInt).map(_.toChar).mkString
  // private def avg(rs: Seq[(Char, Double)]): Double = {
  //   if (rs.length > 0) {
  //     rs.map(_._2).sum / rs.length
  //   } else 0
  // }

  def scaledMetrics(): Seq[ScaledMetrics] = {
    // asciiHeightsPerScaleFactor.toSeq.map { case (scaleFactor, asciiHeightRec) =>

    //   val small = avg(nonZeroHeights("aeoru", asciiHeightRec))
    //   val med1  = avg(nonZeroHeights("ldkh", asciiHeightRec))
    //   val large = avg(nonZeroHeights(caps, asciiHeightRec))
    //   ScaledMetrics(
    //     scaleFactor,
    //     small, med1, large
    //   )
    // }
    Seq()

  }

  import scala.collection.JavaConverters._

  def getFontIdentifier(scalingFactor: Int@@ScalingFactor): String@@ScaledFontID = {
    ScaledFontID(s"${name}x${scalingFactor.unwrap}")
  }

  def getFontIdentifiers(): Seq[String@@ScaledFontID] = {
    totalGlyphOccurrenceCounts.columnKeySet().asScala
      .toList.sorted.map{ scalingFactor =>
        getFontIdentifier(scalingFactor)
      }
  }


  def report(): TB.Box = {
    val letterFreqs = TB.hjoins(TB.center1,
      alphaEvidence.zip(LetterFrequencies.Letters)
        .map{ case (count, letter) =>
          s"| ${letter}" atop s"| ${count}"
        }
    )

    val add = (a:Int, b:Int) => a + b

    val charsPerHeightPerScale = GuavaHelpers.guavaTableToBox[Int@@ScalingFactor, Int@@FloatRep, String](
      asciiHeightsPerScaleFactorInv, ""
    )

    val heightPerScale = GuavaHelpers.guavaTableToBox[Int@@ScalingFactor, Char, Double](
      asciiHeightsPerScaleFactor, 0
    )

    val natGlyphPerPage = GuavaHelpers.guavaTableToLabeledBox(
      natLangGlyphOccurrenceCounts, 0,
      "", "",
      add, add
    )

    val totalGlyphTable =  GuavaHelpers.guavaTableToLabeledBox(totalGlyphOccurrenceCounts, 0,
      "", "",
      add, add
    )

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

  def getFontIdentifiers(): Seq[String@@ScaledFontID] = {
    fontProperties.flatMap { _.getFontIdentifiers }
  }

  def addFont(pdFont: PDFont): FontProperties = {
    val fname = pdFont.getName()

    if (!fontProperties.exists(_.name == fname)) {
      val props = FontProperties(
        fname,
        pdFont.getType,
        FontMetrics(0f, 0f, 0),
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
