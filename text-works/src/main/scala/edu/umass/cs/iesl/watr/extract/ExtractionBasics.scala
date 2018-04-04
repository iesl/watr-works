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

  val asciiHeightsPerScaleFactor = mutable.HashMap[Int@@ScalingFactor, AsciiHeightRecord]()
  val natLangGlyphOccurrenceCounts = gcol.HashBasedTable.create[Int@@PageNum, Int@@ScalingFactor, Int]()
  val totalGlyphOccurrenceCounts = gcol.HashBasedTable.create[Int@@PageNum, Int@@ScalingFactor, Int]()

  def initGlyphEvidence(c: Char, glyphProps: GlyphProps, pageNum: Int@@PageNum, priorSimilarChars: Seq[ExtractedItem.CharItem]): Unit = {
    val bbox = glyphProps.glyphBBox
    val height = bbox.height.asDouble()
    val scalingFactor = glyphProps.scalingFactor

    val recentSimilarTextWindow = priorSimilarChars.map(_.char).mkString

    val hasBigram = LetterFrequencies.hasCommonBigram(recentSimilarTextWindow.take(2))
    val hasTrigram = LetterFrequencies.hasCommonTrigram(recentSimilarTextWindow.take(3))

    if (hasBigram || hasTrigram) {

      val nglyphs = natLangGlyphOccurrenceCounts.get(pageNum, scalingFactor)
      natLangGlyphOccurrenceCounts.put(pageNum, scalingFactor, nglyphs + 1)
    }

    totalGlyphOccurrenceCounts.put(pageNum, scalingFactor,
      1 + totalGlyphOccurrenceCounts.get(pageNum, scalingFactor)
    )

    if (c.toInt < 128) {
      val rec = asciiHeightsPerScaleFactor.getOrElseUpdate(scalingFactor, AsciiHeightRecord())
      rec.heights(c.toInt) = height
    }

    val i = LetterFrequencies.Letters.indexOf(c)
    if (i >= 0) {
      alphaEvidence(i) = alphaEvidence(i) + 1
    }
  }

  private def nonZeroHeights(letters: String, asciiHeights: AsciiHeightRecord): Seq[(Char, Double)]= {
    letters.map { c =>
      val h = asciiHeights.heights(c.toInt)
      (c, h)
    }.filter(_._2 > 0d)
  }

  private val caps = ('A'.toInt to 'Z'.toInt).map(_.toChar).mkString
  private def avg(rs: Seq[(Char, Double)]): Double = {
    if (rs.length > 0) {
      rs.map(_._2).sum / rs.length
    } else 0
  }

  def scaledMetrics(): Seq[ScaledMetrics] = {
    asciiHeightsPerScaleFactor.toSeq.map { case (scaleFactor, asciiHeightRec) =>

      val small = avg(nonZeroHeights("aeoru", asciiHeightRec))
      val med1  = avg(nonZeroHeights("ldkh", asciiHeightRec))
      val large = avg(nonZeroHeights(caps, asciiHeightRec))
      ScaledMetrics(
        scaleFactor,
        small, med1, large
      )
    }

  }

  def inferredMetrics(): FontMetrics = {
    // if (isNatLangFont()) {
    //   val caps = ('A'.toInt to 'Z'.toInt).map(_.toChar).mkString
    //   asciiHeightsPerScaleFactor.foreach { case (scaleFactor, asciiHeightRec) =>
    //     println(s"At ScalingFactor = ${scaleFactor}")
    //     val midlineEv = nonZeroHeights("aeoru", asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg
    //     val ascentEv = nonZeroHeights("ldkh", asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg
    //     val descentEv = nonZeroHeights("ypqg", asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg
    //     val capEv = nonZeroHeights(caps, asciiHeightRec).map {case (c, h) => s"${c}/${h}"} //avg

    //     println(s"    midline ev  : ${midlineEv}")
    //     println(s"    ascentEv ev : ${ascentEv}")
    //     println(s"    descentEv ev: ${descentEv}")
    //     println(s"    cap ev      : ${capEv}")
    //   }
    // }
    declaredMetrics
  }


  def report(): TB.Box = {
    val letterFreqs = TB.hjoins(TB.center1,
      alphaEvidence.zip(LetterFrequencies.Letters)
        .map{ case (count, letter) =>
          s"| ${letter}" atop s"| ${count}"
        }
    )

    val heightPerScale = "Ascii Heights per Scaling Factor" atop indent(4, vjoins(
      asciiHeightsPerScaleFactor.toSeq.map { case (scaleFactor, asciiHeightRec) =>
        val lowerHeights = nonZeroHeights(('a' to 'z').mkString, asciiHeightRec)
        val upperHeights = nonZeroHeights(('A' to 'A').mkString, asciiHeightRec)
        val lbox = hjoins(top, lowerHeights.map{case (ch, height) =>
          s"| ${ch}" atop s"| ${height}"
        })
        val ubox = hjoins(top, upperHeights.map{case (ch, height) =>
          s"| ${ch}" atop s"| ${height}"
        })

        s"@ x${scaleFactor}" atop(indent(2,
          vjoin(
            "Lower Case".box,
            lbox,
            "Upper Case",
            ubox
          )
        ))
      }
    ))


    val natGlyphPerPage = "Nat. Lang. Glyphs per page" atop {
      val tableMatrix = GuavaHelpers.guavaTableToMatrix(natLangGlyphOccurrenceCounts, 0)
      val grid = GuavaHelpers.tableToGrid(tableMatrix)
      indent(4, grid.toBox())
    }

    // Per page counts
    val totalGlyphTable = {
      val tableMatrix = GuavaHelpers.guavaTableToMatrix(totalGlyphOccurrenceCounts, 0)
      val grid = GuavaHelpers.tableToGrid(tableMatrix)
      indent(4, grid.toBox())
    }

    s"${name}".box atop TB.indent(4, vjoin(
      s"Common English Letter Counts",
      letterFreqs,
      vspace(1),
      vspace(1),
      heightPerScale,
      vspace(1),
      natGlyphPerPage,
      vspace(1),
      "Total Glyph Counts Per Page/Scale",
      totalGlyphTable
    ))


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
    val fname = getQualifiedFontName(pdFont)

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


  // def addNGramEvidence(fontName: String, pageNum: Int@@PageNum, char: Char, chars: Char*): Unit = {
  //   val maybeProps = getFont(fontName)
  //   if (maybeProps.isEmpty) {
  //     println(s"Missing font: ${fontName} for chars: ${chars.mkString}")
  //   }

  //   maybeProps.foreach {  props =>
  //     val ngram = (char +: chars).mkString

  //     if (chars.length == 1) {
  //       val i = LetterFrequencies.Bigrams.indexOf(ngram)
  //       if (i >= 0) {
  //         props.bigramEvidence(i) += 1
  //       }
  //     }
  //   }

  // }

  def report(): TB.Box = {
    s"Font Definitions. Page Count: ${pageCount}" atop indent(4, vjoins(
      fontProperties.map(_.report())
    ))
  }


}
