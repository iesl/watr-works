package org.watrworks
package extract

import geometry._
import geometry.syntax._
import utils.{M3x3Position => M3}
import scala.collection.mutable
import org.apache.pdfbox.pdmodel.font._
import utils.ExactFloats._
import java.awt.{Shape}
import java.awt.geom._
import _root_.io.circe, circe._

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

  implicit class RicherRectangle2D(val self: Rectangle2D) extends AnyVal {
    def toRect(): Rect = {
      val left = self.getMinX
      val top  = self.getMinY
      val h    = self.getHeight
      val w    = self.getWidth
      Rect.Doubles(left, top, w, h)
    }
  }

  def makeScaledFontId(
    name: String,
    scalingFactor: Int @@ ScalingFactor
  ): String @@ ScaledFontID = {
    ScaledFontID(s"${name}x${scalingFactor.unwrap}")
  }

}

import ExtractionImplicits._

sealed trait ExtractedItem {

  def id: Int @@ CharID
  def minBBox: Rect

  lazy val location: Point = minBBox.toPoint(M3.BottomLeft)

  def strRepr(): String

}

object ExtractedItem {

  case class CharItem(
    id: Int @@ CharID,
    char: String,
    fontProps: FontProperties,
    glyphProps: GlyphProps,
    isLigature: Boolean
  ) extends ExtractedItem {
    def strRepr(): String = char

    lazy val fontBbox = glyphProps.fontBBox
    lazy val minBBox  = glyphProps.glyphBBox

    lazy val scaledFontId: String @@ ScaledFontID = {
      fontProps.getFontIdentifier(glyphProps.scalingFactor)
    }
  }

  case class CombiningMark(
    id: Int @@ CharID,
    mark: Char,
    fontProps: FontProperties,
    glyphProps: GlyphProps
  ) extends ExtractedItem {
    def strRepr(): String = mark.toString()

    lazy val fontBbox = glyphProps.fontBBox
    lazy val minBBox  = glyphProps.glyphBBox

    lazy val scaledFontId: String @@ ScaledFontID = {
      fontProps.getFontIdentifier(glyphProps.scalingFactor)
    }
  }

  case class ImgItem(
    id: Int @@ CharID,
    minBBox: Rect
  ) extends ExtractedItem {
    def strRepr(): String = s"[img ${minBBox.prettyPrint}]"
  }

  case class PathItem(
    id: Int @@ CharID,
    minBBox: Rect,
    waypoints: Seq[Point]
  ) extends ExtractedItem {
    lazy val pp = waypoints.map(_.prettyPrint).take(4).mkString("->")

    def strRepr(): String = s"[path $pp]"
  }

}

case class GlyphProps(
  finalGlyphBounds: Option[Shape],
  finalFontBounds: Shape,
  finalAffineTrans: AffineTransform,
  rotation: Int,
  prevSimilar: Int @@ CharID
) {

  lazy val fontBBox  = finalFontBounds.getBounds2D().toRect()
  lazy val glyphBBox = finalGlyphBounds.map(_.getBounds2D().toRect()).getOrElse { fontBBox }
  lazy val scalingFactor: Int @@ ScalingFactor = {
    val determinant = finalAffineTrans.getDeterminant
    val normDet     = determinant * 1000 * 1000
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
        )
      )
    )
  }
}

/** Track the offset from font bounds bottom line to each of the following y-positions:
  *    cap      - The maximum distance above the baseline for the tallest glyph in the font at a given text size.
  *    Ascent   - The recommended distance above the baseline for singled spaced text.
  *    Midrise  - (non -standard) the computed topline of chars without ascenders, e.g., eaomn
  *    Baseline - The 'true' baseline, i.e., line touching bottom of non-descender text
  *    Descent  - The recommended distance below the baseline for singled spaced text.
  *    Bottom   - The maximum distance below the baseline for the lowest glyph in the font at a given text size.
  */

case class FontBaselineOffsets(
  scaledFontId: String @@ ScaledFontID,
  fontBaseline: Int @@ FloatRep,
  private val _top: Int @@ FloatRep,
  private val _cap: Int @@ FloatRep,
  private val _ascent: Int @@ FloatRep,
  private val _midrise: Int @@ FloatRep,
  private val _baseline: Int @@ FloatRep,
  private val _descent: Int @@ FloatRep,
  private val _bottom: Int @@ FloatRep
) {
  override def toString(): String = {
    s"""|(${scaledFontId} @ ${fontBaseline.pp()} =
        |top: ${topLine.pp()}+${_top.pp()},
        |cap: ${capLine.pp()}+${_cap.pp()},
        |asc: ${ascentLine.pp()}+${_ascent.pp()},
        |mid: ${midriseLine.pp()}+${_midrise.pp()}
        |bas: ${baseLine.pp()}+${_baseline.pp()}
        |des: ${descentLine.pp()}+${_descent.pp()}
        |bot: ${bottomLine.pp()}+${_bottom.pp()})
        |""".stripMargin.replaceAll("\n", " ")

  }

  val topLine     = fontBaseline - _top
  val capLine     = fontBaseline - _cap
  val ascentLine  = fontBaseline - _ascent
  val midriseLine = fontBaseline - _midrise
  val baseLine    = fontBaseline - _baseline
  val descentLine = fontBaseline - _descent
  val bottomLine  = fontBaseline - _bottom

  def forFontBoxBottom(fontBaseline: Int @@ FloatRep): FontBaselineOffsets = {
    copy(fontBaseline = fontBaseline)
  }

  def rescaledAs(newName: String, newScalingFactor: Int @@ ScalingFactor): FontBaselineOffsets = {
    val (fontName @ _, scalingFactor) = FontDefs.splitScaledFontId(scaledFontId)

    val scalingRatio = newScalingFactor.unwrap.toDouble / scalingFactor.unwrap

    FontBaselineOffsets(
      makeScaledFontId(newName, newScalingFactor),
      fontBaseline = FloatExact.zero,
      _cap = _cap * scalingRatio,
      _ascent = _ascent * scalingRatio,
      _midrise = _midrise * scalingRatio,
      _baseline = _baseline * scalingRatio,
      _descent = _descent * scalingRatio,
      _top = _top * scalingRatio,
      _bottom = _bottom * scalingRatio
    )
  }

  def distanceBetween(
    f1: FontBaselineOffsets => Int @@ FloatRep,
    f2: FontBaselineOffsets => Int @@ FloatRep
  ): Int @@ FloatRep = {
    abs(f1(this) - f2(this))
  }

  def sliceBetween(
    f1: FontBaselineOffsets => Int @@ FloatRep,
    f2: FontBaselineOffsets => Int @@ FloatRep,
    r: Rect
  ): Option[Rect] = {
    val y1   = f1(this)
    val y2   = f2(this)
    val ytop = min(y1, y2)
    val ybot = max(y1, y2)
    val t = max(ytop, r.top)
    val b = min(ybot, r.bottom)
    r.clipTopBottom(t, b)
  }

}

case class FontBaselineOffsetsAccum(
  scaledFontId: String @@ ScaledFontID,
  fontBaseline: Int @@ FloatRep,
  caps: Seq[ExtractedItem.CharItem],
  ascents: Seq[ExtractedItem.CharItem],
  midrises: Seq[ExtractedItem.CharItem],
  baselines: Seq[ExtractedItem.CharItem],
  descents: Seq[ExtractedItem.CharItem],
  tops: Seq[ExtractedItem.CharItem],
  bottoms: Seq[ExtractedItem.CharItem]
)

object FontExportRecords {

  import circe.generic.semiauto._
  import TypeTagCodecs._

  implicit def Encode_FontMetrics: Encoder[FontMetrics]                   = deriveEncoder
  implicit def Encode_ScaledMetrics: Encoder[ScaledMetrics]               = deriveEncoder
  implicit def Encode_ScaledFontSummaryRecord: Encoder[ScaledFontSummary] = deriveEncoder
  implicit def Encode_FontSummaryRecord: Encoder[FontSummary]             = deriveEncoder

  case class FontMetrics(
    // top      : Int@@FloatRep,
    cap: Int @@ FloatRep,
    ascent: Int @@ FloatRep,
    midrise: Int @@ FloatRep,
    // baseline : Int@@FloatRep,
    descent: Int @@ FloatRep,
    bottom: Int @@ FloatRep
  )

  case class ScaledMetrics(
    scalingFactor: Int @@ ScalingFactor,
    fontMetrics: Option[FontMetrics]
  )

  case class ScaledFontSummary(
    name: String @@ ScaledFontID,
    scaledMetrics: ScaledMetrics
  )
  case class FontSummary(
    name: String,
    scaledFontSummaries: List[ScaledFontSummary]
  )
}

import FontExportRecords._

case class FontProperties(
  name: String,
  fontType: String,
  pageCount: Int
) {

  val alphaEvidence = Array.ofDim[Int](CharClasses.MostFrequentLetters.length)

  val asciiHeightsPerScaleFactor = GuavaHelpers.initTable[Int @@ ScalingFactor, Char, Double]()
  val asciiHeightsPerScaleFactorInv =
    GuavaHelpers.initTable[Int @@ ScalingFactor, Int @@ FloatRep, String]()
  val natLangGlyphOccurrenceCounts =
    GuavaHelpers.initTable[Int @@ PageNum, Int @@ ScalingFactor, Int]()
  val totalGlyphOccurrenceCounts =
    GuavaHelpers.initTable[Int @@ PageNum, Int @@ ScalingFactor, Int]()

  var docWideBigramCount   = 0
  var docWideTrigramCount  = 0
  var docWideLigatureCount = 0

  def initGlyphEvidence(
    unicodeStr: String,
    char: Char,
    glyphProps: GlyphProps,
    pageNum: Int @@ PageNum,
    priorSimilarChars: Seq[ExtractedItem.CharItem]
  ): Unit = {
    val bbox          = glyphProps.glyphBBox
    val height        = bbox.height.asDouble()
    val scalingFactor = glyphProps.scalingFactor

    // val recentSimilarTextWindow = char.toLower + priorSimilarChars.map(_.char).mkString.toLowerCase()
    val priorStr                = priorSimilarChars.map(_.char).mkString.toLowerCase()
    val recentSimilarTextWindow = s"${char.toLower}${priorStr}"

    val hasBigram  = CharClasses.hasCommonBigram(recentSimilarTextWindow.take(2))
    val hasTrigram = CharClasses.hasCommonTrigram(recentSimilarTextWindow.take(3))

    val isLigature = CharClasses.Ligatures.All.contains(unicodeStr)

    if (hasBigram) { docWideBigramCount += 1 }
    if (hasTrigram) { docWideTrigramCount += 1 }
    if (isLigature) { docWideLigatureCount += 1 }

    if (hasBigram || hasTrigram) {
      natLangGlyphOccurrenceCounts.modifyOrSet(pageNum, scalingFactor, _ + 1, 0)
    }
    totalGlyphOccurrenceCounts.modifyOrSet(pageNum, scalingFactor, _ + 1, 0)

    val shouldRecord = (32 < char && char < 128 || isLigature)

    if (shouldRecord) {
      asciiHeightsPerScaleFactor.set(scalingFactor, char, height)
      asciiHeightsPerScaleFactorInv.modifyOrSet(
        scalingFactor,
        bbox.height,
        { letters =>
          if (letters.contains(char)) letters
          else (letters + char).toSeq.sorted.unwrap
        },
        char.toString()
      )

    }

    val i = CharClasses.MostFrequentLetters.indexOf(char.toLower)
    if (i >= 0) {
      alphaEvidence(i) = alphaEvidence(i) + 1
    }
  }

  def isNatLangFont(): Boolean = {
    val nonZeros     = alphaEvidence.count(_ > 0)
    val hasNgrams    = docWideBigramCount > 2 || docWideTrigramCount > 2
    val hasLigatures = docWideLigatureCount > 0
    nonZeros > 4 || hasNgrams || hasLigatures
  }

  def getScaledMetrics(scalingFactor: Int @@ ScalingFactor): Option[ScaledMetrics] = {
    scaledMetrics().filter(_.scalingFactor === scalingFactor).headOption
  }

  def scaledMetrics(): Seq[ScaledMetrics] = {
    val midrisers  = CharClasses.Midrisers
    val ascenders  = CharClasses.Ascenders
    val descenders = CharClasses.Descenders

    if (isNatLangFont()) {
      val caps = CharClasses.Caps
      asciiHeightsPerScaleFactor.getRows().map { case (scalingFactor, charHeights) =>
        val sorted = charHeights.sortBy(_._1)
        val top    = sorted.filter(ch => caps.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val midrise =
          sorted.filter(ch => midrisers.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val ascent =
          sorted.filter(ch => ascenders.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val descent =
          sorted.filter(ch => descenders.contains(ch._1)).headOption.map(_._2).getOrElse(0d)
        val bottom =
          sorted.filter(ch => descenders.contains(ch._1)).headOption.map(_._2).getOrElse(0d)

        ScaledMetrics(
          scalingFactor,
          Some(
            FontMetrics(
              top.toFloatExact(),
              ascent.toFloatExact(),
              midrise.toFloatExact(),
              descent.toFloatExact(),
              bottom.toFloatExact()
            )
          )
        )
      }
    } else Seq()
  }

  def getScalingFactors(): Seq[Int @@ ScalingFactor] = {
    totalGlyphOccurrenceCounts.columnKeys.toList.sorted
  }

  def getFontIdentifier(scalingFactor: Int @@ ScalingFactor): String @@ ScaledFontID = {
    makeScaledFontId(name, scalingFactor)
  }

  def getFontIdentifiers(): Seq[String @@ ScaledFontID] = {
    getScalingFactors().map { getFontIdentifier(_) }
  }

  implicit val ShowString = Show.shows[String] { s => s }

  def getFontSummary(): FontSummary = {
    val metrics = scaledMetrics().map { scaledMetrics =>
      ScaledFontSummary(
        makeScaledFontId(name, scaledMetrics.scalingFactor),
        scaledMetrics
      )
    }
    FontSummary(
      name,
      metrics.toList
    )
  }

  def reportShort(): TB.Box = {
    val letterFreqs = TB.hjoins(
      TB.center1,
      alphaEvidence.toSeq
        .zip(CharClasses.MostFrequentLetters)
        .map { case (count, letter) =>
          s"| ${letter}" atop s"| ${count}"
        }
    )
    s"${name}".hangIndent(
      vjoin(
        s"Common English Letter Counts",
        letterFreqs
      )
    )
  }

  def report(): TB.Box = {
    val letterFreqs = TB.hjoins(
      TB.center1,
      alphaEvidence.toSeq
        .zip(CharClasses.MostFrequentLetters)
        .map { case (count, letter) =>
          s"| ${letter}" atop s"| ${count}"
        }
    )

    val charsPerHeightPerScale = asciiHeightsPerScaleFactorInv
      .addLeftLabel("Scaling")
      .addTopLabel("Height")
      .showBox()

    val heightPerScale = asciiHeightsPerScaleFactor
      .addLeftLabel("Scaling")
      .addTopLabel("Glyph")
      .showBox()

    val natGlyphPerPage = natLangGlyphOccurrenceCounts
      .addLeftLabel("Page")
      .addTopLabel("Scaling")
      .computeRowMarginals(0)(_ + _)
      .computeColMarginals(0)(_ + _)
      .showBox()

    val totalGlyphTable = totalGlyphOccurrenceCounts
      .addLeftLabel("Page")
      .addTopLabel("Scaling")
      .computeRowMarginals(0)(_ + _)
      .computeColMarginals(0)(_ + _)
      .showBox()

    s"${name}".hangIndent(
      vjoin(
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
        vspace(3)
      )
    )

  }
}

object FontDefs {
  def splitScaledFontId(scaledFontId: String @@ ScaledFontID): (String, Int @@ ScalingFactor) = {
    val str = scaledFontId.unwrap
    val n   = str.lastIndexOf("x")

    val (name, scaling0) = scaledFontId.unwrap.splitAt(n)
    val scaling          = scaling0.drop(1).toInt

    (name, ScalingFactor(scaling.toInt))
  }

}

class FontDefs(pageCount: Int) {

  val fontProperties = mutable.ArrayBuffer[FontProperties]()

  def getFont(fontName: String): Option[FontProperties] = {
    fontProperties.find(_.name == fontName)
  }

  private val scaledFontBaselineOffsets =
    mutable.HashMap[String @@ ScaledFontID, FontBaselineOffsets]()

  def setScaledFontOffsets(
    scaledFontId: String @@ ScaledFontID,
    fontBaselineOffsets: FontBaselineOffsets
  ): Unit = {
    scaledFontBaselineOffsets.put(scaledFontId, fontBaselineOffsets)
  }

  def hasScaledFontOffsets(scaledFontId: String @@ ScaledFontID): Boolean = {
    scaledFontBaselineOffsets.contains(scaledFontId)
  }
  def getScaledFontOffsets(scaledFontId: String @@ ScaledFontID): FontBaselineOffsets = {
    if (!scaledFontBaselineOffsets.contains(scaledFontId)) {

      val keyValPair = scaledFontBaselineOffsets.toList.mkString("{\n  ", "\n  ", "\n}")
      println(s"getScaledFontOffsets(${scaledFontId}): missing: $keyValPair")

    }
    scaledFontBaselineOffsets(scaledFontId)
  }

  def getFontIdentifiers(isNatLang: Boolean): Seq[String @@ ScaledFontID] = {
    fontProperties
      .to(Seq)
      .filter { p => isNatLang == p.isNatLangFont() }
      .flatMap(_.getFontIdentifiers())
  }

  sealed trait DummyID
  val fontNameIdGen = utils.IdGenerator[DummyID]()

  def addFont(pdFont: PDFont): FontProperties = {
    val fname = if (pdFont.getName() == null) {
      val nextIndex = fontProperties.length
      s"AnonFont#${nextIndex}"
    } else {
      pdFont.getName()
    }

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
  def reportShort(): TB.Box = {
    s"Font Definitions (Short). Page Count: ${pageCount}".hangIndent(
      vjoins(fontProperties.to(Seq).map(_.reportShort()))
    )
  }

  def report(): TB.Box = {
    s"Font Definitions. Page Count: ${pageCount}".hangIndent(
      vjoins(fontProperties.to(Seq).map(_.report()))
    )
  }

  def getFontSummaries(): List[FontSummary] = {
    fontProperties.map(_.getFontSummary()).toList
  }

}
