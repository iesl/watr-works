package org.watrworks
package segment

import extract._
import utils.ExactFloats._
import watrmarks._
import scalaz.syntax.equal._
import scalaz.std.string._
import textboxing.{TextBoxing => TB}, TB._
import utils.SlicingAndDicing._
import ExtractedItem._
import scala.collection.mutable

import TypeTags._

object FontAndGlyphMetrics {

  def createFontNameVariants(
    scaledFontId: String @@ ScaledFontID
  ): Seq[(String, Int @@ ScalingFactor, String @@ ScaledFontID)] = {
    val (fontName, scalingFactor) = FontDefs.splitScaledFontId(scaledFontId)
    val variations                = mutable.ArrayBuffer[String](fontName)
    if (fontName.contains("+")) {
      val split = fontName.split("[+]")
      variations.append(split(1))
    }
    if (fontName.contains(",")) {
      val split = fontName.split(",")
      variations.append(split(0))
    }

    variations.toSeq.map { variant =>
      (variant, scalingFactor, scaledFontId)
    }
  }

  def countFontBaselineOffsetsBy(
    offsetEvidence: Seq[FontBaselineOffsetsAccum],
    f: FontBaselineOffsetsAccum => Seq[CharItem],
    dist: CharItem => Int @@ FloatRep
  ): Seq[(Int, (Int @@ FloatRep, CharItem))] = {

    val offsetCountsByChar = offsetEvidence
      .flatMap { ev =>
        f(ev).map { charItem =>
          (ev.fontBaseline - dist(charItem), charItem)
        }
      }
      .uniqueCountBy(_._2.char)

    offsetCountsByChar
  }

  def findMostCommonOffsetBy(
    offsetEvidence: Seq[FontBaselineOffsetsAccum],
    f: FontBaselineOffsetsAccum => Seq[CharItem],
    dist: CharItem => Int @@ FloatRep
  ): Int @@ FloatRep = {
    val offsetCountsByChar = countFontBaselineOffsetsBy(offsetEvidence, f, dist)

    if (offsetCountsByChar.nonEmpty) {
      val mostCommonOffsetRec = offsetCountsByChar.maxBy(_._1)

      mostCommonOffsetRec._2._1
    } else {
      FloatExact.zero
    }
  }

}

trait FontAndGlyphMetricsDocWide extends BaseDocumentSegmenter { self =>
  import FontAndGlyphMetrics._

  def computeScaledSymbolicFontMetrics(): Unit = {
    val natLangFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang = true)

    val natLangVariantNames = natLangFontIds.flatMap { scaledFontId =>
      createFontNameVariants(scaledFontId)
    }

    val symbolicFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang = false)
    println(s"symbolicFontIds: ${symbolicFontIds}")

    symbolicFontIds.foreach { scaledSymFontId =>
      val (symFontName @ _, symScalingFactor) = FontDefs.splitScaledFontId(scaledSymFontId)

      val symVariants     = createFontNameVariants(scaledSymFontId)
      val symVariantNames = symVariants.map(_._1)

      val matchingVariantName = natLangVariantNames.find {
        case (name, scalingFactor @ _, natScaledFontId @ _) =>
          symVariantNames.contains(name)
      }


      println(s"matching variants: ${matchingVariantName} matches ${scaledSymFontId}")
      matchingVariantName.fold[Unit](() => ())({
        case (variantName, scalingFactor @ _, natScaledFontId) =>
          val matchingNatLangFont = docScope.fontDefs.getScaledFontOffsets(natScaledFontId)
          val rescaledOffsets     = matchingNatLangFont.rescaledAs(variantName, symScalingFactor)

          println(s"Symbolic Font matches Nat Lang Font, rescaling Nat Lang: ")
          println(s"    ${matchingNatLangFont}")
          println(s"        To: ")
          println(s"    ${rescaledOffsets}")
          docScope.fontDefs.setScaledFontOffsets(scaledSymFontId, rescaledOffsets)

      })

    }

  }

  def computeScaledFontHeightMetrics(lineLabel: Label): Unit = {
    // Requires that lineLabel (e.g. LB.CharRunFontBaseline) has linked glyphs (getCharsForShape())
    val offsetEvidences = for {
      pageSeg   <- pageSegmenters
      lineShape <- pageSeg.getLabeledLines(lineLabel)
    } yield {
      val chars        = pageSeg.getCharsForShape(lineShape)
      val scaledFontId = chars.head.scaledFontId

      // isn't this the same as lineShape.bottom?
      val fontBoundsBottoms = chars.map(_.fontBbox.bottom.unwrap)
      val fontBoundsBottom  = fontBoundsBottoms.head
      // Why would there be non-matching chars?
      val nonMatching = fontBoundsBottoms.filterNot(_ == fontBoundsBottom).length

      // Ah...
      assume(nonMatching == 0)

      val midrisers  = chars.filter(charItem => CharClasses.Midrisers.contains(charItem.char))
      val ascenders  = chars.filter(charItem => CharClasses.Ascenders.contains(charItem.char))
      val descenders = chars.filter(charItem => CharClasses.Descenders.contains(charItem.char))
      val caps       = chars.filter(charItem => CharClasses.Caps.contains(charItem.char))

      val topsAndBottoms    = chars.map { c => (c.minBBox.top, c.minBBox.bottom, c) }
      val maxTop            = topsAndBottoms.minBy(_._1)
      val minBottom         = topsAndBottoms.maxBy(_._2)
      val maxTopEvidence    = Seq(maxTop._3)
      val minBottomEvidence = Seq(minBottom._3)

      FontBaselineOffsetsAccum(
        scaledFontId,
        fontBaseline = FloatRep(fontBoundsBottom),
        caps = caps,
        ascents = ascenders,
        midrises = midrisers,
        baselines = midrisers,
        descents = descenders,
        tops = maxTopEvidence,
        bottoms = minBottomEvidence
      )
    }

    val allFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang = true)

    allFontIds.foreach { scaledFontId =>
      val offsetEvidenceForFont = offsetEvidences
        .filter(_.scaledFontId === scaledFontId)

      var baselineOffset =
        findMostCommonOffsetBy(offsetEvidenceForFont, _.baselines, _.minBBox.bottom)

      var midriseOffset =
        findMostCommonOffsetBy(offsetEvidenceForFont, _.midrises, _.minBBox.top)

      var capsOffset = findMostCommonOffsetBy(offsetEvidenceForFont, _.caps, _.minBBox.top)

      var ascentOffset =
        findMostCommonOffsetBy(offsetEvidenceForFont, _.ascents, _.minBBox.top)

      val descentOffset =
        findMostCommonOffsetBy(offsetEvidenceForFont, _.descents, _.minBBox.bottom)

      val topOffsets = countFontBaselineOffsetsBy(offsetEvidenceForFont, _.tops, _.minBBox.top)

      val topOffsetDeltas = topOffsets.map(_._2._1)
      val topOffset       = if (topOffsetDeltas.nonEmpty) topOffsetDeltas.max else FloatExact.zero

      val bottomOffsets =
        countFontBaselineOffsetsBy(offsetEvidenceForFont, _.bottoms, _.minBBox.bottom)

      val bottomOffsetDeltas = bottomOffsets.map(_._2._1)
      val bottomOffset =
        if (bottomOffsetDeltas.nonEmpty) bottomOffsetDeltas.min
        else FloatExact.zero

      if (capsOffset == FloatExact.zero) {
        capsOffset = topOffset
      }
      if (ascentOffset == FloatExact.zero) {
        ascentOffset = topOffset
      }

      // default to 77% , 23% of topLine offset for mid/baseline
      if (midriseOffset == FloatExact.zero) {
        midriseOffset = (topOffset * 0.77.toFloatExact())
      }
      if (baselineOffset == FloatExact.zero) {
        baselineOffset = (topOffset * 0.23.toFloatExact())
      }

      docScope.fontDefs.setScaledFontOffsets(
        scaledFontId,
        FontBaselineOffsets(
          scaledFontId,
          fontBaseline = FloatExact.zero,
          _cap = capsOffset,
          _ascent = ascentOffset,
          _midrise = midriseOffset,
          _baseline = baselineOffset,
          _descent = descentOffset,
          _top = topOffset,
          _bottom = bottomOffset
        )
      )
    }
  }

}

trait FontAndGlyphMetrics extends BasePageSegmenter with TextBlockGrouping { self =>

  def debugPrintStats(allBaselines: Seq[(Int @@ FloatRep, CharItem)]): Unit = {

    val kvCols0 = allBaselines.groupBy(_._1).toList.map { case (k, vs) =>
      val chars = vs.map(_._2.char).uniqueBy(c => c).sorted.mkString

      (s"+${k.pp()}".box, chars.box)
    }

    val byChar = allBaselines.groupBy(_._2.char)

    val kvCols = byChar.map { case (k, vs) =>
      val counted: Seq[(Int, (Int @@ FloatRep, CharItem))] = vs.uniqueCountBy(_._1)
      val offsets                                          = counted.map(_._2._1.unwrap)
      val warning                                          = math.abs(offsets.min - offsets.max) > 1
      val warningSign = if (warning) {
        "!!"
      } else "  "

      if (warning) {
        traceLog.trace {
          val charBounds = vs.map { v => v._2.minBBox }
          figure(charBounds) tagged "Wonky Glyph Bounds Baseline Offsets"
        }
        traceLog.trace {
          val charBounds = vs.map { v => v._2.fontBbox }
          figure(charBounds) tagged "Wonky Font Bounds Baseline Offsets"
        }
      }

      val cvs = counted
        .map { case (count, (offset, char @ _)) =>
          s"${warningSign}${count} x ${offset.pp()}"
        }
        .mkString(", ")

      (k.toString.box, cvs.box)
    }

    val all = allBaselines.map(_._1.unwrap)
    val avg = FloatRep(all.sum / all.length)
    val min = FloatRep(all.min)
    val max = FloatRep(all.max)

    // docScope.fontDefs.getScaledFont(scaledFontId)

    // println(s"${scaledFontId} Baseline Offsets page ${pageNum}")
    println(s"    min/max/avg ${min.pp()} / ${max.pp()} / ${avg.pp()}")
    println(
      "Baseline Offsets by char".hangIndent(
        hjoin(
          vjoins(kvCols.map(_._1).toList),
          hspace(2),
          vjoins(kvCols.map(_._2).toList)
        )
      )
    )
    println(
      "Chars for Baseline Offsets".hangIndent(
        hjoin(
          vjoins(kvCols0.map(_._1).toList),
          hspace(2),
          vjoins(kvCols0.map(_._2).toList)
        )
      )
    )
  }

}
