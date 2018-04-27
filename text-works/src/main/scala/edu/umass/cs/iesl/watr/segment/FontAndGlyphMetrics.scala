package edu.umass.cs.iesl.watr
package segment

import extract._
import utils.ExactFloats._
import utils.SlicingAndDicing._
import watrmarks._
import scalaz.syntax.equal._
import scalaz.std.string._
import textboxing.{TextBoxing => TB}, TB._
import utils.SlicingAndDicing._
import ExtractedItem._

import TypeTags._

trait FontAndGlyphMetrics extends PageScopeSegmenter with TextBlockGrouping { self =>

  def findLineLayoutMetrics(lineLabel: Label): Unit = {
    val lines = getLabeledLines(lineLabel)

    val offsetEvidence = lines.map { lineShape =>
      val chars = getCharsForShape(lineShape)
      val scaledFontId = chars.head.scaledFontId

      val fontBoundsBottoms = chars.map(_.fontBbox.bottom.unwrap)
      val fontBoundsBottom = fontBoundsBottoms.head
      val nonMatching = fontBoundsBottoms.filterNot(_ == fontBoundsBottom).length

      assume(nonMatching == 0)


      val midrisers = chars.filter(charItem => CharClasses.Midrisers.contains(charItem.char))
      val ascenders = chars.filter(charItem => CharClasses.Ascenders.contains(charItem.char))
      val descenders = chars.filter(charItem => CharClasses.Descenders.contains(charItem.char))
      val caps = chars.filter(charItem => CharClasses.Caps.contains(charItem.char))

      // val letterT = chars.filter(_.char == 't')
      // val letterI = chars.filter(_.char == 'i')
      // val allIds = chars.map(_.id)
      // val letterIds = (midrisers ++ ascenders ++ descenders ++ letterT ++ letterI ++ caps).map(_.id)
      // val otherIds = allIds.toSet - letterIds

      // val otherChars = chars.filter(charItem => otherIds.contains(charItem.id))
      // val midriseOffsets = midrisers.map{ charItem =>
      //   (charItem.fontBbox.bottom - charItem.minBBox.top , charItem)
      // }
      // val tAscentOffset = letterT.map{ charItem =>
      //   (charItem.fontBbox.bottom - charItem.minBBox.top , charItem)
      // }


      FontBaselineOffsetsAccum(
        scaledFontId,
        fontBaseline = FloatRep(fontBoundsBottom),
        caps     = caps,
        ascents  = ascenders,
        midrises = midrisers,
        baselines= midrisers,
        descents = descenders,
        bottoms  = descenders,
      )
    }

    val allFontIds = docScope.fontDefs.getNatLangFontIdentifiers()

    allFontIds.foreach{ scaledFontId =>

      val baselineOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.baselines, _.minBBox.bottom)

      val midriseOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.midrises, _.minBBox.top)

      val capsOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.caps, _.minBBox.top)

      val ascentOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.ascents, _.minBBox.top)

      val descentOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.descents, _.minBBox.bottom)
    }
  }

  def findMostCommonOffsetBy(
    offsetEvidence: Seq[FontBaselineOffsetsAccum],
    scaledFontId: String@@ScaledFontID,
    f: FontBaselineOffsetsAccum => Seq[CharItem],
    dist: CharItem => Int@@FloatRep
  ): Int@@FloatRep = {

    val fontEvidence = offsetEvidence
      .filter(_.scaledFontId === scaledFontId)

    if (fontEvidence.nonEmpty) {
      val fontBaseline = fontEvidence.head.fontBaseline

      val offsetCountsByChar = fontEvidence
        .flatMap(f(_))
        .map{ charItem => (fontBaseline - dist(charItem), charItem) }
        .uniqueCountBy(_._2.char)

      val mostCommonOffsetRec = offsetCountsByChar.maxBy(_._1)

      mostCommonOffsetRec._2._1
    } else {
      ???
    }

  }

  def debugPrintStats(allBaselines: Seq[(Int@@FloatRep, CharItem)]): Unit = {

    val kvCols0 = allBaselines.groupBy(_._1)
      .toList.map{ case (k, vs) =>
        val chars = vs.map(_._2.char).uniqueBy(c => c).sorted.mkString

        (s"+${k.pp}".box, chars.box)
      }

    val byChar = allBaselines.groupBy(_._2.char)

    val kvCols = byChar.map{ case (k, vs) =>
      val counted: Seq[(Int, (Int@@FloatRep, CharItem))] = vs.uniqueCountBy(_._1)
      val offsets = counted.map(_._2._1.unwrap)
      val warning = math.abs(offsets.min - offsets.max) > 1
      val warningSign = if (warning) {
        "!!"
      } else "  "

      if (warning) {
        traceLog.trace {
          val charBounds = vs.map{v => v._2.minBBox }
          figure(charBounds:_*) tagged "Wonky Glyph Bounds Baseline Offsets"
        }
        traceLog.trace {
          val charBounds = vs.map{v => v._2.fontBbox }
          figure(charBounds:_*) tagged "Wonky Font Bounds Baseline Offsets"
        }
      }

      val cvs = counted.map{ case (count, (offset, char)) =>
        s"${warningSign}${count} x ${offset.pp}"
      }.mkString(", ")

      (k.toString.box, cvs.box)
    }

    val all = allBaselines.map(_._1.unwrap)
    val avg = FloatRep(all.sum / all.length)
    val min = FloatRep(all.min)
    val max = FloatRep(all.max)

    // docScope.fontDefs.getScaledFont(scaledFontId)

    // println(s"${scaledFontId} Baseline Offsets page ${pageNum}")
    println(s"    min/max/avg ${min.pp} / ${max.pp} / ${avg.pp}")
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
