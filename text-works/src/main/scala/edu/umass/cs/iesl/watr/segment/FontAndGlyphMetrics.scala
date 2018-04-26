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

import TypeTags._

trait FontAndGlyphMetrics extends PageScopeSegmenter with TextBlockGrouping { self =>

  // import utils.GuavaHelpers
  // val evidenceTable  = GuavaHelpers.initTable[String@@ScaledFontID, Int@@FloatRep]()
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

      val midriseOffsets = midrisers.map{ charItem =>
        (charItem.fontBbox.bottom - charItem.minBBox.top , charItem)
      }

      val baselineOffsets = (midrisers ++ ascenders ++ caps).map{ charItem =>
        (
          charItem.fontBbox.bottom - charItem.minBBox.bottom, charItem
        )
      }

      val descenderOffsets = descenders.map{ charItem =>
        (
          charItem.fontBbox.bottom - charItem.minBBox.bottom
            , charItem
        )
      }

      val ascenderOffsets = ascenders.map{ charItem =>
        (
          charItem.fontBbox.bottom - charItem.minBBox.top
            , charItem
        )
      }

      val caplineOffset = caps.map{ charItem =>
        (
          charItem.fontBbox.bottom - charItem.minBBox.top
            , charItem
        )
      }

      // val iAscentOffset = letterI.map{ charItem =>
      //   (
      //     charItem.fontBbox.bottom - charItem.minBBox.top
      //       , charItem
      //   )
      // }

      // val tAscentOffset = letterT.map{ charItem =>
      //   (
      //     charItem.fontBbox.bottom - charItem.minBBox.top
      //       , charItem
      //   )
      // }


      FontBaselineOffsetsAccum(
        scaledFontId,
        fontBaseline = FloatRep(fontBoundsBottom),
        cap     = caplineOffset,
        ascent  = ascenderOffsets,
        midrise = midriseOffsets,
        baseline= baselineOffsets,
        descent = descenderOffsets,
        bottom  = descenderOffsets,

      )
    }

    val allFontIds = docScope.fontDefs.getNatLangFontIdentifiers()

    allFontIds.foreach{ scaledFontId =>


      val allBaselines = offsetEvidence
        .filter(_.scaledFontId === scaledFontId)
        .flatMap{ accum => accum.baseline }

      if (allBaselines.nonEmpty) {
        val byChar = allBaselines.groupBy(_._2.char)

        val kvCols = byChar.map{ case (k, vs) =>
          // vs.uniqueC
          val counted = vs.uniqueCountBy(_._1)
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
            s"${warningSign}${count} x ${offset}'"
          }.mkString(", ")

          (k.toString.box, cvs.box)
        }

        val all = allBaselines.map(_._1.unwrap)
        val avg = FloatRep(all.sum / all.length)
        val min = FloatRep(all.min)
        val max = FloatRep(all.max)

        docScope.fontDefs.getScaledFont(scaledFontId)

        println(s"${scaledFontId} Baseline Offsets page ${pageNum}")
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
      } else {
        // println(s"No evidence for ${scaledFontId} Baseline Offsets")

      }
    }


  }

}
