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
import geometry._
import geometry.syntax._
import utils.QuickNearestNeighbors._
import scala.collection.mutable

import TypeTags._

trait FontAndGlyphMetricsDocWide extends DocumentScopeSegmenter { self =>
  private def createFontNameVariants(scaledFontId: String@@ScaledFontID): Seq[(String, Int@@ScalingFactor, String@@ScaledFontID)] = {
    val (fontName, scalingFactor) = FontDefs.splitScaledFontId(scaledFontId)
    val variations = mutable.ArrayBuffer[String](fontName)
    if (fontName.contains("+")) {
      val split = fontName.split("[+]")
      variations.append(split(1))
    }
    if (fontName.contains(",")) {
      val split = fontName.split(",")
      variations.append(split(0))
    }

    variations.toSeq.map{ variant =>
      (variant, scalingFactor, scaledFontId)
    }
  }

  def computeScaledSymbolicFontMetrics(): Unit = {
    val natLangFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang=true)

    val natLangVariantNames = natLangFontIds.flatMap{ scaledFontId =>
      createFontNameVariants(scaledFontId)
    }

    val symbolicFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang=false)

    symbolicFontIds.foreach { scaledSymFontId =>
      val (symFontName, symScalingFactor) = FontDefs.splitScaledFontId(scaledSymFontId)

      val symVariants = createFontNameVariants(scaledSymFontId)
      val symVariantNames = symVariants.map(_._1)

      val matchingVariantName = natLangVariantNames.find { case (name, scalingFactor, natScaledFontId) =>
        symVariantNames.contains(name)
      }

      matchingVariantName.foreach { case (variantName, scalingFactor, natScaledFontId) =>

        val matchingNatLangFont = docScope.fontDefs.getScaledFontOffsets(natScaledFontId)
        val rescaledOffsets = matchingNatLangFont.rescaledAs(variantName, symScalingFactor)
        // println(s"Symbolic Font matches Nat Lang Font, rescaling Nat Lang: ")
        // println(s"    ${matchingNatLangFont}")
        // println(s"        To: ")
        // println(s"    ${rescaledOffsets}")
        docScope.fontDefs.setScaledFontOffsets(scaledSymFontId, rescaledOffsets)
      }
    }

    // val offsetEvidence = for {
    //   pageSeg <- pageSegmenters
    // }  {
    //   val symbolicGlyphs = pageSeg.pageItems
    //     .collect{ case c: CharItem => c }
    //     .filterNot { _.fontProps.isNatLangFont() }

    //   symbolicGlyphs.groupBy { item =>
    //     item.scaledFontId
    //   }
    // }
  }

  def computeScaledFontHeightMetrics(lineLabel: Label): Unit = {
    val offsetEvidence = for {
      pageSeg <- pageSegmenters
      lineShape <- pageSeg.getLabeledLines(lineLabel)
    } yield {

      val chars = pageSeg.getCharsForShape(lineShape)
      val scaledFontId = chars.head.scaledFontId

      val fontBoundsBottoms = chars.map(_.fontBbox.bottom.unwrap)
      val fontBoundsBottom = fontBoundsBottoms.head
      val nonMatching = fontBoundsBottoms.filterNot(_ == fontBoundsBottom).length

      assume(nonMatching == 0)

      val midrisers = chars.filter(charItem => CharClasses.Midrisers.contains(charItem.char))
      val ascenders = chars.filter(charItem => CharClasses.Ascenders.contains(charItem.char))
      val descenders = chars.filter(charItem => CharClasses.Descenders.contains(charItem.char))
      val caps = chars.filter(charItem => CharClasses.Caps.contains(charItem.char))

      val topsAndBottoms = chars.map{c => (c.minBBox.top, c.minBBox.bottom, c)}
      val maxTop = topsAndBottoms.minBy(_._1)
      val minBottom = topsAndBottoms.maxBy(_._2)
      val maxTopEvidence = Seq(maxTop._3)
      val minBottomEvidence = Seq(minBottom._3)

      FontBaselineOffsetsAccum(
        scaledFontId,
        fontBaseline = FloatRep(fontBoundsBottom),
        caps         = caps,
        ascents      = ascenders,
        midrises     = midrisers,
        baselines    = midrisers,
        descents     = descenders,
        tops         = maxTopEvidence,
        bottoms      = minBottomEvidence,
      )
    }

    val allFontIds = docScope.fontDefs.getFontIdentifiers(isNatLang=true)

    allFontIds.foreach{ scaledFontId =>

      var baselineOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.baselines, _.minBBox.bottom)

      var midriseOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.midrises, _.minBBox.top)

      var capsOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.caps, _.minBBox.top)

      var ascentOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.ascents, _.minBBox.top)

      val descentOffset = findMostCommonOffsetBy(offsetEvidence, scaledFontId,
        _.descents, _.minBBox.bottom)


      val topOffsets = findCountedOffsetsBy(offsetEvidence, scaledFontId,
        _.tops, _.minBBox.top)

      val topOffsetDeltas = topOffsets.map(_._2._1)
      val topOffset = if (topOffsetDeltas.nonEmpty) topOffsetDeltas.max else FloatExact.zero

      val bottomOffsets = findCountedOffsetsBy(offsetEvidence, scaledFontId,
        _.bottoms, _.minBBox.bottom)

      val bottomOffsetDeltas = bottomOffsets.map(_._2._1)
      val bottomOffset = if (bottomOffsetDeltas.nonEmpty) bottomOffsetDeltas.min else FloatExact.zero

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


      docScope.fontDefs.setScaledFontOffsets(scaledFontId,
        FontBaselineOffsets(
          scaledFontId,
          fontBaseline = FloatExact.zero,
          _cap      = capsOffset,
          _ascent   = ascentOffset,
          _midrise  = midriseOffset,
          _baseline = baselineOffset,
          _descent  = descentOffset,
          _top      = topOffset,
          _bottom   = bottomOffset
        )
      )
    }
  }

  def findCountedOffsetsBy(
    offsetEvidence: Seq[FontBaselineOffsetsAccum],
    scaledFontId: String@@ScaledFontID,
    f: FontBaselineOffsetsAccum => Seq[CharItem],
    dist: CharItem => Int@@FloatRep
  ): Seq[(Int, (Int@@FloatRep, CharItem))] = {

    val fontEvidence = offsetEvidence
      .filter(_.scaledFontId === scaledFontId)

    val offsetCountsByChar = fontEvidence.flatMap{ ev =>
      f(ev).map{ charItem =>
        (ev.fontBaseline - dist(charItem), charItem)
      }
    }.uniqueCountBy(_._2.char)

    offsetCountsByChar
  }

  def findMostCommonOffsetBy(
    offsetEvidence: Seq[FontBaselineOffsetsAccum],
    scaledFontId: String@@ScaledFontID,
    f: FontBaselineOffsetsAccum => Seq[CharItem],
    dist: CharItem => Int@@FloatRep
  ): Int@@FloatRep = {
    val offsetCountsByChar = findCountedOffsetsBy(offsetEvidence, scaledFontId, f, dist)

    if (offsetCountsByChar.nonEmpty) {
      val mostCommonOffsetRec = offsetCountsByChar.maxBy(_._1)

      mostCommonOffsetRec._2._1
    } else {
      FloatExact.zero
    }
  }
}

trait FontAndGlyphMetrics extends PageScopeSegmenter with TextBlockGrouping { self =>

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


  protected def recordCharRunWidths(charRunBaselineShapes: Seq[LineShape]): Unit = {
    val pagewiseLineWidthTable = docScope.getPagewiseLinewidthTable();


    val leftToRightGroups = charRunBaselineShapes.groupByPairs {
      case (item1, item2) =>
        val b1 = item1.shape.bounds()
        val b2 = item2.shape.bounds()
        val leftToRight = b1.isStrictlyLeftOf(b2)
        val colinear = b1.bottom == b2.bottom

        leftToRight && colinear
    }

    leftToRightGroups.foreach { lineGroup =>
      val groupBounds = lineGroup.map(_.shape.bounds()).reduce(_ union _)
      val extractedItems = lineGroup.flatMap { lineShape =>
        getExtractedItemsForShape(lineShape)
      }
      val charItems = extractedItems.collect{ case i: ExtractedItem.CharItem =>  i }

      val (num, mostCommonScaledFontId) = charItems
        .map{ item => item.scaledFontId }
        .groupBy { x => x }
        .toList
        .map{case (k, v) => (v.length, k) }
        .sortBy(l => - l._1)
        .head

      // val bottomLine = groupBounds.toLine(Dir.Bottom)
      pagewiseLineWidthTable.modifyOrSet(pageNum, mostCommonScaledFontId, groupBounds.width :: _, List(groupBounds.width))
    }

    traceLog.trace {
      val bottomLines = leftToRightGroups
        .filter(_.length > 1)
        .map { lineGroup =>
          val groupBounds = lineGroup.map(_.shape.bounds()).reduce(_ union _)
          groupBounds.toLine(Dir.Bottom)
        }

      figure(bottomLines:_*) tagged "Joined Nat Lang CharRun Baselines"
    }
  }

  /**
    * Foreach horizontal nat-lang text line, find the 2 closest lines below
    * it, and record the distances to each line.
    *
    * Finally, cluster those vertical jumps into centroids.
    *
    *  l0     :    --------------
    *  dists  :           | |
    *  l0+1   :    -------- | ---
    *  l0+2   :    --------------
    */
  protected def recordNatLangVerticalLineSpacingStats(baselineShapes: Seq[LineShape]): Unit = {
    val windowSize = 2

    val contextDeltas = baselineShapes.map { baselineShape: LineShape =>
      val Line(Point.Doubles(x1, y1), Point.Doubles(x2, y2)) = baselineShape.shape

      val y1Exact = baselineShape.shape.p1.y

      pageVerticalSlice(x1, x2-x1).toSeq.flatMap { pageColumn =>
        val (aboveLine, belowLine) = pageColumn.splitHorizontal(y1Exact)


        val below = belowLine.map { bbox =>
          val query = bbox.translate(x=0, y = +1.0)
          searchForLines(query, LB.CharRunFontBaseline)
        } getOrElse { List() }


        val winBelow = below.sortBy(_.shape.p1.y).take(windowSize)

        winBelow.map { ctxLine => ctxLine.shape.p1.y - y1Exact }
      }
    }

    val yJumps = contextDeltas.flatten
    val yJumpClusters = qnn(yJumps, tolerance = 0.5d)

    val nonUniqueJumps = yJumpClusters.filter { bin =>
      bin.size() > 1
    }

    println(s"recordNatLangLineSpacing: Assigned Bins")
    println(nonUniqueJumps.mkString("\n  ", "\n  ", "\n"))
  }
}
