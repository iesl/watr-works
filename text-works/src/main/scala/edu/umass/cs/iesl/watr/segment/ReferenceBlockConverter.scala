package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._
import extract._
import utils.ExactFloats._
import utils.FunctionalHelpers._
import utils.SlicingAndDicing._
import watrmarks._
import annots._
import corpora._
  // import textboxing.{TextBoxing => TB}, TB._
import utils.SlicingAndDicing._
import utils.DoOrDieHandlers._
import utils.QuickNearestNeighbors._

import TypeTags._

object ReferenceBlockConversion {

  val ReferenceBlock          = Label.auto
  val PartialRefBegin         = Label.auto
  val PartialRefEnd           = Label.auto
  val Reference               = Label.auto
  val LeftHangingIndentColumn = Label.auto
  val LeftJustifiedColumn     = Label.auto
  val AlphabeticRefMarker     = Label.auto
  val NumericRefMarker        = Label.auto

  val allLabels = List(
    ReferenceBlock,
    PartialRefBegin,
    PartialRefEnd,
    Reference,
    LeftHangingIndentColumn,
    LeftJustifiedColumn,
    AlphabeticRefMarker,
    NumericRefMarker,
  )

}

trait ReferenceBlockConverter extends PageScopeSegmenter
    with FontAndGlyphMetrics
    with TextBlockGrouping { self =>


  import ReferenceBlockConversion._

  protected def filterPrelabeledRegions(refBlockBounds: LTBounds, candidates: Seq[LTBounds]): Seq[LTBounds] = {
    val refBeginZones = searchForRects(refBlockBounds, PartialRefBegin)
    val refEndZones = searchForRects(refBlockBounds, PartialRefEnd)
    val refZones = searchForRects(refBlockBounds, Reference)
    val allPreLabeled = refBeginZones ++ refEndZones ++ refZones
    val preLabeledBounds = allPreLabeled.map(_.shape)
    candidates.filter{ candidateBounds =>
      val intersectsPrelabeled = preLabeledBounds.exists { bbox =>
        candidateBounds.shave(1d).intersects(bbox.shave(1d))
      }
      val containsGlyphs = searchForRects(candidateBounds.shave(1d), LB.Glyph).nonEmpty
      val isMinimumHeight = candidateBounds.height > 2d

      !intersectsPrelabeled && containsGlyphs && isMinimumHeight
    }
  }

  protected def findCandidateMinBounds(candidateBounds: LTBounds): LTBounds = {
    val glyphs = searchForRects(candidateBounds.shave(1d), LB.Glyph)
    glyphs.map(_.shape).reduce(_ union _)
  }

  protected def queryForLeftmostCharsInColumn(columnBounds: LTBounds, refBlockBounds: LTBounds): Seq[LTBounds] = {
    val expandedColumnBounds = columnBounds.withinRegion(refBlockBounds)
      .adjacentRegions(Dir.Top, Dir.Center, Dir.Bottom)
      .orDie("What happened here?")

    val shiftedDown = expandedColumnBounds.translate(x=0, y=4d)
    val halfWidth = shiftedDown.width / 2
    val queryRect = shiftedDown.shave(Dir.Right, halfWidth)

    val fontBaselines = searchForLines(queryRect, LB.CharRunFontBaseline)
    val ySortedBaselines = fontBaselines.sortBy(_.shape.p1.y)

    val leftmostCharBounds = ySortedBaselines.map { fontBaseline =>
      val baselineItems = getCharsForShape(fontBaseline)
      val includedCharItems = baselineItems.filter{item =>

        queryRect.intersects(item.minBBox)
      }
      if (includedCharItems.nonEmpty) {
        val leftmostItem = includedCharItems.minBy(_.minBBox.left)
        val fontOffsets = docScope.fontDefs.getScaledFontOffsets(leftmostItem.scaledFontId)
        val baselineOffsets = fontOffsets.forBaseline(leftmostItem.fontBbox.bottom)
        val baseline = baselineOffsets.baseline
        // val midrise = baselineOffsets.midrise
        val capline = baselineOffsets.cap
        val adjHeight = baseline - capline
        val LTBounds(l, t, w, h) = leftmostItem.minBBox
        val adjustedBbox = if (adjHeight.unwrap == 0) {
          leftmostItem.minBBox
        } else {
          LTBounds(l, capline, w, adjHeight)
        }

        // Some(leftmostItem.minBBox)
        Some(adjustedBbox)
      } else None
    }.flatten

    val groupedByLine = leftmostCharBounds.groupByPairs { case (bbox1, bbox2) =>
      bbox1.isNeitherAboveNorBelow(bbox2)
    }

    groupedByLine.map{ line =>
      line.sortBy(_.left).head
    }
  }

  def convertReferenceBlocks(pageZonesAndLabels: Seq[(AnnotatedLocation.Zone, Label)]): Unit = {
    pageZonesAndLabels.foreach { case (zone: AnnotatedLocation.Zone, label: watrmarks.Label) =>
      assume(zone.regions.length == 1)
      val zoneRegion = zone.regions.head
      val zoneBounds = zoneRegion.bbox
      val zoneShape = indexShape(zoneBounds, label)

      traceLog.trace {
        shape(zoneShape).tagged(s"RefBlock Human Labeled ${label}")
      }
    }

    val zonesByLabel = pageZonesAndLabels.groupBy(_._2).mapValues(_.map(_._1))

    val referenceBlockZones = zonesByLabel.get(ReferenceBlock)

    referenceBlockZones.foreach { zones => zones.foreach { refBlockZone =>
      val refBlockRegion = refBlockZone.regions.headOption.orDie(s"no regions found in zone ${refBlockZone}")
      val leftHangIndentShapes = searchForRects(refBlockRegion.bbox, LeftHangingIndentColumn)
      val leftJustifiedColumnShapes = searchForRects(refBlockRegion.bbox, LeftJustifiedColumn)

      if (leftHangIndentShapes.nonEmpty) {

        // Case 1. LeftHangingIndentColumn exists
        val leftHangIndentShape = leftHangIndentShapes.head
        val leftmostCharBounds = queryForLeftmostCharsInColumn(leftHangIndentShape.shape, refBlockRegion.bbox)

        val hangingLineTops = leftmostCharBounds.map { bbox => bbox.top }

        val zero = (
          List[LTBounds](),
          refBlockRegion.bbox
        )

        val (finalSlices, finalBounds) = hangingLineTops.foldLeft(zero) {
          case ((slices, remainingBounds), yVal) =>
            val (maybeUpper, maybeLower) = remainingBounds.splitHorizontal(yVal)

            val slicesUpdate = maybeUpper.map(_ :: slices).getOrElse(slices)
            val remainingBoundsUpdate = maybeLower.orDie("should be lower slice here")
            (slicesUpdate, remainingBoundsUpdate)
        }

        val referenceCandidates = (finalBounds :: finalSlices).reverse

        traceLog.trace { figure(finalSlices:_*).tagged(s"Hanging Indent Slices: ${finalSlices.length}") }
        traceLog.trace { figure(finalBounds).tagged(s"Hanging Indent Final Bounds") }


        val nonIntersectingCandidates = filterPrelabeledRegions(refBlockRegion.bbox, referenceCandidates)

        traceLog.trace {
          figure(nonIntersectingCandidates:_*).tagged(s"Non intersecting Reference Candidates (${nonIntersectingCandidates.length})")
        }

        val referenceMinBounds = nonIntersectingCandidates.map{ candidateBounds =>
          findCandidateMinBounds(candidateBounds)
        }

        traceLog.trace {
          figure(referenceMinBounds:_*).tagged(s"Final Hanging Reference Min Bounds (${referenceMinBounds.length})" )
        }


      } else if (leftJustifiedColumnShapes.nonEmpty) {


        // Case 2. LeftJustifiedColumn exists

        val leftJustifiedColumnShape = leftJustifiedColumnShapes.head
        val leftmostCharBounds = queryForLeftmostCharsInColumn(leftJustifiedColumnShape.bounds, refBlockRegion.bbox)

        val leftmostCharBottoms = leftmostCharBounds.map(_.bottom)
        val yDeltas = findDeltas(leftmostCharBottoms)
        val yJumpClusters = qnn(yDeltas, tolerance = 1d)
        val yJumpBins = yJumpClusters.filter { bin =>
          bin.size() > 0
        }
        val sortedYJumps = yJumpBins.sortBy(_.size()).reverse

        println(s"sortedYJumps: Sorted Bins")
        println(sortedYJumps.mkString("\n  ", "\n  ", "\n"))

        println(s"leftmostChar bounds")
        println(leftmostCharBounds.map(_.prettyPrint).mkString("\n  ", "\n  ", "\n"))

        println(s"leftmostCharBottoms")
        println(leftmostCharBottoms.map(_.pp).mkString("\n  ", "\n  ", "\n"))

        println(s"yDeltas")
        println(yDeltas.map(_.pp)mkString("\n  ", "\n  ", "\n"))

        val maxJumpForIntraRefLines = sortedYJumps.headOption.map(_.maxValue()).orDie("no y-jump val 1 clusters??")

        val maybeMinJumpForInterRefLines = sortedYJumps.drop(1).headOption.map(_.minValue())

        if (maybeMinJumpForInterRefLines.isEmpty) {
          println("No suitable y-jumps for inter-reference gaps")
        }

        maybeMinJumpForInterRefLines.map{ minJumpForInterRefLines =>
          val jumpThreshold = ((maxJumpForIntraRefLines + minJumpForInterRefLines) / 2) + 0.1d
          // val jumpThreshold = minJumpForInterRefLines - 1d

          println(s"Jump threshold: ${jumpThreshold};  max-intra/min-inter = (${maxJumpForIntraRefLines}, ${minJumpForInterRefLines})")

          val groupedIntoRefs = leftmostCharBounds.groupByPairs{ case (bbox1, bbox2) =>
            val vspace = bbox2.bottom - bbox1.bottom
            vspace < jumpThreshold
          }

          val candidateBounds = groupedIntoRefs.map { refLeftChars =>
            val leftCharMinBounds = refLeftChars.reduce(_ union _)
            traceLog.trace {
              figure(leftCharMinBounds).tagged(s"Reference LeftChar Bounds")
            }
            val referenceCandidateBounds = leftCharMinBounds.withinRegion(refBlockRegion.bbox)
              .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
              .orDie("What happened here?")

            referenceCandidateBounds
          }

          val nonIntersectingCandidates = filterPrelabeledRegions(refBlockRegion.bbox, candidateBounds)

          traceLog.trace { figure(nonIntersectingCandidates:_*).tagged(s"Left-Justified Reference Candidate Bounds") }

          val referenceMinBounds = nonIntersectingCandidates.map{ bounds =>
            findCandidateMinBounds(bounds)
          }
          traceLog.trace {
            figure(referenceMinBounds:_*).tagged(s"Final Justified Reference Min Bounds (${referenceMinBounds.length})" )
          }
        }

      } else {
        println(s"ERROR: Refblock had neither hanging indent or justified column labels")
      }
    }}
  }

}
