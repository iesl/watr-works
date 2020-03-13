package org.watrworks
package segment

import scala.{ collection => sc }
import sc.Seq

import geometry._
import geometry.syntax._
import extract._
import utils.ExactFloats._
import utils.FunctionalHelpers._
import utils.SlicingAndDicing._
import watrmarks._
import annots._
import corpora._
import textboxing.{TextBoxing => TB}, TB._
import utils.SlicingAndDicing._
import utils.DoOrDieHandlers._
import utils.QuickNearestNeighbors._
import textgrid._

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
    with TextReconstruction
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

    val fontBaselines = searchForRects(queryRect, LB.BaselineMidriseBand)
    val ySortedBaselines = fontBaselines.sortBy(_.shape.bottom)

    val leftmostCharBounds = ySortedBaselines.map { fontBaseline =>
      val baselineItems = getCharsForShape(fontBaseline)
      val includedCharItems = baselineItems.filter{item =>

        queryRect.intersects(item.minBBox)
      }
      if (includedCharItems.nonEmpty) {
        val leftmostItem = includedCharItems.minBy(_.minBBox.left)
        val LTBounds(l, t, w, h) = leftmostItem.minBBox

        if (docScope.fontDefs.hasScaledFontOffsets(leftmostItem.scaledFontId)) {
          val fontOffsets = docScope.fontDefs.getScaledFontOffsets(leftmostItem.scaledFontId)
          val baselineOffsets = fontOffsets.forFontBoxBottom(leftmostItem.fontBbox.bottom)
          val baseline = baselineOffsets.baseLine
          val capline = baselineOffsets.capLine
          val adjHeight = baseline - capline
          val adjustedBbox = if (adjHeight.unwrap == 0) {
            leftmostItem.minBBox
          } else {
            LTBounds(l, capline, w, adjHeight)
          }

          Some(adjustedBbox)

        } else  {
          Some(leftmostItem.minBBox)
        }
      } else None
    }.flatten

    val groupedByLine = leftmostCharBounds.groupByPairs { case (bbox1, bbox2) =>
      bbox1.isNeitherAboveNorBelow(bbox2)
    }

    groupedByLine.map{ line =>
      line.sortBy(_.left).head
    }
  }

  def convertReferenceBlocks(pageZonesAndLabels: Seq[(AnnotatedLocation.Zone, Label)]): Option[Seq[(LTBounds, TextGrid, PageRegion)]] = {
    pageZonesAndLabels.foreach { case (zone: AnnotatedLocation.Zone, label: watrmarks.Label) =>
      assume(zone.regions.length == 1)
      val zoneRegion = zone.regions.head
      val zoneBounds = zoneRegion.bbox
      val zoneShape = indexShape(zoneBounds, label)

      traceLog.trace {
        shape(zoneShape).tagged(s"RefBlock Human Labeled ${label}")
      }
    }

    // val zonesByLabel = pageZonesAndLabels.groupBy(_._2).mapValues(_.map(_._1))
    val zonesByLabel = pageZonesAndLabels
      .groupBy(_._2).view
      .mapValues(_.map(_._1))
      .toMap

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

        traceLog.trace { figure(finalSlices).tagged(s"Hanging Indent Slices: ${finalSlices.length}") }
        traceLog.trace { figure(finalBounds).tagged(s"Hanging Indent Final Bounds") }


        val nonIntersectingCandidates = filterPrelabeledRegions(refBlockRegion.bbox, referenceCandidates)

        traceLog.trace {
          figure(nonIntersectingCandidates).tagged(s"Non intersecting Reference Candidates (${nonIntersectingCandidates.length})")
        }

        val referenceMinBounds = nonIntersectingCandidates.map{ candidateBounds =>
          findCandidateMinBounds(candidateBounds)
        }

        referenceMinBounds.foreach { refMinBounds =>
          indexShape(refMinBounds, Reference)
        }

        traceLog.trace {
          figure(referenceMinBounds).tagged(s"Final Hanging Reference Min Bounds (${referenceMinBounds.length})" )
        }

      } else if (leftJustifiedColumnShapes.nonEmpty) {


        // Case 2. LeftJustifiedColumn exists

        val leftJustifiedColumnShape = leftJustifiedColumnShapes.head
        val leftmostCharBounds = queryForLeftmostCharsInColumn(leftJustifiedColumnShape.bounds, refBlockRegion.bbox)
        if (leftmostCharBounds.isEmpty) {
          println(s"Warning: no chars found in left-justified column")

        } else {


          val leftmostCharBottoms = leftmostCharBounds.map(_.bottom)
          val yDeltas = findDeltas(leftmostCharBottoms)
          val yJumpClusters = qnn(yDeltas, tolerance = 1d)
          val yJumpBins = yJumpClusters.filter { bin =>
            bin.size() > 0
          }
          val sortedYJumps = yJumpBins.sortBy(_.size()).reverse

          // println(s"sortedYJumps: Sorted Bins")
          // println(sortedYJumps.mkString("\n  ", "\n  ", "\n"))

          // println(s"leftmostChar bounds")
          // println(leftmostCharBounds.map(_.prettyPrint).mkString("\n  ", "\n  ", "\n"))

          // println(s"leftmostCharBottoms")
          // println(leftmostCharBottoms.map(_.pp).mkString("\n  ", "\n  ", "\n"))

          // println(s"yDeltas")
          // println(yDeltas.map(_.pp).mkString("\n  ", "\n  ", "\n"))

          val maxJumpForIntraRefLines = sortedYJumps.headOption.map(_.maxValue()).orDie("no y-jump val 1 clusters??")

          val maybeMinJumpForInterRefLines = sortedYJumps.drop(1).headOption.map(_.minValue())

          if (maybeMinJumpForInterRefLines.isEmpty) {
            println("No suitable y-jumps for inter-reference gaps")
          }

          maybeMinJumpForInterRefLines.map{ minJumpForInterRefLines =>
            val jumpThreshold = ((maxJumpForIntraRefLines + minJumpForInterRefLines) / 2) + 0.1d
            // val jumpThreshold = minJumpForInterRefLines - 1d

            // println(s"Jump threshold: ${jumpThreshold};  max-intra/min-inter = (${maxJumpForIntraRefLines}, ${minJumpForInterRefLines})")

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

            traceLog.trace { figure(nonIntersectingCandidates).tagged(s"Left-Justified Reference Candidate Bounds") }

            val referenceMinBounds = nonIntersectingCandidates.map{ bounds =>
              findCandidateMinBounds(bounds)
            }

            referenceMinBounds.foreach { refMinBounds =>
              indexShape(refMinBounds, Reference)
            }

            traceLog.trace {
              figure(referenceMinBounds).tagged(s"Final Justified Reference Min Bounds (${referenceMinBounds.length})" )
            }
          }
        }

      } else {
        println(s"ERROR: Refblock had neither hanging indent or justified column labels")
      }
    }}

    val autoLabeledBlocks = referenceBlockZones.map { zones => zones.flatMap { refBlockZone =>
      val refBlockRegion = refBlockZone.regions.headOption.orDie(s"no regions found in zone ${refBlockZone}")
      val stablePage = refBlockRegion.page
      val referenceShapes = searchForRects(refBlockRegion.bbox, Reference)
      val sortedRefShapes = referenceShapes.map{ referenceShape =>
        val textGrid = getTextGrid(Some(referenceShape.shape))
        (referenceShape.shape, textGrid, refBlockRegion)
      }.sortBy(_._1.top)


      sortedRefShapes
    }}

    val checkedBlocks = autoLabeledBlocks.map{ block  =>
      val isSane = sanityCheckReferences(block)
      val marker = if (isSane) "#" else "*"

      block.foreach{ case (referenceShape, textGrid, pageRegion) =>
        println(
          TB.borderLeft(s"##${marker}>> ")(
            s"-- Reference ---".hangIndent(
              textGrid.toText()
            )
          )
        )
      }
      (block, isSane)
    }

    checkedBlocks.filter(_._2).map(_._1)
  }

  def sanityCheckReferences(referenceTextGrids: Seq[(LTBounds, TextGrid, PageRegion)]): Boolean = {
    referenceTextGrids.zipWithIndex.foreach{ case ((bbox, textGrid, pageRegion), refNum) =>
      val isEmpty = textGrid.toText().replaceAll("\n", " ").trim.isEmpty()

      if (isEmpty) {
        println(s"Warning: no text found for reference ${refNum} on page ${pageRegion.page.pageNum}")
      }
    }

    val refTexts = referenceTextGrids.flatMap{ case (bbox, textGrid, pageRegion) =>
      textGrid.toText().split("\n").headOption
    }


    val numericRef1 = """^\[([\d ]+)\].*""".r
    def isNumericMarker1(s: String) = numericRef1.findFirstIn(s).nonEmpty
    def getNumericMarker1(s: String) = numericRef1.findFirstMatchIn(s).map { m => m.group(1) }

    val numericRef2 = """^([\d ]+).*""".r
    def isNumericMarker2(s: String) = numericRef2.findFirstIn(s).nonEmpty
    def getNumericMarker2(s: String) = numericRef2.findFirstMatchIn(s).map { m => m.group(1) }

    val alphaMarker = """^([a-zA-Z]+).*""".r
    def isAlphabeticMarker(s: String) = alphaMarker.findFirstIn(s).nonEmpty
    def getAlphabeticMarker(s: String) = alphaMarker.findFirstMatchIn(s).map { m => m.group(1) }

    val allNumeric1 = refTexts.forall(isNumericMarker1(_))
    val allNumeric2 = refTexts.forall(isNumericMarker2(_))
    val allAlpha = refTexts.forall(isAlphabeticMarker(_))


    if (allAlpha) {
      println(s"allAlpha")

      val markers = refTexts.map{ refText =>
        getAlphabeticMarker(refText)
      }
      val refMarkerText = markers.flatten

      val foundAllMarkers = refMarkerText.length == markers.length

      val commonNameConnectors = "de du von van".split(" ").toList

      val isIncreasingSeq = refMarkerText.sliding(2).forall { pairs =>
        if (pairs.length==2) {

          val line1 = pairs.head
          val line2 = pairs.last
          val minLen = math.min(line1.length(), line2.length())
          val linesHaveConnectors = (
            commonNameConnectors.exists(line1.toLowerCase.startsWith(_))
              || commonNameConnectors.exists(line2.toLowerCase.startsWith(_)))

          val maybeFineAnway = (
            line1.head.isUpper && line2.head.isUpper &&
              line1.contains(".,") &&
              line2.contains(".,") &&
              math.abs(line1.head - line2.head) <= 1
          )

          val fst = pairs.head.slice(0, minLen)
          val lst = pairs.last.slice(0, minLen)
          val isOrdered = fst.compareTo(lst) <= 0

          if (!isOrdered) {
            println(s"Warning: ordering between (haveConnectors: ${linesHaveConnectors}, maybeFineAnway: ${maybeFineAnway}) \n   ${line1} \n   ${line2}")
          }

          isOrdered || linesHaveConnectors || maybeFineAnway
        } else true
      }

      if (foundAllMarkers && isIncreasingSeq) {
        println("Sanity Check passed")
        true
      } else {
        println("FAILED Sanity Check")
        val refDbg = refTexts.mkString("{\n  ", "\n  ", "\n}")
        val dbg = markers.mkString("{\n  ", "\n  ", "\n}")
        println(s"refs = ${refDbg}")
        println(s"markers = ${dbg}")
        false
      }

    } else if (allNumeric1) {
      val markers = refTexts.map{ refText =>
        getNumericMarker1(refText)
      }

      val refMarkerText = markers.flatten.map{ marker =>
        marker.replaceAll(" ", "")
      }

      val foundAllMarkers = refMarkerText.length == markers.length
      val isIncreasingSeq = refMarkerText.map{_.toInt}.sliding(2).forall { pairs =>
        if (pairs.length==2) {
          pairs.head == pairs.last - 1
        } else true
      }


      if (foundAllMarkers && isIncreasingSeq) {
        println("Sanity Check passed (numeric 1)")
        true
      } else {
        println("FAILED Sanity Check (numeric 1)")
        val refDbg = refTexts.mkString("{\n  ", "\n  ", "\n}")
        val dbg = markers.mkString("{\n  ", "\n  ", "\n}")
        println(s"refs = ${refDbg}")
        println(s"markers = ${dbg}")
        false
      }

    } else if (allNumeric2) {
      val markers = refTexts.map{ refText =>
        getNumericMarker2(refText)
      }

      val refMarkerText = markers.flatten.map{ marker =>
        marker.replaceAll(" ", "")
      }

      val foundAllMarkers = refMarkerText.length == markers.length
      val isIncreasingSeq = refMarkerText.map{_.toInt}.sliding(2).forall { pairs =>
        if (pairs.length==2) {
          pairs.head == pairs.last - 1
        } else true
      }


      if (foundAllMarkers && isIncreasingSeq) {
        println("Sanity Check passed (numeric marker 2)")
        true
      } else {
        println("FAILED Sanity Check (numeric marker 2)")
        val refDbg = refTexts.mkString("{\n  ", "\n  ", "\n}")
        val dbg = markers.mkString("{\n  ", "\n  ", "\n}")
        println(s"refs = ${refDbg}")
        println(s"markers = ${dbg}")
        false
      }

    } else {
      println(s"could not identify reference numbering")
      false
    }

  }


}
