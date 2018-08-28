package edu.umass.cs.iesl.watr
package segment


import geometry._
import geometry.syntax._
import extract._
import shapeless.Lazy
import utils.ExactFloats._
import utils.FunctionalHelpers._
import utils.SlicingAndDicing._
import watrmarks._
import textboxing.{TextBoxing => TB}, TB._
import utils.DoOrDieHandlers._
import utils.SlicingAndDicing._

import TypeTags._
import utils.DisjointSet

trait LineSegmentation extends PageScopeSegmenter
    with FontAndGlyphMetrics
    with TextBlockGrouping { self =>

  import SegmentationSystem._


  // def getBaselineMidrisePageSlice(fontOffsets: FontBaselineOffsets): Option[LTBounds] = {
  //   val baselineMidriseHeight  = fontOffsets.baseLine - fontOffsets.midriseLine
  //   pageHorizontalSlice(
  //     fontOffsets.midriseLine.asDouble(),
  //     baselineMidriseHeight.asDouble()
  //   )
  // }
  // protected def pageHorizontalSlice(top: Double, height: Double): Option[LTBounds] = {
  //   val texact = top.toFloatExact()
  //   val hexact = height.toFloatExact()
  //   val t = max(texact, pageGeometry.top)
  //   val b = min(texact + hexact, pageGeometry.bottom)
  //   pageGeometry.getHorizontalSlice(t, b-t)
  // }
  def getBaselineMidrisePageSlice(fontOffsets: FontBaselineOffsets): Option[LTBounds] = {
    fontOffsets.sliceBetween(
      _.baseLine, _.midriseLine, pageGeometry
    )
  }

  def getAscentDescentPageSlice(fontOffsets: FontBaselineOffsets): Option[LTBounds] = {
    fontOffsets.sliceBetween(
      _.ascentLine, _.descentLine, pageGeometry
    )
    val sliceHeight  = fontOffsets.descentLine - fontOffsets.ascentLine
    pageHorizontalSlice(
      fontOffsets.ascentLine.asDouble(),
      sliceHeight.asDouble()
    )
  }


  def findTextLineShapesFromFontBaselines(): Unit = {
    // mergeFontBaselinePairwise()
    joinFontBaselinesViaPageBands(LB.CharRunFontBaseline, LB.BaselineMidriseBand)
    reindexShapes(LB.Glyph)
    segmentSuperSubScripts()
  }


  def getFontsSortedByHighestOccuranceCount(): Seq[String@@ScaledFontID] ={
    docScope.getFontsWithDocwideOccuranceCounts()
      .sortBy(_._2).reverse.map(_._1)
  }

  def mergeFontBaselinePairwise(): Unit = {
    val fontsByMostOccuring = getFontsSortedByHighestOccuranceCount()

    fontsByMostOccuring.zipWithIndex.foreach { case (scaledFontId, fontIndex) =>

      val fontBaselines = getLabeledLines(LB.CharRunFontBaseline)
      val shapeAndCharsAndScaledFontId = fontBaselines
        .map(l => (l, getCharsForShape(l), getFontsForShape(l).head))
        .sortBy { case (fontBaselineShape, baselineChars, scaledFontId) =>
          baselineChars.head.id
        }

      val fontBaselineSets = DisjointSet.empty[AnyShape]

      shapeAndCharsAndScaledFontId.sliding(3).foreach { case baselineWindow =>
        if (baselineWindow.length == 3) {
          val (fontBaselineShape1, baselineChars1, scaledFontId1) = baselineWindow(0)
          val (fontBaselineShape2, baselineChars2, scaledFontId2) = baselineWindow(1)
          val (fontBaselineShape3, baselineChars3, scaledFontId3) = baselineWindow(2)

          val isBracketedFontWindow = scaledFontId1 == scaledFontId && scaledFontId3 == scaledFontId && scaledFontId2 != scaledFontId

          val winCharsAreConsecutive = (itemsAreConsecutive(baselineChars1.last, baselineChars2.head)
            && itemsAreConsecutive(baselineChars2.last, baselineChars3.head))

          val line1and3AreColinear = fontBaselineShape1.shape.p1.y == fontBaselineShape3.shape.p1.y

          traceLog.trace { shape(fontBaselineShape1, fontBaselineShape2, fontBaselineShape3) tagged s"Trigram FontRunBaselines: isBracketed=${isBracketedFontWindow} isConsecutive=${winCharsAreConsecutive} 1and3 colinear=${line1and3AreColinear}" }

          if (isBracketedFontWindow && winCharsAreConsecutive && line1and3AreColinear) {

            val bracketFontOffsets = docScope.fontDefs.getScaledFontOffsets(scaledFontId)
              .forFontBoxBottom(fontBaselineShape1.shape.p1.y)

            getBaselineMidrisePageSlice(bracketFontOffsets).foreach { slice =>

              val joinedBaselineBand = clipRectBetween(
                fontBaselineShape1.shape.p1.x,
                fontBaselineShape3.shape.p2.x,
                slice
              )

              // traceLog.trace { shape(fontBaselineShape1, fontBaselineShape2, fontBaselineShape3) tagged s"Tri-Window FontRunBaselines: joined=${joinedBaselineBand.nonEmpty}" }

              // val baselineBand = joinedBaselineBand.orDie("Why no baseline band here?")
              joinedBaselineBand.foreach { baselineBand =>
                val baselineMidriseShape = indexShape(baselineBand, LB.BaselineMidriseBand)

                val allBaselineChars = (baselineChars1 ++ baselineChars2 ++ baselineChars3) // .flatMap(getCharsForShape(_))
                  allBaselineChars.foreach { charItem =>
                    val glyphShapes = searchForRects(charItem.minBBox, LB.Glyph)
                    glyphShapes.foreach { glyphShape =>
                      unindexShape(glyphShape)
                    }
                  }

                setFontIndexForShape(baselineMidriseShape, fontIndex)


                fontBaselineSets.union(fontBaselineShape1, fontBaselineShape2)
                fontBaselineSets.union(fontBaselineShape1, fontBaselineShape3)
                fontBaselineSets.union(fontBaselineShape1, baselineMidriseShape)
              }
            }
          }
        }
      }


      fontBaselineSets.sets.foreach { set =>
        val baselineCluster = set.toList
        val baselineMidrisers = baselineCluster.filter{ shape => shape.hasLabel(LB.BaselineMidriseBand) }
        val fontBaselines = baselineCluster.filter{ shape => shape.hasLabel(LB.CharRunFontBaseline) }

        unindexShapes(baselineMidrisers)

        val baselineMidriseSegments = baselineMidrisers.map{ baselineMidriseShape =>
          val fontIndex = getFontIndexForShape(baselineMidriseShape)
          (baselineMidriseShape, fontIndex)
        }
        val minFontIndex = baselineMidriseSegments.map(_._2).min
        val minFontBands = baselineMidriseSegments.filter(_._2 == minFontIndex)

        val charsForFontBaselines = fontBaselines.flatMap{ fontBaseline =>
          getCharsForShape(fontBaseline)
        }.uniqueBy(_.id).sortBy(_.id)

        unindexShapes(fontBaselines)

        val combinedBaselineMidrise = minFontBands.map(_._1.shape.minBounds).reduce(_ union _)
        val baselineMidriseShape = indexShape(combinedBaselineMidrise, LB.BaselineMidriseBand)
        setExtractedItemsForShape(baselineMidriseShape, charsForFontBaselines)
        traceLog.trace { shape(baselineMidriseShape) }
      }
    }
  }



  private def segmentSuperSubScripts(): Unit = {
    val textlineReprShapes = getLabeledRects(LB.BaselineMidriseBand)
    textlineReprShapes.foreach { textlineReprShape =>
      findSuperSubScriptsInLine(textlineReprShape)
    }
  }




  private def findSuperSubScriptsInLine(
    midriseBaselineBand: RectShape
  ): Unit = {
    val primaryFontId = getPrimaryFontForShape(midriseBaselineBand).get
    val midriseBaselineRect  = midriseBaselineBand.shape
    val reprShapeLeft = midriseBaselineRect.left
    val reprShapeRight = midriseBaselineRect.right
    val capDescentRect  = midriseBaselineBand.shape

    def clipped(slice: LTBounds): Option[LTBounds] = {
      // slice.flatMap(clipRectBetween(reprShapeLeft, reprShapeRight, _))
      clipRectBetween(reprShapeLeft, reprShapeRight, slice)
    }

    for {
      offsetsAtLine  <- getFontOffsetsForShape(midriseBaselineBand)
      ascentDescentRect <- offsetsAtLine.sliceBetween(
        _.ascentLine, _.descentLine, pageGeometry
      ).flatMap(clipped(_))

      baselineMidriseHeight = offsetsAtLine.distanceBetween(_.baseLine, _.midriseLine)
      baselineMidriseMidpoint = offsetsAtLine.midriseLine + (baselineMidriseHeight / 2)

      topMidriseBand <- ascentDescentRect.splitHorizontal(baselineMidriseMidpoint)._1
      baselineBottomBand <- ascentDescentRect.splitHorizontal(offsetsAtLine.baseLine)._2

      // (_, maybeBaselineBottomBand) <- midriseBaselineRect.splitHorizontal(offsetsAtLine.baseLine)

      (_, maybeMidriseBaselineCenterBand) = midriseBaselineRect.splitHorizontal(baselineMidriseMidpoint)

      midriseToplineHeight = offsetsAtLine.midriseLine - offsetsAtLine.topLine
      midriseToplineMidpoint = offsetsAtLine.topLine + midriseToplineHeight
      (maybeMidriseToplineBand, _) = ascentDescentRect.splitHorizontal(midriseToplineMidpoint)

       midriseBaselineCenterBand <- maybeMidriseBaselineCenterBand
       midriseToplineCenterBand <- maybeMidriseToplineBand
    } {

      // val topMidriseBand = maybeTopMidriseBand.orDie(s"Could not split CapDescenderBand (${ascentDescentRect}) into top-midrise / __ at ${offsetsAtLine.midriseLine}; ${offsetsAtLine}")
      // val baselineBottomBand = maybeBaselineBottomBand.orDie(s"Could not split CapDescenderBand (${ascentDescentRect}) into __ / baseline-bottom at ${offsetsAtLine.baseLine}; ${offsetsAtLine}")
      // val midriseBaselineCenterBand = maybeMidriseBaselineCenterBand.orDie(s"Could not split MidriseBaselineRect (${midriseBaselineRect}) into __ / midrise-baseline-center at ${baselineMidriseMidpoint.pp}; height:${baselineMidriseHeight.pp} ${offsetsAtLine}")
      // val midriseToplineCenterBand = maybeMidriseToplineBand.orDie(s"Could not split CapDescenderBand (${ascentDescentRect}) into midrise-topline-center / __ at ${midriseToplineMidpoint}; ${offsetsAtLine}")

      traceLog.trace {
        traceLog.figure(topMidriseBand) tagged s"Top-Midrise Band - Strictly above Subscript"
      }
      traceLog.trace {
        traceLog.figure(baselineBottomBand) tagged s"Baseline-Bottom Band - Intersects Subscript"
      }
      // traceLog.trace {
      //   traceLog.figure(midriseBaselineCenterBand) tagged s"Midrise-Baseline Center -> Bottom Band - Strictly below Superscript"
      // }
      // traceLog.trace {
      //   traceLog.figure(midriseToplineCenterBand) tagged s"Midrise-Topline Center -> Topline Band - Intersects Superscript"
      // }

      val charsInBand = getCharsForShape(midriseBaselineBand)

      // superscript = strictly above the midrise-baseline center line && intersects midrise-topline center -> topline band
      val eitherSuperScriptOrNot = collectSpanEither[ExtractedItem.CharItem](charsInBand, { c =>

        val charFontMidpoint = c.fontBbox.toPoint(Dir.Center)
        val fontIsAboveBaseline = offsetsAtLine.baseLine > c.fontBbox.bottom
        val isRaised = midriseBaselineCenterBand.isStrictlyBelow(charFontMidpoint.y)
        val intersects =  midriseToplineCenterBand.intersects(c.fontBbox)

        (c.scaledFontId != primaryFontId
            && isRaised
            && intersects
            && fontIsAboveBaseline
            && !c.isLigature)
      })



      val superScriptChars = eitherSuperScriptOrNot.collect { case Right(i) => i }

      superScriptChars.foreach { charSpan =>
        val minBounds = charSpan.map(_.minBBox).reduce(_ union _)
        traceLog.trace {
          traceLog.figure(minBounds) tagged s"Superscript MinBounds"
        }
      }

      // subscript = strictly below midriseLine && intersect (baseline+delta)-descender band
      val eitherSubScriptOrNot = collectSpanEither[ExtractedItem.CharItem](charsInBand, { c =>
        // val charFontOffsets = docScope.fontDefs.getScaledFontOffsets(c.scaledFontId)
        // val charOffsetsAtBaseline = charFontOffsets.forFontBoxBottom(c.fontBbox.bottom)
        // val charMidriseLine = charOffsetsAtBaseline.midriseLine

        // val charMidpoint = c.minBBox.toPoint(Dir.Center)
        val charMidpoint = c.fontBbox.toPoint(Dir.Center)
        val fontIsBelowBaseline = offsetsAtLine.baseLine < c.fontBbox.bottom
        // val isLowered = c.minBBox.isStrictlyBelow(topMidriseBand)
        val isLowered = topMidriseBand.isStrictlyAbove(charMidpoint.y)
        val intersectsBaselineBottom = c.minBBox.intersects(baselineBottomBand)
        (c.scaledFontId != primaryFontId
          && isLowered
          && intersectsBaselineBottom
          && fontIsBelowBaseline
          && !c.isLigature)
      })

      import utils.intervals._

      def beginLenInterval(b: Int, l: Int, label: Label): Interval[Int, Label] = {
        Interval.bounded.create.leftClosedRightOpen(b, b+l).withAttr[Label](label)
      }

      val init = (0, List[Interval[Int, Label]]())

      val (_, superScriptSpansRev) = eitherSuperScriptOrNot.foldLeft(init) { case ((offset, spans), e) =>
        e match {
          case Right(v) =>
            val sp = beginLenInterval(offset, v.length, LB.Sup) :: spans
            val newOffset= offset + v.length
            (newOffset, sp)
          case Left(v) =>
            val newOffset = offset + v.length
            (newOffset, spans)
        }
      }

      val superScriptSpans  = superScriptSpansRev.reverse

      val (_, subScriptSpansRev) = eitherSubScriptOrNot.foldLeft(init) { case ((offset, spans), e) =>
        e match {
          case Right(v) =>
            val sp = beginLenInterval(offset, v.length, LB.Sub) :: spans
            val newOffset= offset + v.length
            (newOffset, sp)
          case Left(v) =>
            val newOffset= offset + v.length
            (newOffset, spans)
        }
      }
      val subScriptSpans  = subScriptSpansRev.reverse

      val scriptSpans = (subScriptSpans ++ superScriptSpans).sorted(Interval.defaultIntervalOrdering[Int, Label]())
      if (scriptSpans.nonEmpty) {
        setLabeledIntervalsForShape(midriseBaselineBand, scriptSpans)
      }
      val subScriptChars = eitherSubScriptOrNot.collect { case Right(i) => i }

      subScriptChars.foreach { charSpan =>
        val minBounds = charSpan.map(_.minBBox).reduce(_ union _)
        traceLog.trace {
          traceLog.figure(minBounds) tagged s"SubScript MinBounds"
        }
      }
    }

  }


  /**
    * Number columns:
    * - no punctuation (e.g., (1) [1] 1. )
    * - strictly increasing
    * - each page starts at 1 or last page max+1
    * - alignment is centered on midrise-band
    * - tend towards extreme left/right of page
    * - font may be unique to numbering
    * - relatively constant spacing to page edge
    * - wide whitespace margins

    * Marginalia removal
    *
    **/


  /**
    * Search-base line joining
    */
  def joinFontBaselinesViaPageBands(startingShapeLabel: Label, outputLabel: Label): Unit = {

    def _loop(
      scaledFontIds: List[String@@ScaledFontID],
      lineShapes: Seq[LineShape],
      depth: Int = 0
    ): Unit = scaledFontIds match {

      case headFontId :: tailFontIds =>
        val (linesForFont, others) = lineShapes.partition{ lineShape =>
          getFontsForShape(lineShape).contains(headFontId)
        }

        val allAdjustedOffsets = linesForFont.map{ lineShape =>
          val line = lineShape.shape
          val fontOffsets = docScope.fontDefs.getScaledFontOffsets(headFontId)
          (lineShape, fontOffsets.forFontBoxBottom(line.p1.y))
        }

        val linesAndOffsetsAndHeadChar = allAdjustedOffsets.map{ case (lineShape, offsetsAtLine) =>
          val lineChars = getCharsForShape(lineShape)
          (lineShape, offsetsAtLine, lineChars.head)
        }

        linesAndOffsetsAndHeadChar.foreach { case (lineShape, offsetsAtLine, headChar) =>
          getAscentDescentPageSlice(offsetsAtLine).foreach{ slice =>
            findLineCharsInPageBandHighPrecision(slice, headChar, outputLabel)
          }
        }

        _loop(tailFontIds, others, depth+1)

      case Nil =>
    }

    val startingLines = getLabeledLines(startingShapeLabel)

    val fontsByMostOccuring = getFontsSortedByHighestOccuranceCount()
    _loop(fontsByMostOccuring.toList, startingLines)
  }

  def indexPathRegions(): Unit = {
    val pathShapes = pageScope.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.PathItem] }
      .map { item =>
        indexShape(item.minBBox, LB.PathBounds)
      }

    // traceLog.trace { shape(pathShapes:_*) tagged "Path Line Bounds" }

  }

  private def findLineCharsInPageBandHighPrecision(
    pageSlice: LTBounds,
    rootChar: ExtractedItem.CharItem,
    outputLabel: Label
  ): Option[RectShape] = {
    val glyphsInBand = searchForRects(pageSlice, LB.Glyph)

    val glyphsWithChar = glyphsInBand.map { g =>
      (g, getCharsForShape(g).head)
    }

    glyphsInBand.flatMap { g =>
      getLinkedShape(g, LB.CharRunFontBaseline)
    }.uniqueBy(_.id)


    val orderedById = glyphsWithChar.sortBy{ case(glyphShape, charItem) =>
      charItem.id
    }

    val consecutiveById = orderedById.groupByPairs { case ((_, char1), (_, char2)) =>
      itemsAreConsecutive(char1, char2)
    }

    val indexedSets = consecutiveById.zipWithIndex

    val setWithRootChar = indexedSets.filter { case (charSet, setNum) =>
      charSet.map(_._2.id).contains(rootChar.id)
    }

    if (setWithRootChar.nonEmpty) {
      val setNum = setWithRootChar.head._2
      val charSetWithRootChar = setWithRootChar.flatMap(_._1.map(_._2))
      val orderedLeftToRight = charSetWithRootChar.sortBy{ _.minBBox.left }
      val sameOrderByIdAndByLeftToRight = charSetWithRootChar
        .zip(orderedLeftToRight)
        .forall{ case (char1, char2) => char1 == char2 }

      val allItemsAreConsecutive = consecutiveById.length == 1


      // println(s"ord.by.id  : ${orderedById.map(_._2.char).mkString}")
      // println(s" (w/root)  : ${charSetWithRootChar.map(_.char).mkString}")
      // println(s"ord.l->r   : ${orderedLeftToRight.map(_.char).mkString}")

      if (sameOrderByIdAndByLeftToRight || allItemsAreConsecutive) {
        val rootFontOffsets = docScope.fontDefs.getScaledFontOffsets(rootChar.scaledFontId)
          .forFontBoxBottom(rootChar.fontBbox.bottom)

        val fontIds = charSetWithRootChar.map(_.scaledFontId).toSet
        val charBounds = charSetWithRootChar.map(_.minBBox).reduce(_ union _)
        val charSetText = charSetWithRootChar.map(_.char).mkString

        rootFontOffsets.sliceBetween(_.baseLine, _.midriseLine, pageGeometry)
          .flatMap { baselineMidriseSlice =>

            val baselineMidriseRect = clipRectBetween(
              charBounds.left, charBounds.right,
              baselineMidriseSlice
            )

            setWithRootChar.foreach { case (charSet, setNum) =>
              charSet.foreach { case (glyphShape, charItem) =>
                unindexShape(glyphShape)
              }
            }

            baselineMidriseRect.map{ baselineMidrise =>

              val pageBand = indexShape(baselineMidrise, outputLabel).asRectShape

              traceLog.trace { shape(pageBand) }

              setExtractedItemsForShape(pageBand, charSetWithRootChar)
              setFontsForShape(pageBand, fontIds)
              setPrimaryFontForShape(pageBand, rootChar.scaledFontId)
              setFontOffsetsForShape(pageBand, rootFontOffsets)
              pageBand
            }
          }
      } else None
    } else None
  }


  def clipRectBetween(x1: Int@@FloatRep, x2: Int@@FloatRep, rect: LTBounds): Option[LTBounds] = {
    for {
      rightHalf <- rect.splitVertical(x1)._2
      leftHalf <- rightHalf.splitVertical(x2)._1
    } yield leftHalf
  }



  private def findLineCharsInPageBand(pageSlice: LTBounds, rootChar: ExtractedItem.CharItem, outputLabel: Label): Option[RectShape] = {
    val glyphsInBand = searchForRects(pageSlice, LB.Glyph)

    val glyphsWithChar = glyphsInBand.map { g =>
      (g, getCharsForShape(g).head)
    }

    // var dbgGrid = Lazy[TB.Grid]{ TB.Grid.widthAligned() }

    // dbgGrid = dbgGrid.map { grid =>
    //   var g = TB.Grid.widthAligned(
    //     (8, AlignRight), // h-dist
    //     (1, AlignLeft),  // space
    //     (1, AlignLeft),  // line text (char(s))
    //     (1, AlignLeft),  // space
    //   )

    //   g = g.addRow(
    //     "H-Dst|||",
    //     " ",
    //     ">",
    //     " "
    //   )
    //   g = g.addRow("        ", " ", " ", " ")
    //   g
    // }


    val orderedById = glyphsWithChar.sortBy{ case(glyphShape, charItem) =>
      // (glyphShape.shape.left, charItem.id)
      charItem.id
    }


    val rootCharOffsets = docScope.fontDefs.getScaledFontOffsets(rootChar.scaledFontId)
    val capDescentDist = rootCharOffsets.descentLine - rootCharOffsets.capLine
    val whitespaceCutoff = capDescentDist * 3

    val dbgChars = orderedById.map(_._2.char).mkString
    println(s"dbgString> ${dbgChars}")

    val consecutiveSets = orderedById.groupByPairs { case ((bbox1, char1), (bbox2, char2)) =>
      val hdist = bbox2.shape.left - bbox1.shape.right
      val hdist2 = bbox2.shape.left - bbox1.shape.right

      val isClose = hdist < whitespaceCutoff
      val isConsecutive = char1.id.unwrap == char2.id.unwrap - 1

      isClose && isConsecutive
    }

    val hDists: Seq[Int@@FloatRep] = orderedById.sliding(2).toList.map { w => w.toList match {
      case (bbox1, char1) :: (bbox2, char2) :: Nil => bbox2.shape.left - bbox1.shape.right
      case _ => FloatExact.zero
    } }


    val indexedSets = consecutiveSets.zipWithIndex
    indexedSets.foreach {  case (charSet, setNum) =>
      val hasRoot = charSet.map(_._2.id).contains(rootChar.id)
      val hasRootStr = if (hasRoot) "*" else " "
      val setChars = charSet.map{ case (glyphShape, charItem) =>
        charItem.char
      }
      println(s"set ${setNum}${hasRootStr}> ${setChars.mkString}")
    }

    val setWithRootChar = indexedSets.filter { case (charSet, setNum) =>
      charSet.map(_._2.id).contains(rootChar.id)
    }

    if (setWithRootChar.nonEmpty) {
      val setNum = setWithRootChar.head._2
      val lenBeforeRoot = indexedSets.takeWhile(_._2 < setNum).map(_._1.length).sum
      val lenAfterRoot = indexedSets.drop(setNum+1).map(_._1.length).sum
      val indicator = (" "*lenBeforeRoot) + ("="*setWithRootChar.head._1.length) + (" "*lenAfterRoot)

      val charSetWithRootChar = setWithRootChar.flatMap(_._1.map(_._2))

      // orderedById.zip(hDists :+ FloatExact.zero).zip(indicator.toList)
      //   .foreach{ case (((glyphShape, charItem), hDist), indicatorSym) =>
      //     dbgGrid = dbgGrid.map { grid =>
      //       grid.addRow(
      //         hDist.pp(),
      //         "",
      //         charItem.char.toString(),
      //         indicatorSym.toString(),
      //       )
      //     }
      //   }
      // println("Debug Grid")
      // println(dbgGrid.value.toBox().transpose())


      setWithRootChar.foreach { case (charSet, setNum) =>
        charSet.foreach { case (glyphShape, charItem) =>
          println(s"Unindexing ${charItem.char} : ${glyphShape}")
          unindexShape(glyphShape)
        }
      }

      val fontIds = charSetWithRootChar.map(_.scaledFontId).toSet
      val charBounds = charSetWithRootChar.map(_.minBBox).reduce(_ union _)
      val charSetText = charSetWithRootChar.map(_.char).mkString

      traceLog.trace {
        figure(charBounds) tagged s"Char Bounds For Set With Root '${rootChar.char}' In '${charSetText}'"
      }

      traceLog.trace {
        figure(pageSlice) tagged s"Page Slice For Grouping Glyphs With Root '${rootChar.char}' In '${charSetText}'"
      }

      val charBoundsWithinSlice = charBounds.withinRegion(pageSlice)
        .adjacentRegions(Dir.Top, Dir.Center, Dir.Bottom)

      charBoundsWithinSlice.map{ textRegion =>

        traceLog.trace {
          figure(textRegion) tagged s"Text Region Clipped to Page Slice For Grouping Glyphs With Root '${rootChar.char}' In '${charSetText}'"
        }

        val pageBand = indexShape(textRegion, outputLabel).asRectShape
        setExtractedItemsForShape(pageBand, charSetWithRootChar)
        setFontsForShape(pageBand, fontIds)
        pageBand
      }

    } else None
  }











  ////


  private def indexImageRegionsAndDeleteOverlaps(): Unit = {
    val deletedShapes = pageScope.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.ImgItem] }
      .flatMap { imageItem =>
        indexShape(imageItem.minBBox, LB.Image)
        val baseLines: Seq[LineShape] = searchForLines(imageItem.minBBox, LB.CharRunFontBaseline)
        deleteShapes(baseLines)
        baseLines
      }

    traceLog.trace { shape(deletedShapes:_*) tagged "Deleted Intersect Image Bounds" }
    traceLog.trace { labeledShapes(LB.Image) tagged "Image Regions" }
  }

  private def createCharRunFontBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted = charRun.sortBy { _.minBBox.left }
    val runBeginPt =  Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }


  private def findNatLangBaselineRuns(retainNatLang: Boolean): Seq[Seq[ExtractedItem.CharItem]] = {
    pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filter(_.fontProps.isNatLangFont() == retainNatLang)
      .groupByPairs { case (item1, item2) =>
        item2.glyphProps.prevSimilar == item1.id
      }
  }

  private def findSymbolicCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filterNot(_.fontProps.isNatLangFont())
      .groupByPairs {
        case (item1, item2) =>
          lazy val consecutive = itemsAreConsecutive(item1, item2)
          lazy val leftToRight = item1.minBBox.left < item2.minBBox.left
          lazy val colinear = item1.minBBox.isNeitherAboveNorBelow(item2.minBBox)

          consecutive && leftToRight && colinear
      }

    charRuns
  }

  private def combineCombiningMarks(): Unit = {
    val combiningMarks = pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CombiningMark => item }

    combiningMarks.foreach { combiningMark =>
      val cmShape = indexShapeAndSetItems(combiningMark.minBBox, LB.Glyph, combiningMark)
      traceLog.trace { shape(cmShape) }
    }
  }


  def findContiguousGlyphSpans(): Unit = {

    recordNatLangCharSpans(
      LB.CharRunFontBaseline,
      findNatLangBaselineRuns(retainNatLang=true)
    )


     // combineCombiningMarks()

    indexPathRegions()

    indexImageRegionsAndDeleteOverlaps()

    val symbolicLangCharRuns = findSymbolicCharRuns()

    symbolicLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.SymbolicGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }
    }

    traceLog.trace { labeledShapes(LB.NatLangGlyph) tagged "All Natural Lang Glyph Rects" }
    traceLog.trace { labeledShapes(LB.SymbolicGlyph) tagged "All Symbolic Glyph Rects" }

    initSymbolicCharSpans(symbolicLangCharRuns)

  }



  private def initSymbolicCharSpans(symbolicRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    symbolicRuns.map { charRun =>
      val xSorted = charRun.sortBy { _.minBBox.left }
      val p1 = xSorted.head.minBBox.toPoint(Dir.Center)
      val p2 = xSorted.last.minBBox.toPoint(Dir.Center)

      val baseLine = Line(p1, p2)

      val symbolicGlyphLine = indexShape(baseLine, LB.SymbolicGlyphLine)

      setExtractedItemsForShape(symbolicGlyphLine, charRun)

      symbolicGlyphLine
    }.asLineShapes


    traceLog.trace { labeledShapes(LB.SymbolicGlyphLine) }
  }

  private def recordNatLangCharSpans(spanLabel: Label, natLangCharRuns: Seq[Seq[ExtractedItem.CharItem]]): Unit = {

    natLangCharRuns.foreach { charRun =>
      val charItems = charRun.map(_.asInstanceOf[ExtractedItem.CharItem])

      val glyphShapes = charItems.map { item =>
        indexShapeAndSetItems(item.minBBox, LB.NatLangGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }

      val baseLine = createCharRunFontBaseline(charItems)

      val baselineShape = indexShape(baseLine, spanLabel)

      glyphShapes.foreach { headGlyphShape =>
        setLinkedShape(headGlyphShape, LB.CharRunFontBaseline, baselineShape)
      }

      val fontIds = charItems.map{ _.scaledFontId }.toSet

      setFontsForShape(baselineShape, fontIds)

      setExtractedItemsForShape(baselineShape, charRun)

      baselineShape
    }

    traceLog.trace { labeledShapes(spanLabel) tagged "Initial Font Baselines" }
  }



}
