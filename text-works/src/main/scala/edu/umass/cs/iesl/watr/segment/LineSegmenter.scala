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

trait LineSegmentation extends PageScopeSegmenter
    with FontAndGlyphMetrics
    with TextBlockGrouping { self =>

  import SegmentationSystem._

  lazy val lineSegmenter = self

  def doLineJoining(focalFont: String@@ScaledFontID, baselineShape: LineShape): Unit = {
    println(s"doLineJoining(${focalFont}, ${baselineShape})")
    val baselineFonts = getFontsForShape(baselineShape)


    assume(baselineFonts.size==1)
    assume(baselineFonts.contains(focalFont))

    val line = baselineShape.shape
    // val scaledFontMetrics = docScope.fontDefs.getScaledFont(focalFont).orDie(s"font ${focalFont} not found")
    val charItems = getCharsForShape(baselineShape)

    // val metrics = scaledFontMetrics.fontMetrics.orDie(s"font metrics ${focalFont} not found")
    // val headItem = charItems.head
    // val trueBaseline = 0
    // headItem.char
    // headItem.fontProps
    // headItem.glyphProps.scalingFactor


    val heights = charItems.map{ item =>
      item.minBBox.height
    }
    // TODO get doc-wide avg or max height for these chars
    val maxHeight = heights.max
    val avgWidth = charItems.map{ _.minBBox.width.asDouble() }.sum / charItems.length
    val windowWidth = avgWidth*6
    val windowDelta = avgWidth*4


    for {
      baselineSlice <- pageHorizontalSlice(line.p1.y.asDouble()-maxHeight.asDouble(), maxHeight.asDouble())
    } {

      val windows = baselineSlice.slidingHorizontalWindow(windowWidth, windowDelta)
      var firstNLGlyphWin = Int.MaxValue
      windows.zipWithIndex.foreach { case (window, winNum) =>
        val glyphsInWindowNL = searchForRects(window, LB.NatLangGlyph)
        val glyphsInWindowSymbolic = searchForRects(window, LB.SymbolicGlyph)

        val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
          glyphsInWindowSymbolic.exists { glyphShape =>
            shapeIndex.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
          }
        }

        if (someGlyphIsNLOrRootedToNL) {
          val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowSymbolic

          val glyphItems = glyphsInWindow.map{ g =>
            getExtractedItemsForShape(g).head
          }

          val ids = glyphItems.filter(_ != null)
            .map{ g => g.id.unwrap }
            .sorted
            .toList

          val idRange = (ids.min to ids.max).toList
          val glyphsAreConsecutive = idRange == ids
          if (glyphsAreConsecutive) {
            firstNLGlyphWin = math.min(firstNLGlyphWin, winNum)
            clusterN(LB.ContiguousGlyphs, glyphsInWindow)
          }
        }
      }

      val revWindows = windows.slice(0, firstNLGlyphWin+3).reverse

      revWindows.zipWithIndex.foreach { case (window, winNum) =>
        val glyphsInWindowNL = searchForRects(window, LB.NatLangGlyph)
        val glyphsInWindowSymbolic = searchForRects(window, LB.SymbolicGlyph)

        val someGlyphIsNLOrRootedToNL = glyphsInWindowNL.nonEmpty || {
          glyphsInWindowSymbolic.exists { glyphShape =>
            shapeIndex.getClusterRoot(LB.ContiguousGlyphs, glyphShape).isDefined
          }
        }

        if (someGlyphIsNLOrRootedToNL) {
          val glyphsInWindow = glyphsInWindowNL ++ glyphsInWindowSymbolic

          val glyphItems = glyphsInWindow.map{ g =>
            getExtractedItemsForShape(g).head
          }


          val ids = glyphItems.filter(_ != null)
            .map{ g => g.id.unwrap }
            .sorted
            .toList

          val idRange = (ids.min to ids.max).toList

          val glyphsAreConsecutive = idRange == ids

          if (glyphsAreConsecutive) {
            clusterN(LB.ContiguousGlyphs, glyphsInWindow)
          }
        }
      }
    }

    val allClusterBounds = getClusteredRects(LB.ContiguousGlyphs).map{
      case (clusterReprId, glyphRects) =>
        val glyphBounds = glyphRects.map(_.shape).reduce(_ union _)
        val glyphItems = getExtractedItemsForShapes(glyphRects).flatten
        val continuousGlyphBaseline = glyphBounds.toLine(Dir.Bottom)
        val baselineShape = indexShape(continuousGlyphBaseline, LB.ContiguousGlyphBaseline)
        setExtractedItemsForShape(baselineShape, glyphItems)
        glyphBounds
    }

    traceLog.trace {
      val bottomLines = allClusterBounds.map(_.toLine(Dir.Bottom))
      figure(bottomLines:_*) tagged "ContiguousGlyphBounds"
    }

    println(s"doLineJoining(): done")
  }

  def joinFontBaselines(label: Label): Unit = {
    val fontsByMostOccuring = docScope.getFontsWithDocwideOccuranceCounts()
      .sortBy(_._2).reverse.map(_._1)

    //... Filter to fonts on page

    shapeIndex.ensureCluster(LB.ContiguousGlyphs)

    def joinLinesLoop(
      scaledFontIds: List[String@@ScaledFontID],
      lineShapes: Seq[LineShape],
      depth: Int = 0
    ): Unit = scaledFontIds match {

      case headFontId :: tailFontIds =>
        println(s"joinLinesLoop(depth=${depth}): ${headFontId} of ${tailFontIds.length}")
        println(s"                             : lineShapes len = ${lineShapes.length}")
        val markedLineSpans = collectSpanEither[LineShape](lineShapes, { lineShape =>
          getFontsForShape(lineShape).contains(headFontId)
        })


        markedLineSpans.foreach{ _ match {
          case Right(lines) =>
            lines.foreach {lineShape =>
              doLineJoining(headFontId, lineShape)
            }

          case Left(lines)  => joinLinesLoop(tailFontIds, lines, depth+1)
        }}

      case Nil =>
        println(s"joinFontBaselines: Nil")
    }

    val startingLines = getLabeledLines(label)

    joinLinesLoop(fontsByMostOccuring.toList, startingLines)
  }


  def findLineCharsInPageBand(pageSlice: LTBounds, rootChar: ExtractedItem.CharItem, outputLabel: Label): Option[RectShape] = {
    val glyphsInBand = searchForRects(pageSlice, LB.Glyph)

    val glyphsWithChar = glyphsInBand.map { g =>
      (g, getCharsForShape(g).head)
    }

    var dbgGrid = Lazy[TB.Grid]{ TB.Grid.widthAligned() }

    dbgGrid = dbgGrid.map { grid =>
      var g = TB.Grid.widthAligned(
        (8, AlignRight), // h-dist
        (1, AlignLeft),  // space
        (1, AlignLeft),  // line text (char(s))
        (1, AlignLeft),  // space
      )

      g = g.addRow(
        "H-Dst|||",
        " ",
        ">",
        " "
      )
      g = g.addRow("        ", " ", " ", " ")
      g
    }


    // val orderedById = glyphsWithChar.sortBy(_._2.id)
    val orderedById = glyphsWithChar.sortBy(_._1.shape.left)

    val rootCharOffsets = docScope.fontDefs.getScaledFontOffsets(rootChar.scaledFontId)
    val capDescentDist = rootCharOffsets.descentLine - rootCharOffsets.capLine
    val whitespaceCutoff = capDescentDist * 3
    val dbgChars = orderedById.map(_._2.char).mkString



    // println(s"findLineCharsInPageBand: ${dbgChars}")
    // println(s"                       : ${pageSlice}")
    // println(s"                       : ${rootCharOffsets}: capDescentDist=${capDescentDist}, ws cutoff=${whitespaceCutoff}")
    val consecutiveSets = orderedById.groupByPairs { case ((bbox1, char1), (bbox2, char2)) =>
      val hdist = bbox2.shape.left - bbox1.shape.right
      val isClose = hdist < whitespaceCutoff
      val isConsecutive = char1.id.unwrap == char2.id.unwrap - 1

      isClose && isConsecutive
    }

    val hDists: Seq[Int@@FloatRep] = orderedById.sliding(2).toList.map { w => w.toList match {
      case (bbox1, char1) :: (bbox2, char2) :: Nil => bbox2.shape.left - bbox1.shape.right
      case _ => FloatExact.zero
    } }


    val indexedSets = consecutiveSets.zipWithIndex
    val setWithRootChar = indexedSets.filter { case (charSet, setNum) =>
      charSet.map(_._2.id).contains(rootChar.id)
    }

    if (setWithRootChar.nonEmpty) {
      val setNum = setWithRootChar.head._2
      val lenBeforeRoot = indexedSets.takeWhile(_._2 < setNum).map(_._1.length).sum
      val lenAfterRoot = indexedSets.drop(setNum+1).map(_._1.length).sum
      val indicator = (" "*lenBeforeRoot) + ("="*setWithRootChar.head._1.length) + (" "*lenAfterRoot)

      val charSetWithRootChar = setWithRootChar.flatMap(_._1.map(_._2))

      orderedById.zip(hDists :+ FloatExact.zero).zip(indicator.toList)
        .foreach{ case (((glyphShape, charItem), hDist), indicatorSym) =>
          dbgGrid = dbgGrid.map { grid =>
            grid.addRow(
              hDist.pp(),
              "",
              charItem.char.toString(),
              indicatorSym.toString(),
            )
          }
        }

      println("Debug Grid")
      println(dbgGrid.value.toBox().transpose())


      setWithRootChar.foreach { charSet =>
        charSet._1.map(_._1).foreach { unindexShape(_) }
      }

      val fontIds = charSetWithRootChar.map(_.scaledFontId).toSet
      val charBounds = charSetWithRootChar.map(_.minBBox).reduce(_ union _)

      charBounds.withinRegion(pageSlice)
        .adjacentRegions(Dir.Top, Dir.Center, Dir.Bottom)
        .map{ textRegion =>
          val pageBand = indexShape(textRegion, outputLabel).asRectShape
          setExtractedItemsForShape(pageBand, charSetWithRootChar)
          setFontsForShape(pageBand, fontIds)
          pageBand
        }

    } else None
  }

  def generatePageRules(startingShapeLabel: Label, outputLabel: Label): Unit = {
    val fontsByMostOccuring = docScope.getFontsWithDocwideOccuranceCounts()
      .sortBy(_._2).reverse.map(_._1)


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
          (lineShape, fontOffsets.forBaseline(line.p1.y))
        }

        val linesAndOffsetsAndHeadChar = allAdjustedOffsets.map{ case (lineShape, offsetsAtLine) =>
          val lineChars = getCharsForShape(lineShape)
          (lineShape, offsetsAtLine, lineChars.head)
        }

        linesAndOffsetsAndHeadChar.foreach { case (lineShape, offsetsAtLine, headChar) =>
          val capDescentBandHeight = offsetsAtLine.descentLine - offsetsAtLine.capLine
          println(s"generatePageRules: offsetsAtLine: ${offsetsAtLine}")

          pageHorizontalSlice(
            offsetsAtLine.capLine.asDouble(),
            capDescentBandHeight.asDouble()
          ).foreach{ slice =>
            val maybeBand = findLineCharsInPageBand(slice, headChar, outputLabel)


            maybeBand.foreach { capDescentBand =>
              val baselineMidriseHeight = offsetsAtLine.baseLine - offsetsAtLine.midriseLine
              val superSubCutoffY = offsetsAtLine.midriseLine + (baselineMidriseHeight / 3)
              println(s"generatePageRules: baselineMidriseHeight=${baselineMidriseHeight}")

              val capDescentRect = capDescentBand.shape
              val (maybeUpper, maybeLower) = capDescentRect.splitHorizontal(superSubCutoffY)
              val upperBand = maybeUpper.orDie(s"Could not split CapDescenderBand (${capDescentRect}) into upper/lower at ${superSubCutoffY}; ${offsetsAtLine}")
              val lowerBand = maybeLower.orDie(s"Could not split CapDescenderBand (${capDescentRect}) into upper/lower at ${superSubCutoffY}; ${offsetsAtLine}")

              traceLog.trace {
                traceLog.figure(capDescentRect) tagged s"CapDescenderBand Clipped"
              }
              traceLog.trace {
                traceLog.figure(upperBand) tagged s"Upper Superscript bounds"
              }
              traceLog.trace {
                traceLog.figure(lowerBand) tagged s"Lower Subscript bounds"
              }

              // val (maybeCapMidrise, maybMidriseDescent) = capDescentRect.splitHorizontal(offsetsAtLine.midrise)
              // val capsMidriseBand = maybeCapMidrise.orDie(s"Could not split CapDescenderBand (${capDescentRect}) into upper/lower")
              // val midriseDescentBand = maybMidriseDescent.orDie(s"Could not split CapDescenderBand (${capDescentRect}) into upper/lower")
              // val (maybeMidriseBaseline, maybeBaselineDescent) = midriseDescentBand.splitHorizontal(offsetsAtLine.baseline)

              // val midriseBaselineBand = maybeMidriseBaseline.orDie(s"Could not split midriseDescentBand (${midriseDescentBand}) into upper/lower")
              // val baselineDescentBand = maybeBaselineDescent.orDie(s"Could not split midriseDescentBand (${midriseDescentBand}) into upper/lower")


              val charsInBand = getCharsForShape(capDescentBand)

              val eitherSuperScriptOrNot = collectSpanEither[ExtractedItem.CharItem](charsInBand, { c =>
                val isRaised = c.minBBox.isStrictlyAbove(lowerBand)
                c.scaledFontId != headFontId && isRaised
              })
              val superScriptChars = eitherSuperScriptOrNot.collect { case e@ Right(i) => e }

              superScriptChars.foreach {
                case Right(charSpan) =>
                  val minBounds = charSpan.map(_.minBBox).reduce(_ union _)
                  traceLog.trace {
                    traceLog.figure(minBounds) tagged s"Superscript MinBounds"
                  }
              }

              val eitherSubScriptOrNot = collectSpanEither[ExtractedItem.CharItem](charsInBand, { c =>
                // && c.minBBox.intersects(baselineDescentBand)
                // val isContained1 = c.minBBox.isContainedBy(lowerBand)
                val isLowered = c.minBBox.isStrictlyBelow(upperBand)
                c.scaledFontId != headFontId && isLowered
              })

              val subScriptChars = eitherSubScriptOrNot.collect { case e@ Right(i) => e }

              subScriptChars.foreach {
                case Right(charSpan) =>
                  val minBounds = charSpan.map(_.minBBox).reduce(_ union _)
                  traceLog.trace {
                    traceLog.figure(minBounds) tagged s"SubScript MinBounds"
                  }
              }

              traceLog.trace {
                traceLog.shape(capDescentBand) tagged s"${outputLabel} Font#${depth}. ${headFontId}"
              }
            }
          }
        }

        _loop(tailFontIds, others, depth+1)

      case Nil =>
    }

    val startingLines = getLabeledLines(startingShapeLabel)

    _loop(fontsByMostOccuring.toList, startingLines)
    reindexShapes(LB.Glyph)
  }

  def indexPathRegions(): Unit = {
    val pathShapes = pageScope.pageItems.toSeq
      .filter { _.isInstanceOf[ExtractedItem.PathItem] }
      .map { item =>
        indexShape(item.minBBox, LB.PathBounds)
      }

    traceLog.trace {
      shape(pathShapes:_*) tagged "Path Line Bounds"
    }

  }

  def indexImageRegionsAndDeleteOverlaps(): Unit = {
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

  def createCharRunFontBaseline(charRun: Seq[ExtractedItem.CharItem]): Line = {
    val xSorted = charRun.sortBy { _.minBBox.left }
    val runBeginPt =  Point(xSorted.head.minBBox.left, xSorted.head.fontBbox.bottom)
    val runEndPt = Point(xSorted.last.minBBox.right, xSorted.last.fontBbox.bottom)
    Line(runBeginPt, runEndPt)
  }


  private def findNatLangBaselineRuns(retainNatLang: Boolean): Seq[Seq[ExtractedItem.CharItem]] = {
    pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filter(_.fontProps.isNatLangFont() == retainNatLang)
      .groupByPairs {
        case (item1, item2) =>
          lazy val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          lazy val sameLine = item1.fontBbox.bottom == item2.fontBbox.bottom
          lazy val sameFont = item1.fontProps == item2.fontProps

          consecutive && sameLine && sameFont
      }
  }

  private def findSymbolicCharRuns(): Seq[Seq[ExtractedItem.CharItem]] = {
    val charRuns = pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CharItem => item }
      .filterNot(_.fontProps.isNatLangFont())
      .groupByPairs {
        case (item1, item2) =>
          lazy val consecutive = item1.id.unwrap+1 == item2.id.unwrap
          lazy val leftToRight = item1.minBBox.left < item2.minBBox.left
          lazy val colinear = item1.minBBox.isNeitherAboveNorBelow(item2.minBBox)

          consecutive && leftToRight && colinear
      }

    charRuns
  }

  def combineCombiningMarks(): Unit = {
    val combiningMarks = pageScope.pageItems.toSeq
      .collect { case item: ExtractedItem.CombiningMark => item }

    combiningMarks.foreach { combiningMark =>
      indexShapeAndSetItems(combiningMark.minBBox, LB.Glyph, combiningMark)
    }
  }


  def findContiguousGlyphSpans(): Unit = {

    recordNatLangCharSpans(
      LB.CharRunFontBaseline,
      findNatLangBaselineRuns(retainNatLang=true)
    )

    // combineCombiningMarks()

    val charRunBaselineShapes = getLabeledLines(LB.CharRunFontBaseline)
    // recordNatLangVerticalLineSpacingStats(charRunBaselineShapes)
    // recordCharRunWidths(charRunBaselineShapes)
    // joinFontBaselines(LB.CharRunFontBaseline)

    indexPathRegions()

    indexImageRegionsAndDeleteOverlaps()

    val symbolicLangCharRuns = findSymbolicCharRuns()

    symbolicLangCharRuns.foreach { charItems =>
      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.SymbolicGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }
    }

    traceLog.trace { labeledShapes(LB.NatLangGlyph) tagged "All Natural Lang Glyphs" }
    traceLog.trace { labeledShapes(LB.SymbolicGlyph) tagged "All Symbolic Glyphs" }

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

      charItems.foreach { item =>
        indexShapeAndSetItems(item.minBBox, LB.NatLangGlyph, item)
        indexShapeAndSetItems(item.minBBox, LB.Glyph, item)
      }

      val baseLine = createCharRunFontBaseline(charItems)

      val baselineShape = indexShape(baseLine, spanLabel)

      val fontIds = charItems.map{ _.scaledFontId }.toSet

      setFontsForShape(baselineShape, fontIds)

      setExtractedItemsForShape(baselineShape, charRun)

      baselineShape
    }

    traceLog.trace { labeledShapes(spanLabel) tagged "Initial Font Baselines" }
  }



}
