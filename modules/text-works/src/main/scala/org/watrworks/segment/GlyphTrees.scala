package org.watrworks
package segment

import extract.FontBaselineOffsets
import geometry._
import geometry.syntax._


trait GlyphTrees extends GlyphRuns with LineSegmentation with GlyphGraphSearch { self =>

  // import utils.GuavaHelpers._

  // TODO how many times does a run of glyphs of the same font appear *not* on the same baseline
  //    (which may indicate a font used for graph/chart data points)
  def buildGlyphTree(): Unit = {
    defineBaselineMidriseTrees()
    evalGlyphTrees()

    // println("Font Backslash Angles \n")
    // println(fontBackslashAngle.table.showAsList().toString())
    // println("\n\n==================\n")
  }

  def traceShapes(name: String, shape: GeometricFigure*): Option[Unit] = {
    Option({
      modLabel(l => l.withChildren(createLabel(name).onShapes(shape: _*)))
    })
  }

  protected def withLowerSkyline(
    focalShape: RectShape,
    fontId: String @@ ScaledFontID,
    callback: Seq[RectShape] => Unit
  ): Neighbor.BySearch[Rect] = {
    val focalRect = focalShape.shape
    val chars     = focalShape.getAttr(ExtractedChars).getOrElse(Nil)
    val maxDist   = chars.headOption.map(_.fontBbox.height).get * 2
    lookDownFor[Rect](
      focalShape.shape,
      maxDist,
      (foundShapes: Seq[RectShape]) => {

        pushLabel(
          createLabel("FindSkylinePairs")
            .withProp("class", ">lazy")
            .withChildren(
              createLabelOn("FocalRect", focalShape.shape)
                .withProp("class", "=eager")
            )
        )

        // filter to shapes with nothing between them and focal shape
        val lowerSkylineShapes: Seq[RectShape] = for {
          hitShape <- foundShapes
          if hasFont(hitShape, fontId)
          if hitShape.hasLabel(LB.BaselineMidriseBand)
          if hitShape.id != focalShape.id

          hitRect = hitShape.shape

          hitShapeAndFocus = hitRect.union(focalRect)

          _              <- traceShapes("MidriseUnionFocusHorizon", hitShapeAndFocus)
          belowFocus     <- focalRect.withinRegion(hitShapeAndFocus).adjacentRegion(Dir.Bottom)
          _              <- traceShapes("BelowFocus", belowFocus)
          aboveHit: Rect <- hitRect.withinRegion(hitShapeAndFocus).adjacentRegion(Dir.Top)
          _              <- traceShapes("AboveHit", aboveHit)
          checkRegion    <- belowFocus.intersection(aboveHit)
          _              <- traceShapes("BetweenFocusAndHit", checkRegion)

          occlusionHits = searchForShapes(checkRegion, LB.BaselineMidriseBand)
          occlusionShapes = occlusionHits
                              .filter(hit => { hit.id != focalShape.id && hit.id != hitShape.id })

          _ <- traceShapes("Occlusions", occlusionShapes.map(_.minBounds): _*)
          if occlusionShapes.isEmpty
          _ <- traceShapes("FinalHit", hitRect)
        } yield hitShape

        popLabel()

        callback(lowerSkylineShapes)
      },
      LB.BaselineMidriseBand
    )
  }

  protected def labelAllColumnEvidence(): Unit = {
    val initShapes   = getLabeledShapes[Rect](LB.LowerSkyline)
    val sortedShapes = sortShapesByFontOccurrence(initShapes)

    sortedShapes.foreach({ case (skylineShapes, fontId @ _) =>
      skylineShapes.foreach(skylineShape => {
        skylineShape.getAttr(ChildShapes)
        val skylineUpperShape = skylineShape.getAttr(ParentShape).get.asRectShape
        val skylineRect       = skylineShape.shape

        val leftDeltaMax  = 15
        val leftDeltaInt  = math.abs(skylineRect.left.unwrap - skylineUpperShape.shape.left.unwrap)
        val leftAlignment = leftDeltaInt <= leftDeltaMax
        // val leftAlignment  = runion.left == focalShape.shape.left

        val rightDeltaMax = 25
        val rightDeltaInt =
          math.abs(skylineRect.right.unwrap - skylineUpperShape.shape.right.unwrap)
        val rightAlignment = rightDeltaInt <= rightDeltaMax
        // val rightAlignment = runion.right == focalShape.shape.right
        //
        if (leftAlignment) {
          traceLog.trace {
            val p1             = skylineUpperShape.shape.toPoint(Dir.BottomLeft)
            val p2             = skylineRect.toPoint(Dir.BottomLeft)
            val columnEvidence = Line(p1, p2)
            createLabelOn("LeftColumnEvidence", columnEvidence)
          }

        }
        if (rightAlignment) {
          traceLog.trace {
            val p1             = skylineUpperShape.shape.toPoint(Dir.BottomRight)
            val p2             = skylineRect.toPoint(Dir.BottomRight)
            val columnEvidence = Line(p1, p2)
            createLabelOn("RightColumnEvidence", columnEvidence)
          }
        }
      })
    })
  }

  protected def labelLowerSkyline(
    focalShape: RectShape,
    fontId: String @@ ScaledFontID
  ): Neighbor.BySearch[Rect] = {

    val focalRect = focalShape.shape

    withLowerSkyline(
      focalShape,
      fontId,
      (lowerSkylineShapes) => {
        lowerSkylineShapes
          .groupBy(_.shape.bottom.unwrap)
          .map { case (_, rects) =>
            val sorted       = rects.sortBy(_.shape.left.unwrap)
            val sortedRects  = sorted.map(_.shape)
            val runion       = sortedRects.reduce(_ union _)
            val lowerSkyline = indexShape(runion, LB.LowerSkyline)
            lowerSkyline.setAttr(ChildShapes)(sorted)
            lowerSkyline.setAttr(ParentShape)(focalShape)
            lowerSkyline.setAttr(Fonts)(Set(fontId))


            docScope.docStats.fontVJumpByPage.add(
              pageNum, fontId,
              runion.bottom,
              focalRect.bottom
            )

            traceLog.trace {
              val vdist = math.abs(runion.bottom.unwrap - focalRect.bottom.unwrap)

              createLabel("LowerSkylineCluster")
                .withProp("class", ">lazy")
                .withChildren(
                  createLabelOn("Skyline", runion),
                  createLabel("Children")
                    .withChildren(shapesToLabels(rects: _*): _*),
                  createLabelOn("Parent", focalRect)
                    .withProp("class", "=eager")
                    .withProp("Font", s"${fontId}")
                    .withProp("VDist", s"${vdist}")
                )
            }
          }
      }
    )
  }

  protected def defineBaselineMidriseTrees(): Unit = {
    val initShapes   = getLabeledShapes[Rect](LB.BaselineMidriseBand)
    val sortedShapes = sortShapesByFontOccurrence(initShapes)

    sortedShapes.foreach({ case (focalShapes, fontId) =>
      focalShapes.foreach(focalShape => {
        val bySearch = labelLowerSkyline(focalShape, fontId)
        runNeigborBySearch(bySearch)
      })
    })

    labelAllColumnEvidence()
  }

  protected def defineGlyphTrees(): Unit = {
    val fontBaselines   = getLabeledLines(LB.CharRunFontBaseline)
    val sortedBaselines = sortShapesByFontOccurrence(fontBaselines)

    sortedBaselines.foreach({ case (baselinesForFont, baselineFontId) =>
      val fontOffsets: FontBaselineOffsets = docScope.fontDefs.getScaledFontOffsets(baselineFontId)

      baselinesForFont.foreach(baseline => {
        findLeadingRunBaselines(baseline, baselineFontId)
        defineGlyphNGramsForFontRun(fontOffsets, baseline.asLineShape)
        // addNeighborEdgesById(glyphNGrams)
      })
    })
  }

  protected def evalGlyphTrees(): Unit = {
    expandEdges()
  }

  protected def defineGlyphNGramsForFontRun(
    fontOffsets: FontBaselineOffsets,
    fontBaselineShape: LineShape
  ): Seq[RectShape] = {
    val baselineFontId         = fontOffsets.scaledFontId
    val chars                  = fontBaselineShape.getAttr(ExtractedChars).getOrElse(Nil)
    val fontBaseline           = fontBaselineShape.shape
    val offsets                = fontOffsets.forFontBoxBottom(fontBaseline.p1.y)
    val ascentDescentPageSlice = getAscentDescentPageSlice(offsets).get

    val glyphNGrams: Seq[RectShape] = chars
      .sliding(2)
      .to(Seq)
      .flatMap(_ match {
        case Seq(c1, c2) =>
          val glyphRect = boundingRect(ascentDescentPageSlice, c1.minBBox, c2.minBBox).get

          Seq(initNodeShape(glyphRect, LB.GlyphBigram, Some(baselineFontId)))

        case Seq(c1) =>
          val glyphRect = boundingRect(ascentDescentPageSlice, c1.minBBox).get

          Seq(initNodeShape(glyphRect, LB.Glyph1gram, Some(baselineFontId)))

        case _ =>
          Seq()
      })

    glyphNGrams
  }
}


trait GlyphTreeDocScope extends BaseDocumentSegmenter { self =>
  import utils.GuavaHelpers._
  import scala.jdk.CollectionConverters._

  def analyzeVJumps(): Unit = {
    val fontVJumpByPage = docScope.docStats.fontVJumpByPage
    val asList = fontVJumpByPage.table.showBox()
    fontVJumpByPage.table.computeColMarginals(0)({ case (acc, e: fontVJumpByPage.ValueT) => {
      val wer = e.iterator().asScala.to(List)

      acc
    } })



    println("Font V-Jumps per page")
    println(asList.toString())
    println("\n\n==================\n")
  }
}
