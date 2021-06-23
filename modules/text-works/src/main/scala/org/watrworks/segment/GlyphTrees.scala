package org.watrworks
package segment

import geometry._
import geometry.syntax._
import utils.Interval
import Interval._
import utils.{RelativeDirection => Dir}

trait GlyphTrees extends GlyphRuns with LineSegmentation with GlyphGraphSearch { self =>

  // TODO how many times does a run of glyphs of the same font appear *not* on the same baseline
  //    (which may indicate a font used for graph/chart data points)

  def buildGlyphTree(): Unit = {
    defineBaselineMidriseTrees()
  }

  def buildGlyphTreeStep2(): Unit = {
    val graph = new ShapeIDGraph()

    val maxClustered = docScope.docStats.fontVJumpByPage.documentMaxClustered
    val initShapes   = getLabeledShapes[Rect](LB.LowerSkyline)
    for {
      (fontId, (count, range)) <- maxClustered
      lowerSkyline             <- initShapes
      if hasFont(lowerSkyline, fontId)
    } {
      val skylineChildren    = lowerSkyline.getAttr(ChildShapes).get.asRectShapes
      val skylineParentShape = lowerSkyline.getAttr(ParentShape).get.asRectShape

      skylineChildren
        .sortBy(_.shape.left.unwrap)
        .sliding(2)
        .foreach({
          _ match {
            case Seq(shape1, shape2) =>
              graph.edge(shape1, shape2, "Right")
            case _ =>
          }
        })

      skylineChildren.foreach({ shape =>
        graph.edge(skylineParentShape, shape, "Down")
      })
    }

    val connected = graph.weaklyConnectedComponents()

    connected.foreach({ components: graph.TopoOrdering =>
      val compsAsShapes = components
        .to(List)
        .map(_.toOuter)
        .map(shapeIndex.getById(_))

      val compBounds = compsAsShapes.headOption
        .map(headShape => {
          compsAsShapes.tail.foldLeft(headShape.shape.minBounds) { case (acc, e) =>
            acc union e.shape.minBounds
          }
        })

      compBounds.foreach(bounds => {
        traceLog.trace {
          createLabelOn("ConnCompBounds", bounds)
        }
      })
    })
  }

  def traceShapes(name: String, shape: GeometricFigure*): Option[Unit] = {
    Option({
      modLabel(l => l.withChildren(createLabel(name).onShapes(shape: _*)))
    })
  }

  protected def withUpperSkyline(
    focalShape: RectShape,
    fontId: String @@ ScaledFontID,
    callback: Seq[RectShape] => Unit
  ): Neighbor.BySearch[Rect] = {
    val focalRect = focalShape.shape
    val chars     = focalShape.getAttr(ExtractedChars).getOrElse(Nil)
    val maxDist   = chars.headOption.map(_.fontBbox.height).get * 2
    lookAdjacentFor[Rect](
      Dir.Bottom,
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
  protected def withLowerSkyline(
    focalShape: RectShape,
    fontId: String @@ ScaledFontID,
    callback: Seq[RectShape] => Unit
  ): Neighbor.BySearch[Rect] = {
    val focalRect = focalShape.shape
    val chars     = focalShape.getAttr(ExtractedChars).getOrElse(Nil)
    val maxDist   = chars.headOption.map(_.fontBbox.height).get * 2
    lookAdjacentFor[Rect](
      Dir.Bottom,
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
              pageNum,
              fontId,
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

}


trait GlyphTreeDocScope extends BaseDocumentSegmenter { self =>
  import utils.GuavaHelpers._
  import utils.ExactFloats._

  val ClusterEpsilonWidth = 0.2.toFloatExact() // FloatRep(20)
  def analyzeVJumps(): Unit = {
    val fontVJumpByPage = docScope.docStats.fontVJumpByPage
    val asList          = fontVJumpByPage.table.showBox()

    type AccT = (Int, Interval.FloatExacts)
    val zero: List[AccT] = List.empty

    implicit val AccTShow = scalaz.Show.shows[AccT]({ case (i, fes) =>
      val shfes = FloatExacts.FloatExactsShow.show(fes)
      s"${shfes}x${i}"
    })
    implicit val ListAccTShow = scalaz.Show.shows[List[AccT]]({
      _.map(AccTShow.show(_)).mkString(", ")
    })

    val mapped = fontVJumpByPage.table.map({ countedJumps =>
      val sorted = countedJumps
        .toList()
        .sortBy(_._1)
        .reverse

      val clusteredJumps = sorted.foldLeft(zero) { case (countedRanges, (ecount, edist)) =>
        val (containsEDist, others) = countedRanges.partition({ case (_, r) =>
          val rt0       = r.translate(ClusterEpsilonWidth)
          val rt1       = r.translate(-ClusterEpsilonWidth)
          val rexpanded = rt0.union(rt1)

          rexpanded.contains(edist)
        })
        val edistRange = FloatExacts(edist, FloatExact.zero)

        val expanded = containsEDist match {
          case (count, crange) :: rest =>
            val subsumedRange = crange.union(edistRange)
            (count + ecount, subsumedRange) :: rest

          case Nil =>
            (ecount -> edistRange) :: Nil
        }

        (expanded ++ others).sortBy(_._1).reverse
      }

      clusteredJumps
    })

    val withColMargins = mapped.computeColMarginals(zero) { case (allRanges, pageRanges) =>
      val acc = (allRanges ++ pageRanges).foldLeft(zero) { case (countedRanges, (ecount, erange)) =>
        val (containsEDist, others) = countedRanges.partition({ case (_, r) =>
          r.intersect(erange).isDefined
        })

        val expanded = containsEDist match {
          case (count, crange) :: rest =>
            val subsumedRange = crange.union(erange)
            (count + ecount, subsumedRange) :: rest

          case Nil =>
            (ecount -> erange) :: Nil
        }
        (expanded ++ others).sortBy(_._1).reverse
      }

      acc
    }

    val perPageMax: List[(fontVJumpByPage.ColT, AccT)] = withColMargins.colMarginals
      .map({ fontsByCount: Map[fontVJumpByPage.ColT, List[AccT]] =>
        val docWideFontRankedByVJumpDist =
          fontsByCount.toList.flatMap({ case (fontId, countsAndRanges) =>
            countsAndRanges.sortBy(_._1).reverse.headOption.map(cr => (fontId, cr))
          })
        docWideFontRankedByVJumpDist
      })
      .getOrElse(List.empty)

    val sortedByOverallMax: List[(fontVJumpByPage.ColT, AccT)] = perPageMax.sortBy(_._2._1).reverse

    val pprintPerPageMax = sortedByOverallMax
      .map { case (fontId, (count, range)) =>
        val rstr = Interval.FloatExacts.FloatExactsShow.show(range).toString()
        s"${fontId}: ${count}x${rstr}"
      }
      .mkString(", ")

    fontVJumpByPage.documentMaxClustered = sortedByOverallMax

    println("Font V-Jumps per page")
    println(asList.toString())
    println("Cluster ")
    println(mapped.showBox())
    println("WithColMargins")
    println(withColMargins.showBox())
    println("Max Per Page")
    println(pprintPerPageMax)
    println("\n\n==================\n")
  }
}
