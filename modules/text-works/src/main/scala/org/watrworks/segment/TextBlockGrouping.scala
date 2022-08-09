package org.watrworks
package segment

import prelude._
import geometry._
import geometry.syntax._
import utils.ExactFloats._
import utils.{Direction => Dir}
import utils.Interval

trait TextBlockGrouping extends NeighborhoodSearch { self =>

  def findMonoFontBlocks(): Unit = {
    val initShapes   = getLabeledShapes[Rect](LB.BaselineMidriseBand)
    val sortedShapes = sortShapesByFontOccurrence(initShapes)

    sortedShapes.foreach({ case (focalShapes, fontId) =>
      focalShapes.foreach(focalShape => {
        connectBaselineShapes(focalShape, fontId)
      })
    })
  }

  protected def connectBaselineShapes(
    focalShape: RectShape,
    fontId: String @@ ScaledFontID
  ): Unit = {

    traceLog.startTask(s"ConnectComponents")
    val graph           = pageScope.pageStats.connectedRunComponents.graph
    val fontVJumpByPage = docScope.docStats.fontVJumpByPage
    val GE              = LabeledShapeGraph.JumpEdge
    val GN              = LabeledShapeGraph.JumpNode

    runSearch(Dir.Down)
    runSearch(Dir.Up)

    def runSearch(facingDir: Dir) = {

      withAdjacentSkyline(
        facingDir,
        focalShape,
        fontId,
        connectJumps(facingDir, _)
      )
    }

    def connectJumps(facingDir: Dir, shapes: Seq[RectShape]) = {
      traceLog.startTask(s"ConnectJumps")
      shapes
        .groupBy(_.shape.bottom)
        .foreach { case (skylineGroupBottom, rects) =>
          val sorted = rects.sortBy(_.shape.left.unwrap)

          val jumpVDist = fontVJumpByPage.getJumpDist(focalShape.shape.bottom, skylineGroupBottom)

          sorted
            .sliding(2)
            .foreach({
              _ match {
                case Seq(shape1, shape2) =>
                  val evidenceDir = Dir.toReverse(facingDir)
                  // val jumpRight   = graph.edgeRight(fontId, evidenceDir, jumpVDist)
                  val n1 = GN(shape1, shapeIndex)
                  val n2 = GN(shape2, shapeIndex)

                  graph.upsertEdge(
                    _.map(GE.withEvidence(_, jumpVDist))
                      .getOrElse(GE(n1, n2, Dir.Right, jumpVDist)),
                    n1,
                    n2
                  )

                  traceLog.trace {
                    val focalShapeFont = focalShape.getAttr(PrimaryFont).get
                    val shape1Font     = shape1.getAttr(PrimaryFont).get
                    val shape2Font     = shape2.getAttr(PrimaryFont).get

                    createLabel(s"Facing${facingDir}DoJumpRight")
                      .withProp("display", "icon")
                      .withProp("Font", fontId.toString())
                      .withProp("JumpVDist", jumpVDist.pp())
                      .withProp("EvidenceDir", evidenceDir.toString())
                      .withChildren(
                        createLabelOn(s"FocalShape${evidenceDir}", focalShape.shape)
                          .withProp("Font", focalShapeFont.toString())
                          .withProp("role", "icon"),
                        createLabelOn("Shape1", shape1.shape)
                          .withProp("Font", shape1Font.toString()),
                        createLabelOn("Shape2", shape2.shape)
                          .withProp("Font", shape2Font.toString())
                      )
                  }
                case _ =>
              }
            })

          facingDir match {
            case Dir.Down =>
              val firstRect = sorted.head

              val n1 = GN(focalShape, shapeIndex)
              val n2 = GN(firstRect, shapeIndex)

              graph.upsertEdge(
                _.map(GE.withEvidence(_, jumpVDist))
                  .getOrElse(GE(n1, n2, facingDir, jumpVDist)),
                n1,
                n2
              )

              docScope.docStats.fontVJumpByPage.add(
                pageNum,
                fontId,
                skylineGroupBottom,
                focalShape.shape.bottom
              )

              traceLog.trace {
                val p1  = focalShape.shape.toPoint(M3.BottomLeft)
                val p2  = firstRect.shape.toPoint(M3.TopLeft)
                val l12 = p1.lineTo(p2)

                val focalShapeFont = focalShape.getAttr(PrimaryFont).get
                val shape1Font     = firstRect.getAttr(PrimaryFont).get

                createLabel(s"Facing${facingDir}DoJumpDown")
                  .withProp("display", "icon")
                  .withProp("Font", fontId.toString())
                  .withProp("JumpVDist", jumpVDist.pp())
                  .withChildren(
                    createLabelOn(s"FocalShape", focalShape.shape)
                      .withProp("Font", focalShapeFont.toString())
                      .withProp("role", "icon"),
                    createLabelOn("FoundShape", firstRect.shape)
                      .withProp("Font", shape1Font.toString()),
                    createLabelOn("Jump", l12)
                  )

              }

            case _ =>
          }
        }
      traceLog.endTask()
    }
    traceLog.endTask()
  }

  import Interval._
  import TypeTags._
  import utils.IndexedSeqADT._

  private def unionAll(shapes: Seq[GeometricFigure]): Option[Rect] = {
    if (shapes.length == 0) None
    else {
      val bounds = shapes.map(_.minBounds).reduce(_ union _)
      Some(bounds)
    }

  }

  def joinMonoFontBlocks(): Unit = {
    traceLog.startTask("CreateMonoFontLattice")
    val maxClustered             = docScope.docStats.fontVJumpByPage.documentMaxClustered
    val graph: LabeledShapeGraph = pageScope.pageStats.connectedRunComponents.graph

    val isValidJump: Interval.FloatExacts => graph.EdgeFilter = range => _.vJumpEvidence.exists(range.containsLCRC(_))

    val isValidFont: String @@ ScaledFontID => graph.NodeFilter =
      queryFontId => node => node.fontId == queryFontId

    for {
      (fontId, (count, range)) <- maxClustered
      components <-
        graph.findConnectedComponents(isValidJump(range), isValidFont(fontId), shapeIndex)
      if components.size > 1

      _componentShapes = components.map(_.nodeShape.asRectShape)
      _componentBounds = _componentShapes.map(_.shape)
      ccBounds <- unionAll(_componentBounds)

      componentShapes = searchForShapes[Rect](ccBounds, LB.BaselineMidriseBand)
                          .filter(s => hasFont(s, fontId))

      componentBounds = componentShapes.map(_.shape)
      ccBounds <- unionAll(componentBounds)

    } {

      val boundedBands = for {
        ccbound <- componentBounds
        boundedCC = ccbound withinRegion ccBounds
        boundedBand <- boundedCC.adjacentRegions(M3.Left, M3.Center, M3.Right)
      } yield boundedBand

      val uniqBands = boundedBands
        .groupBy(_.bottom)
        .values
        .flatMap(_.headOption)
        .to(List)
        .sortBy(_.bottom)

      val bandedShapes = for {
        band <- uniqBands
        ccshapes = componentShapes
                     .filter(_.shape.bottom == band.bottom)
                     .sortBy(_.shape.left)
      } yield (band, ccshapes)

      val bandedWithAdjoiningShapes = for {
        (band, shapes) <- bandedShapes
      } yield {
        val bandPairs = shapes.sliding(2).to(List).zipWithIndexADT.to(List)

        val adjoining = bandPairs.flatMap { case (shapePairs, pos) =>
          shapePairs match {
            case shape1 :: shape2 :: Nil =>
              val rect1        = shape1.shape
              val rect2        = shape2.shape
              val middleOf     = rect1.minSeparatingRect(rect2).map(_._1).toList
              lazy val leftOf  = rect1.withinRegion(band).adjacentRegion(M3.Left).toList
              lazy val rightOf = rect2.withinRegion(band).adjacentRegion(M3.Right).toList

              pos match {
                case SeqPos.First   => leftOf ++ middleOf
                case SeqPos.Last(_) => middleOf ++ rightOf
                case SeqPos.Sole    => leftOf ++ middleOf ++ rightOf
                case SeqPos.Mid(_)  => middleOf
              }
            case shape1 :: Nil =>
              val rect1        = shape1.shape
              lazy val leftOf  = rect1.withinRegion(band).adjacentRegion(M3.Left).toList
              lazy val rightOf = rect1.withinRegion(band).adjacentRegion(M3.Right).toList
              leftOf ++ rightOf
            case _ =>
              List()
          }
        }

        val l1                                   = adjoining.map(a => (a, None))
        val l2                                   = shapes.map(s => (s.shape, Some(s)))
        val all: List[(Rect, Option[RectShape])] = l1 ++ l2

        (band, all.sortBy(_._1.left))
      }

      val monofontLattice = indexShape[Rect](ccBounds, LB.MonoFontTextLattice)
      monofontLattice.setAttr(Fonts)(Set(fontId))
      monofontLattice.setAttr(PrimaryFont)(fontId)

      traceLog.trace {
        val bandLabels = for {
          (band, bandShapes) <- bandedWithAdjoiningShapes
        } yield {
          val bls = bandShapes.map({ case (rect, maybeShape) =>
            maybeShape match {
              case Some(_) => createLabelOn(s"BandedShape", rect)
              case None    => createLabelOn(s"BandedJoin", rect)
            }
          })

          createLabelOn(s"Band", band)
            .withChildren(bls: _*)
        }

        createLabel(s"MonoFontLattice")
          .withProp("display", "icon")
          .withProp("Font", fontId.toString())
          .withChildren(
            createLabelOn(s"ComponentBounds", ccBounds)
              .withProp("role", "icon")
          )
          .withChildren(bandLabels: _*)
      }
    }

    traceLog.endTask()
  }

}

trait TextBlockGroupingDocScope extends BaseDocumentSegmenter { self =>
  import utils.GuavaHelpers._
  import utils.ExactFloats._
  import utils.Interval._

  val ClusterEpsilonWidth = 0.2.toFloatExact() // FloatRep(20)

  def collectMonoFontFeatures(): Unit = {
    val fontVJumpByPage = docScope.docStats.fontVJumpByPage
    // val asList          = fontVJumpByPage.table.showBox()

    type AccT = (Int, Interval.FloatExacts)
    val zero: List[AccT] = List.empty

    // implicit val AccTShow = scalaz.Show.shows[AccT]({ case (i, fes) =>
    //   val shfes = Interval.FloatExacts.FloatExactsShow.show(fes)
    //   s"${shfes}x${i}"
    // })
    // implicit val ListAccTShow = scalaz.Show.shows[List[AccT]]({
    //   _.map(AccTShow.show(_)).mkString(", ")
    // })

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

          rexpanded.containsLCRC(edist)
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

    // val pprintPerPageMax = sortedByOverallMax
    //   .map { case (fontId, (count, range)) =>
    //     val rstr = Interval.FloatExacts.FloatExactsShow.show(range).toString()
    //     s"${fontId}: ${count}x${rstr}"
    //   }
    //   .mkString(", ")

    fontVJumpByPage.documentMaxClustered = sortedByOverallMax

    // println("Font V-Jumps per page")
    // println(asList.toString())
    // println("Cluster ")
    // println(mapped.showBox())
    // println("WithColMargins")
    // println(withColMargins.showBox())
    // println("Max Per Page")
    // println(pprintPerPageMax)
    // println("\n\n==================\n")
  }
}
