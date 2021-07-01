package org.watrworks
package segment

import geometry._
import geometry.syntax._
import rsearch.{Octothorpe => Oct}
import rsearch.{Octothorpe}
import watrmarks.Label
import utils.ExactFloats._
import utils.{Direction => Dir}

sealed trait Neighbor {
  def tags: List[String]
}

object Neighbor {
  case class SortWith[A, B: Ordering](
    f: A => B
  )
  case class BySearch[Figure <: GeometricFigure](
    oct: Oct,
    search: Rect => Seq[DocSegShape[Figure]],
    sortBy: SortWith[DocSegShape[Figure], Double],
    filterResults: Seq[DocSegShape[Figure]] => Seq[DocSegShape[Figure]],
    action: Seq[DocSegShape[Figure]] => Unit,
    tags: List[String]
  ) extends Neighbor

  implicit class RicherNeighbor(val theNeighbor: Neighbor) extends AnyVal {
    def withTags(tags: String*): Neighbor = theNeighbor match {
      // case v: ByShape     => v.copy(tags = tags.to(List) ++ v.tags)
      // case v: ByIDOffset  => v.copy(tags = tags.to(List) ++ v.tags)
      case v: BySearch[_] => v.copy(tags = tags.to(List) ++ v.tags)
    }
  }

}

trait NeighborhoodSearch extends BasePageSegmenter {

  def searchAdjacentFor[Figure <: GeometricFigure](
    dir: Dir,
    focus: Rect,
    maxDistance: Int @@ FloatRep
  ): Octothorpe = {

    val horizon = focus.expand(Dir.toPosition(dir), maxDistance)

    val searchPosition = Dir.toPosition(dir)
    Oct
      .withSearchRegions(Oct.cell(searchPosition))
      .withHorizon(horizon)
      .centeredOn(focus)
  }


  def lookAdjacentFor[Figure <: GeometricFigure](
    facingDir: Dir,
    focus: Rect,
    maxDistance: Int @@ FloatRep,
    cb: Seq[DocSegShape[Figure]] => Unit,
    forLabels: Label*
  ): Neighbor.BySearch[Figure] = {
    val octo = searchAdjacentFor[Figure](facingDir, focus, maxDistance)

    val tags = s"""Searching${facingDir}For(${forLabels.map(_.fqn).mkString(" & ")})""" :: Nil

    val sortWithTop = Neighbor.SortWith[DocSegShape[Figure], Double](
      _.shape.minBounds.getTop
    )

    Neighbor.BySearch[Figure](
      octo,
      (area: Rect) => searchForShapes(area, forLabels: _*),
      sortBy = sortWithTop,
      filterResults = (ds: Seq[DocSegShape[Figure]]) => ds,
      cb,
      tags
    )
  }

  def boundingRect(pageSlice: Rect, c1Rect: Rect): Option[Rect] = {
    pageSlice.clipLeftRight(c1Rect.left, c1Rect.right)
  }

  def boundingRect(pageSlice: Rect, c1Rect: Rect, c2Rect: Rect): Option[Rect] = {
    val c12Rect = c1Rect union c2Rect
    pageSlice.clipLeftRight(c12Rect.left, c12Rect.right)
  }

  // def fontBackslashAngle = pageScope.pageStats.fontBackslashAngle
  def fontVJump          = pageScope.pageStats.fontVJump


  import Neighbor._


  protected def runNeighborBySearch[Figure <: GeometricFigure](bySearch: BySearch[Figure]): Unit = {
    val BySearch(oct, search, sortBy, filterResults, cb, tags) = bySearch

    val res0     = oct.runSearch(search)
    val sorted   = res0.sortBy(sortBy.f)
    val filtered = filterResults(sorted)

    traceLog.trace {

      val searchAreaLabels = oct
        .searchAreas()
        .to(List)
        .map({ case (sbounds, searchArea) =>
          val name = sbounds match {
            case Oct.Bounds.Cell(dir)        => s"Cell:${dir}"
            case Oct.Bounds.CellSpan(d1, d2) => s"Span:${d1}-${d2}"
          }
          createLabel(s"Query/${name}")
            .onShapes(searchArea)
        })

      val resLabels = traceLog.shapesToLabels(filtered: _*)

      createLabel("OctSearch")
        .withProp("class", ">lazy")
        .withChildren(
          createLabel("Octothorpe")
            .withChildren(
              createLabelOn("FocalRect", oct.focalRect)
                .withProp("class", "=eager")
                .withProp("tags", tags: _*),
              createLabelOn("HorizonRect", oct.horizonRect),
              createLabel("SearchArea")
                .withChildren(searchAreaLabels: _*)
            ),
          createLabel("Found")
            .withChildren(resLabels: _*)
        )

    }
    cb(filtered)
  }

  def traceShapes(name: String, shape: GeometricFigure*): Option[Unit] = {
    Option({
      modLabel(l => l.withChildren(createLabel(name).onShapes(shape: _*)))
    })
  }



  protected def withAdjacentSkyline(
    facingDir: Dir,
    focalShape: RectShape,
    fontId: String @@ ScaledFontID,
    callback: Seq[RectShape] => Unit
  ): Neighbor.BySearch[Rect] = {
    val focalRect = focalShape.shape
    val chars     = focalShape.getAttr(ExtractedChars).getOrElse(Nil)
    // TODO rethink this maxDist value
    val maxDist = chars.headOption.map(_.fontBbox.height).get * 2
    lookAdjacentFor[Rect](
      facingDir,
      focalShape.shape,
      maxDist,
      (foundShapes: Seq[RectShape]) => {

        pushLabel(
          createLabel(s"Facing${facingDir}FindSkyline")
            .withProp("class", ">lazy")
            .withChildren(
              createLabelOn("FocalRect", focalShape.shape)
                .withProp("class", "=eager")
                .withProp("Font", fontId.toString())
            )
        )

        val adjacentShapes: Seq[RectShape] = for {
          hitShape <- foundShapes
          if hasFont(hitShape, fontId)
          if hitShape.hasLabel(LB.BaselineMidriseBand)
          if hitShape.id != focalShape.id

          hitRect = hitShape.shape

          _                   <- traceShapes("HitRect", hitRect)
          (occlusionQuery, _) <- hitRect.minSeparatingRect(focalRect)
          _                   <- traceShapes("OcclusionQuery", occlusionQuery)
          occlusionHits = searchForShapes(occlusionQuery, LB.BaselineMidriseBand)
          occlusionShapes = occlusionHits
                              .filter(hit => { hit.id != focalShape.id && hit.id != hitShape.id })

          _ <- traceShapes("Occlusions", occlusionShapes.map(_.minBounds): _*)
          if occlusionShapes.isEmpty
          _ <- traceShapes("FinalHit", hitRect)
        } yield hitShape

        popLabel()

        callback(adjacentShapes)
      },
      LB.BaselineMidriseBand
    )
  }




}


  // def findIndentationShapes(
  //   fromBaseline: AnyShape,
  //   baselineFontId: String @@ ScaledFontID
  // ): Unit = {
  //   val fontBaselineShape = fromBaseline.asLineShape
  //   val fontBaseline      = fontBaselineShape.shape

  //   val fontOffsets      = docScope.fontDefs.getScaledFontOffsets(baselineFontId)
  //   val fontHeight       = fontOffsets.distanceBetween(_.topLine, _.bottomLine)
  //   val lookDownDistance = fontHeight * 3
  //   val minIndent        = fontHeight // TODO choose better values for these parameters
  //   val maxIndent        = fontHeight * 10

  //   _addEdge(fontBaselineShape)(
  //     // lookDownFor(
  //     lookAdjacentFor(
  //       M3.Bottom,
  //       fontBaseline.minBounds,
  //       lookDownDistance,
  //       (_: Seq[LineShape]).foreach(shape => {
  //         val neighborShape   = shape.shape
  //         val neighborShapeP1 = neighborShape.p1
  //         val baselineP1      = fontBaseline.p1

  //         val leftSideOffset = neighborShapeP1.x - baselineP1.x
  //         val isIndent       = minIndent < leftSideOffset && leftSideOffset <= maxIndent
  //         val isOutdent      = minIndent < -leftSideOffset && -leftSideOffset <= maxIndent
  //         if (leftSideOffset != FloatExact.zero) {
  //           println(s"leading traps: leftSideOffset=${leftSideOffset.pp()}, min/max = ${minIndent
  //             .pp()}/${maxIndent.pp()}, isIn/Out = ${isIndent || isOutdent} ")
  //         }
  //         if (isIndent || isOutdent) {
  //           val neighborFontId = shape.getAttr(PrimaryFont).get
  //           val angleTo        = neighborShapeP1.angleTo(baselineP1)
  //           val angleToDeg     = ((angleTo * 180) / math.Pi).toFloatExact()

  //           fontBackslashAngle.add(baselineFontId, neighborFontId, angleToDeg)

  //           val leadingTrapezoid = Trapezoid.fromHorizontals(fontBaseline, neighborShape)
  //           val trapType         = if (isIndent) "Indent" else "Outdent"

  //           traceLog.trace {
  //             figure(leadingTrapezoid) tagged s"Leading ${trapType}"
  //           }
  //         }

  //       }),
  //       LB.CharRunFontLeadingBaseline
  //     )
  //   )
  // }
