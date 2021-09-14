package org.watrworks
package segment

import geometry._
import geometry.syntax._
import rsearch.{Octothorpe => Oct}
import rsearch.{Octothorpe}
import watrmarks.Label
import utils.ExactFloats._
import utils.{Direction => Dir}
import prelude._

sealed trait Neighbor {
  def tags: List[String]
}

object Neighbor {
  case class SortWith[A, B: Ordering](
    f: A => B
  )
  case class BySearch[F <: GeometricFigure](
    oct: Oct,
    search: Rect => Seq[Shape[F]],
    sortBy: SortWith[Shape[F], Double],
    filterResults: Seq[Shape[F]] => Seq[Shape[F]],
    action: Seq[Shape[F]] => Unit,
    tags: List[String]
  ) extends Neighbor

  implicit class RicherNeighbor(val theNeighbor: Neighbor) extends AnyVal {
    def withTags(tags: String*): Neighbor = theNeighbor match {
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

    val sortWithTop = Neighbor.SortWith[Shape[Figure], Double](
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

  import Neighbor._

  protected def runNeighborBySearch[Figure <: GeometricFigure](bySearch: BySearch[Figure]): Unit = {
    val BySearch(oct, search, sortBy, filterResults, cb, tags @ _) = bySearch

    val res0     = oct.runSearch(search)
    val sorted   = res0.sortBy(sortBy.f)
    val filtered = filterResults(sorted)

    traceLog.trace {
      createOctoSearchLabel(bySearch, filtered)
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
  ): Unit = {
    traceLog.startTask(s"WithAdjacentSkyline/${facingDir}")
    // val focalRect = focalShape.shape
    // TODO rethink this maxDist value
    val chars   = focalShape.getAttr(ExtractedChars).getOrElse(Nil)
    val maxDist = chars.headOption.map(_.fontBbox.height).get * 2

    withUnoccludedShapes(
      facingDir,
      focalShape,
      LB.BaselineMidriseBand,
      maxDist,
      (hit) => hasFont(hit, fontId),
      callback
    )
    traceLog.endTask()
  }

  protected def withUnoccludedShapes(
    facingDir: Dir,
    focalShape: RectShape,
    queryLabel: Label,
    queryMaxDist: Int @@ FloatRep,
    hitFilter: AnyShape => Boolean,
    callback: Seq[RectShape] => Unit
  ): Unit = {
    val focalRect = focalShape.shape


    val neighorhoodDef = lookAdjacentFor[Rect](
      facingDir,
      focalRect,
      queryMaxDist,
      (foundShapes: Seq[RectShape]) => {


        pushLabel(
          createLabel(s"Facing${facingDir}Find( ${queryLabel.fqn} )")
            .withProp("class", ">lazy")
            .withChildren(
              createLabelOn("FocalRect", focalShape.shape)
                .withProp("class", "=eager")
            )
        )

        val adjacentShapes: Seq[RectShape] = for {
          hitShape <- foundShapes
          if hitFilter(hitShape)
          if hitShape.id != focalShape.id

          hitRect = hitShape.shape

          _                   <- traceShapes("HitRect", hitRect)
          (occlusionQuery, _) <- hitRect.minSeparatingRect(focalRect)
          _                   <- traceShapes("OcclusionQuery", occlusionQuery)
          occlusionHits = searchForShapes(occlusionQuery, queryLabel)
                            .filter(hit => {
                              hit.id != focalShape.id && hit.id != hitShape.id
                            })

          _ <- traceShapes("Occlusions", occlusionHits.map(_.minBounds): _*)
          if occlusionHits.isEmpty
          _ <- traceShapes("FinalHit", hitRect)
        } yield hitShape

        popLabel()

        callback(adjacentShapes)
      },
      queryLabel
    )
    runNeighborBySearch(neighorhoodDef)
  }

}
