package org.watrworks
package geometry

import utils.{M3x3Position => M3}
import TypeTags._
import utils.ExactFloats._
import utils.{Direction => Dir}

sealed trait AngleType

object AngleType {
  case object Right  extends AngleType
  case object Acute  extends AngleType
  case object Obtuse extends AngleType
}

trait RectangularCuts extends GeometricOps {

  class RectangularCutOps(innerRect: Rect, enclosingRect: Rect) {

    def burstAllPossibleDirections(): Seq[(M3, Option[Rect])] = {

      val overlapOps = innerRect.withinRegion(enclosingRect)

      M3.All.foldLeft(
        List.empty[(M3, Option[Rect])]
      ) {
        case (acc, dir) => {
          (dir, overlapOps.adjacentRegion(dir)) :: acc
        }
      }

    }

    def burstAllDirections(): (Option[Rect], Seq[(M3, Rect)]) = {
      val intersection = innerRect.intersection(enclosingRect)

      val burst1 = M3.AllAdjacent.map { dir =>
        innerRect
          .withinRegion(enclosingRect)
          .adjacentRegion(dir)
          .map(r => (dir, r))
      }

      val burst2 = M3.AllAdjacent.map { dir =>
        enclosingRect
          .withinRegion(innerRect)
          .adjacentRegion(dir)
          .map(r => (dir, r))
      }

      val burstRegions = (burst1 ++ burst2).flatten.sortBy { b =>
        (b._2.left, b._2.top)
      }

      (intersection, burstRegions)
    }

    def burstAllAdjacent(): (Option[Rect], Seq[Rect]) = {
      val (intersection, burstRegions) = burstAllDirections()
      (intersection, burstRegions.map(_._2))
    }

    def burstAll(): Seq[Rect] = {
      val (maybeIntersect, burstRegions) = burstAllAdjacent()

      maybeIntersect.map(_ +: burstRegions).getOrElse(burstRegions)
    }

    def adjacentRegions(dirs: M3*): Option[Rect] = {
      val adjacents = dirs.toList.map { dir =>
        adjacentRegion(dir)
      }
      val nonEmptyAdjs = adjacents.flatten
      if (nonEmptyAdjs.nonEmpty) {
        Some(nonEmptyAdjs.reduce(_ union _))
      } else None
    }

    def adjacentRegion(dir: M3): Option[Rect] = {
      dir match {
        case M3.Center => innerRect.intersection(enclosingRect)
        case M3.Top =>
          for {
            right <- enclosingRect.splitVertical(innerRect.left)._2
            left  <- right.splitVertical(innerRect.right)._1
            top   <- left.splitHorizontal(innerRect.top)._1
          } yield top

        case M3.Bottom =>
          for {
            right <- enclosingRect.splitVertical(innerRect.left)._2
            left  <- right.splitVertical(innerRect.right)._1
            bot   <- left.splitHorizontal(innerRect.bottom)._2
          } yield bot

        case M3.Right =>
          for {
            bot   <- enclosingRect.splitHorizontal(innerRect.top)._2
            top   <- bot.splitHorizontal(innerRect.bottom)._1
            right <- top.splitVertical(innerRect.right)._2
          } yield right

        case M3.Left =>
          for {
            bot  <- enclosingRect.splitHorizontal(innerRect.top)._2
            top  <- bot.splitHorizontal(innerRect.bottom)._1
            left <- top.splitVertical(innerRect.left)._1
          } yield left

        case M3.TopLeft =>
          for {
            left <- enclosingRect.splitVertical(innerRect.left)._1
            top  <- left.splitHorizontal(innerRect.top)._1
          } yield top

        case M3.BottomLeft =>
          for {
            left <- enclosingRect.splitVertical(innerRect.left)._1
            bot  <- left.splitHorizontal(innerRect.bottom)._2
          } yield bot

        case M3.TopRight =>
          for {
            right <- enclosingRect.splitVertical(innerRect.right)._2
            top   <- right.splitHorizontal(innerRect.top)._1
          } yield top

        case M3.BottomRight =>
          for {
            right <- enclosingRect.splitVertical(innerRect.right)._2
            bot   <- right.splitHorizontal(innerRect.bottom)._2
          } yield bot
      }
    }

    def slidingRects(
      dir: Dir,
      stepSize: Int @@ FloatRep = innerRect.height
    ): Seq[Rect] = {
      val (clipHead, clipTail, initSlice) = dir match {
        case Dir.Down =>
          val headF = (r: Rect) => {
            val (rTop, _) = r.splitHorizontal(r.top + innerRect.height)
            rTop
          }
          val tailF = (r: Rect) => {
            val rs = r.shave(M3.Top, stepSize)
            if (rs.area() > 0) Some(rs) else None
          }
          val slice = adjacentRegions(M3.Center, M3.Bottom)
          (headF, tailF, slice)
        case Dir.Right =>
          val headF = (r: Rect) => {
            val (rLeft, _) = r.splitVertical(r.left + innerRect.width)
            rLeft
          }
          val tailF = (r: Rect) => {
            val rs = r.shave(M3.Left, stepSize)
            if (rs.area() > 0) Some(rs) else None
          }
          val slice = adjacentRegions(M3.Center, M3.Right)
          (headF, tailF, slice)
        case _ =>
          ???
      }

      def _run(currSlice: Option[Rect]): List[Rect] = {
        currSlice match {
          case Some(rect) =>
            val head    = clipHead(rect)
            val tail    = clipTail(rect)
            val newtail = _run(tail)
            head.map(_ :: newtail).getOrElse(newtail)
          case None => Nil
        }
      }

      _run(initSlice)
    }

  }

  implicit class RectangularCuts_RicherRect(val self: Rect) {

    def withinRegion(enclosingRect: Rect): RectangularCutOps =
      new RectangularCutOps(self, enclosingRect)

    def enclosingRect(inner: Rect): RectangularCutOps =
      new RectangularCutOps(inner, self)

    def minSeparatingRect(rect: Rect): Option[(Rect, M3)] = {
      val outer = self union rect
      val focus = self.withinRegion(outer)
      val other = rect.withinRegion(outer)

      def _check(d1: M3, d2: M3): Option[(Rect, M3)] = {
        val region1 = focus.adjacentRegion(d1)
        val region2 = other.adjacentRegion(d2)
        region1.zip(region2) match {
          case Some((r1, r2)) => (r1 intersection r2).map(rx => (rx, d1))
          case None           => None
        }
      }

      _check(M3.Bottom, M3.Top) orElse
        _check(M3.Left, M3.Right) orElse
        _check(M3.Top, M3.Bottom) orElse
        _check(M3.Right, M3.Left)
    }

  }

}
