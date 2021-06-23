package org.watrworks
package geometry

import utils.{RelativeDirection => Dir}
import TypeTags._

sealed trait AngleType

object AngleType {
  case object Right  extends AngleType
  case object Acute  extends AngleType
  case object Obtuse extends AngleType
}

trait RectangularCuts extends GeometricOps {

  class RectangularCutOps(innerRect: Rect, enclosingRect: Rect) {

    def burstAllPossibleDirections(): Seq[(Dir, Option[Rect])] = {

      val overlapOps = innerRect.withinRegion(enclosingRect)

      Dir.All.foldLeft(
        List.empty[(Dir, Option[Rect])]
      ) {
        case (acc, dir) => {
          (dir, overlapOps.adjacentRegion(dir)) :: acc
        }
      }

    }

    def burstAllDirections(): (Option[Rect], Seq[(Dir, Rect)]) = {
      val intersection = innerRect.intersection(enclosingRect)

      val burst1 = Dir.AllAdjacent.map { dir =>
        innerRect
          .withinRegion(enclosingRect)
          .adjacentRegion(dir)
          .map(r => (dir, r))
      }

      val burst2 = Dir.AllAdjacent.map { dir =>
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

    def adjacentRegions(dirs: Dir*): Option[Rect] = {
      val adjacents = dirs.toList.map { dir =>
        adjacentRegion(dir)
      }
      val nonEmptyAdjs = adjacents.flatten
      if (nonEmptyAdjs.nonEmpty) {
        Some(nonEmptyAdjs.reduce(_ union _))
      } else None
    }

    def adjacentRegion(dir: Dir): Option[Rect] = {
      dir match {
        case Dir.Center => innerRect.intersection(enclosingRect)
        case Dir.Top =>
          for {
            right <- enclosingRect.splitVertical(innerRect.left)._2
            left  <- right.splitVertical(innerRect.right)._1
            top   <- left.splitHorizontal(innerRect.top)._1
          } yield top

        case Dir.Bottom =>
          for {
            right <- enclosingRect.splitVertical(innerRect.left)._2
            left  <- right.splitVertical(innerRect.right)._1
            bot   <- left.splitHorizontal(innerRect.bottom)._2
          } yield bot

        case Dir.Right =>
          for {
            bot   <- enclosingRect.splitHorizontal(innerRect.top)._2
            top   <- bot.splitHorizontal(innerRect.bottom)._1
            right <- top.splitVertical(innerRect.right)._2
          } yield right

        case Dir.Left =>
          for {
            bot  <- enclosingRect.splitHorizontal(innerRect.top)._2
            top  <- bot.splitHorizontal(innerRect.bottom)._1
            left <- top.splitVertical(innerRect.left)._1
          } yield left

        case Dir.TopLeft =>
          for {
            left <- enclosingRect.splitVertical(innerRect.left)._1
            top  <- left.splitHorizontal(innerRect.top)._1
          } yield top

        case Dir.BottomLeft =>
          for {
            left <- enclosingRect.splitVertical(innerRect.left)._1
            bot  <- left.splitHorizontal(innerRect.bottom)._2
          } yield bot

        case Dir.TopRight =>
          for {
            right <- enclosingRect.splitVertical(innerRect.right)._2
            top   <- right.splitHorizontal(innerRect.top)._1
          } yield top

        case Dir.BottomRight =>
          for {
            right <- enclosingRect.splitVertical(innerRect.right)._2
            bot   <- right.splitHorizontal(innerRect.bottom)._2
          } yield bot
      }
    }


  }

  implicit class RectangularCuts_RicherRect(val self: Rect) {

    def withinRegion(enclosingRect: Rect): RectangularCutOps =
      new RectangularCutOps(self, enclosingRect)

    def enclosingRect(inner: Rect): RectangularCutOps =
      new RectangularCutOps(inner, self)

    def maxSeparatingRect(rect: Rect): Option[(Rect, Dir)] = {
      if (self.hasOverlappingArea(rect)) {
        None
      } else {
        val runion = self.union(rect)
        val cuts = self.withinRegion(runion)
      }

      ???
    }

  }

}
