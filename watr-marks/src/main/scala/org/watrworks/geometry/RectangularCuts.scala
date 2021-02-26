package org.watrworks
package geometry

import utils.{RelativeDirection => Dir}
import TypeTags._

sealed trait AngleType

object AngleType {
  case object Right extends AngleType
  case object Acute extends AngleType
  case object Obtuse extends AngleType
}


trait RectangularCuts extends GeometricOps {

  class RectangularCutOps(innerRect: LTBounds, enclosingRect: LTBounds) {
    def burstAllPossibleDirections(): Seq[(Dir, Option[LTBounds])] = {

      val overlapOps = innerRect.withinRegion(enclosingRect)

      Dir.All.foldLeft(
        List.empty[(Dir, Option[LTBounds])]
      ){ case (acc, dir) => {
        (dir, overlapOps.adjacentRegion(dir)) :: acc
      }}

    }

    def burstAllDirections(): (Option[LTBounds], Seq[(Dir, LTBounds)]) = {
      val intersection = innerRect.intersection(enclosingRect)

      val burst1 = Dir.AllAdjacent.map{ dir =>
        innerRect.withinRegion(enclosingRect)
          .adjacentRegion(dir)
          .map(r => (dir, r))
      }

      val burst2 = Dir.AllAdjacent.map{ dir =>
        enclosingRect.withinRegion(innerRect)
          .adjacentRegion(dir)
          .map(r => (dir, r))
      }

      val burstRegions = (burst1 ++ burst2).flatten.sortBy{b =>
        (b._2.left, b._2.top
        )
      }

      (intersection, burstRegions)
    }

    def burstAllAdjacent(): (Option[LTBounds], Seq[LTBounds]) = {
      val intersection = innerRect.intersection(enclosingRect)

      val burst1 = Dir.AllAdjacent.map{ dir =>
        innerRect.withinRegion(enclosingRect)
          .adjacentRegion(dir)
      }

      val burst2 = Dir.AllAdjacent.map{ dir =>
        enclosingRect.withinRegion(innerRect)
          .adjacentRegion(dir)
      }

      val burstRegions = (burst1 ++ burst2).flatten.sortBy(b => (b.left, b.top))

      (intersection, burstRegions)

    }

    def burstAll(): Seq[LTBounds] = {
      val (maybeIntersect, burstRegions) =
        innerRect.withinRegion(enclosingRect).burstAllAdjacent()

      maybeIntersect.map(_ +: burstRegions).getOrElse(burstRegions)
    }


    def adjacentRegions(dirs: Dir*): Option[LTBounds] = {
      val adjacents = dirs.toList.map{ dir =>
        adjacentRegion(dir)
      }
      val nonEmptyAdjs = adjacents.flatten
      if (nonEmptyAdjs.nonEmpty) {
        Some(nonEmptyAdjs.reduce(_ union _))
      } else None
    }

    def adjacentRegion(dir: Dir): Option[LTBounds] = {
      dir match {
        case Dir.Center  => innerRect.intersection(enclosingRect)
        case Dir.Top  =>
          for {
            right <- enclosingRect.splitVertical(innerRect.left)._2
            left  <- right.splitVertical(innerRect.right)._1
            top   <- left.splitHorizontal(innerRect.top)._1
          } yield top

        case Dir.Bottom  =>
          for {
            right <- enclosingRect.splitVertical(innerRect.left)._2
            left  <- right.splitVertical(innerRect.right)._1
            bot   <- left.splitHorizontal(innerRect.bottom)._2
          } yield bot


        case Dir.Right  =>
          for {
            bot   <- enclosingRect.splitHorizontal(innerRect.top)._2
            top   <- bot.splitHorizontal(innerRect.bottom)._1
            right <- top.splitVertical(innerRect.right)._2
          } yield right

        case Dir.Left  =>
          for {
            bot  <- enclosingRect.splitHorizontal(innerRect.top)._2
            top  <- bot.splitHorizontal(innerRect.bottom)._1
            left <- top.splitVertical(innerRect.left)._1
          } yield left

        case Dir.TopLeft =>
          for {
            left  <- enclosingRect.splitVertical(innerRect.left)._1
            top   <- left.splitHorizontal(innerRect.top)._1
          } yield top

        case Dir.BottomLeft =>
          for {
            left  <- enclosingRect.splitVertical(innerRect.left)._1
            bot   <- left.splitHorizontal(innerRect.bottom)._2
          } yield bot

        case Dir.TopRight =>
          for {
            right  <- enclosingRect.splitVertical(innerRect.right)._2
            top    <- right.splitHorizontal(innerRect.top)._1
          } yield top

        case Dir.BottomRight =>
          for {
            right <- enclosingRect.splitVertical(innerRect.right)._2
            bot   <- right.splitHorizontal(innerRect.bottom)._2
          } yield bot
      }
    }

  }




  implicit class RectangularCuts_RicherLTBounds(val self: LTBounds) {

    def withinRegion(enclosingRect: LTBounds): RectangularCutOps =
      new RectangularCutOps(self, enclosingRect)

    def enclosingRect(inner: LTBounds): RectangularCutOps =
      new RectangularCutOps(inner, self)

  }

}
