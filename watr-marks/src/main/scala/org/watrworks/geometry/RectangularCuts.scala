package edu.umass.cs.iesl.watr
package geometry

import utils.ExactFloats._
import utils.Interval
import utils.Interval._
import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.EnrichNumerics._

sealed trait AngleType

object AngleType {
  case object Right extends AngleType
  case object Acute extends AngleType
  case object Obtuse extends AngleType
}


trait RectangularCuts extends GeometricOps {
  import GeometryImplicits._

  class RectangularCutOps(innerRect: LTBounds, enclosingRect: LTBounds) {

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

      // val burstRegions = (burst1 ++ burst2).flatten.sortBy(b => (b.left, b.top))
      val burstRegions = (burst1 ++ burst2).flatten.sortBy{b =>
        (b._2.left, b._2.top
        )
      }
      // (b => (b.left, b.top))

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



  implicit class RectangularCuts_RicherTrapezoid(val self: Trapezoid) {
    def leftBaseAngle(): Double = {
      toPoint(Dir.BottomLeft).angleTo(toPoint(Dir.TopLeft))
    }

    def rightBaseAngle(): Double = {
      math.Pi - toPoint(Dir.BottomRight).angleTo(toPoint(Dir.TopRight))
    }

    def prettyPrint: String = {

      val lla = leftBaseAngle()
      val lra = rightBaseAngle()

      val (llba, lrba) = classifyBaseAngles()

      val lls = llba match {
        case AngleType.Right => "◻"
        case AngleType.Acute => s"◿"
        case AngleType.Obtuse => s"◹"
      }

      val lrs = lrba match {
        case AngleType.Right => "◻"
        case AngleType.Acute => s"◺"
        case AngleType.Obtuse => s"◸"
      }

      // val tline = self.toLine(Dir.Top)
      // val bline = self.toLine(Dir.Bottom)
      val lldeg = (lla * 180) / math.Pi
      val lrdeg = (lra * 180) / math.Pi

      s"${lls}◻${lrs}:<${lldeg.pp}º,${lrdeg.pp}º>mbr:${minBoundingRect(self)}"
    }

    private val defaultAngleTolerance = 0.08d // ~ 4.6 deg.
    private val pi2 = math.Pi/2

    def leftBaseAngleType(tolerance: Double = defaultAngleTolerance): AngleType = {
      val lla = leftBaseAngle()
      val deg90 = Interval.Doubles(pi2-tolerance, tolerance*2)

      if (lla.withinRange(deg90)) AngleType.Right
      else if (lla < pi2) AngleType.Acute
      else AngleType.Obtuse

    }

    def rightBaseAngleType(tolerance: Double = defaultAngleTolerance): AngleType = {
      val lra = rightBaseAngle()
      val deg90 = Interval.Doubles(pi2-tolerance, tolerance*2)

      if (lra.withinRange(deg90)) AngleType.Right
      else if (lra < pi2) AngleType.Acute
      else AngleType.Obtuse
    }

    def classifyBaseAngles(tolerance: Double = defaultAngleTolerance): (AngleType, AngleType) = {
      (leftBaseAngleType(tolerance), rightBaseAngleType(tolerance))
    }

    def toPoint(dir: Dir): Point = {
      val Trapezoid(Point(tlx, tly), twidth, Point(blx, bly), bwidth) = self

      dir match {
        case Dir.Top         => ???
        case Dir.Bottom      => ???
        case Dir.Right       => ???
        case Dir.Left        => ???
        case Dir.TopLeft     => self.topLeft
        case Dir.BottomLeft  => self.bottomLeft
        case Dir.TopRight    => Point(tlx+twidth, tly)
        case Dir.BottomRight => Point(blx+bwidth, bly)
        case Dir.Center      => ???
      }
    }

    def toLine(dir: Dir): Line = dir match {
      case Dir.Top         => Line(toPoint(Dir.TopLeft), toPoint(Dir.TopRight))
      case Dir.Bottom      => Line(toPoint(Dir.BottomLeft), toPoint(Dir.BottomRight))
      case Dir.Right       => Line(toPoint(Dir.TopRight), toPoint(Dir.BottomRight))
      case Dir.Left        => Line(toPoint(Dir.TopLeft), toPoint(Dir.BottomLeft))
      case Dir.TopLeft     => ???
      case Dir.BottomLeft  => ???
      case Dir.TopRight    => ???
      case Dir.BottomRight => ???
      case Dir.Center      => ???
    }

  }

  implicit class RectangularCuts_RicherLTBounds(val self: LTBounds) {

    def withinRegion(enclosingRect: LTBounds): RectangularCutOps =
      new RectangularCutOps(self, enclosingRect)

    def enclosingRect(inner: LTBounds): RectangularCutOps =
      new RectangularCutOps(inner, self)

  }

}
