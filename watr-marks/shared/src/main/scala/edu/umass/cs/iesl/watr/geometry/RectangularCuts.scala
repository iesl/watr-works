package edu.umass.cs.iesl.watr
package geometry

import utils.ExactFloats._
import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.EnrichNumerics._

sealed trait AngleType

object AngleType {
  case object Right extends AngleType
  case object Acute extends AngleType
  case object Obtuse extends AngleType
}


trait RectangularCuts {
  import GeometryImplicits._

  class RectangularCutOps(innerRect: LTBounds, enclosingRect: LTBounds) {

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

    private val defaultAngleTolerance = 0.03d
    private val pi2 = math.Pi/2

    def leftBaseAngleType(tolerance: Double = defaultAngleTolerance): AngleType = {
      val lla = leftBaseAngle()
      val deg90 = DoubleInterval(pi2-tolerance, tolerance*2)

      if (lla.withinRange(deg90)) AngleType.Right
      else if (lla < pi2) AngleType.Acute
      else AngleType.Obtuse

    }

    def rightBaseAngleType(tolerance: Double = defaultAngleTolerance): AngleType = {
      val lra = rightBaseAngle()
      val deg90 = DoubleInterval(pi2-tolerance, tolerance*2)

      if (lra.withinRange(deg90)) AngleType.Right
      else if (lra < pi2) AngleType.Acute
      else AngleType.Obtuse
    }

    def classifyBaseAngles(tolerance: Double = defaultAngleTolerance): (AngleType, AngleType) = {
      (leftBaseAngleType(tolerance), rightBaseAngleType(tolerance))
    }


    def toPoint(dir: Dir): Point ={
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
      case Dir.Top         => Line(self.toPoint(Dir.TopLeft), self.toPoint(Dir.TopRight))
      case Dir.Bottom      => Line(self.toPoint(Dir.BottomLeft), self.toPoint(Dir.BottomRight))
      case Dir.Right       => Line(self.toPoint(Dir.TopRight), self.toPoint(Dir.BottomRight))
      case Dir.Left        => Line(self.toPoint(Dir.TopLeft), self.toPoint(Dir.BottomLeft))
      case Dir.TopLeft     => ???
      case Dir.BottomLeft  => ???
      case Dir.TopRight    => ???
      case Dir.BottomRight => ???
      case Dir.Center      => ???
    }

  }

  implicit class RectangularCuts_RicherLTBounds(val self: LTBounds) {
    def area: Double = (self.width*self.height).asDouble

    def withinRegion(enclosingRect: LTBounds): RectangularCutOps =
      new RectangularCutOps(self, enclosingRect)

    def enclosingRect(inner: LTBounds): RectangularCutOps =
      new RectangularCutOps(inner, self)

    // TODO these should return Option[LTBounds] and/or check for empty regions
    def setLeft(left: Int@@FloatRep): LTBounds = {
      self.copy(
        left=left,
        width=self.right-left
      )
    }

    def setRight(right: Int@@FloatRep): LTBounds = {
      self.copy(
        width=right-self.left
      )
    }

    def translate(pvec: Point): LTBounds = {
      translate(pvec.x, pvec.y)
    }

    def translate(x: Int@@FloatRep, y: Int@@FloatRep): LTBounds = {
      self.copy(
        left=self.left+x,
        top=self.top+y
      )
    }

    def translate(x: Double, y: Double): LTBounds = {
      self.copy(
        left=self.left+x,
        top=self.top+y
      )
    }

    def moveTo(x: Int@@FloatRep, y: Int@@FloatRep): LTBounds = {
      self.copy(left=x, top=y)
    }

    def moveToOrigin(): LTBounds = {
      moveTo(FloatRep(0), FloatRep(0))
    }

    def scale(byPercent: Double@@Percent): LTBounds = {
      val scale = byPercent.unwrap/100d
      val w = self.width + self.width*scale
      val h = self.height + self.height*scale
      self.copy(width=w, height=h)
    }


    def splitVertical(x: Int@@FloatRep): (Option[LTBounds], Option[LTBounds]) = {
      val LTBounds(left, top, width, height) = self

      val maybeLeft = if (left < x) {
        val leftWidth = min(x-left, width)
        Some(LTBounds(left, top, leftWidth, height))
      } else None

      val maybeRight = if (x < self.right) {
        val leftWidth = maybeLeft.map(_.width).getOrElse(0.toFloatExact)
        Some(LTBounds(left+leftWidth, top, width-leftWidth, height))
      } else None
      (maybeLeft, maybeRight)
    }

    def sliceHorizontal(n: Int): Seq[LTBounds] = {
      val LTBounds(_, top, _, height) = self
      val sliceHeight: Int@@FloatRep = height / n
      // println(s"sliceHorizontal($n): ${self}")

      val slices = (1 until n).map{ i =>
        val sliceAt = top + sliceHeight * i
        val split = splitHorizontal(sliceAt)
        // println(s"    sliceAt($sliceAt) = ${split}")
        split
      }

      slices.map(_._1.get) :+ slices.last._2.get
    }

    def splitHorizontal(y: Int@@FloatRep): (Option[LTBounds], Option[LTBounds]) = {
      val LTBounds(left, top, width, height) = self


      val maybeTop = if (top < y) {
        val topHeight = min(y-top, height)
        Some(LTBounds(left, top, width, topHeight))
      } else None


      val maybeBottom = if (y < self.bottom) {
        val topHeight = maybeTop.map(_.height).getOrElse(0.toFloatExact)
        Some(LTBounds(left, top+topHeight, width, height-topHeight))
      } else None

      (maybeTop, maybeBottom)
    }

    // def splitHorizontal(y: Int@@FloatRep): Option[(LTBounds, LTBounds)] = {
    //   if (overlapsY(y)) {
    //     val LTBounds(left, top, width, height) = self
    //     val upper = LTBounds(left, top, width, y-top)
    //     val lower = LTBounds(left, y, width, height-upper.height)

    //     Some((upper, lower))
    //   } else None
    // }

    def overlapsX(x: Int@@FloatRep): Boolean = {
      self.left <= x &&  x <= self.right
    }
    def overlapsY(y: Int@@FloatRep): Boolean = {
      self.top <= y && y <= self.bottom
    }

    def intersectsX(x: Int@@FloatRep):Boolean = {
      self.left <= x && x <= self.right
    }


    def union(b: LTBounds): LTBounds = {
      val left   = min(self.left, b.left)
      val top    = min(self.top, b.top)
      val right = max(self.right, b.right)
      val bottom = max(self.bottom, b.bottom)
      LTBounds(
        left, top,
        right-left,
        bottom-top
      )
    }

    def intersection(b: LTBounds): Option[LTBounds] = {
      val left   = max(self.left, b.left)
      val top    = max(self.top, b.top)
      val right = min(self.right, b.right)
      val bottom = min(self.bottom, b.bottom)
      if (left < right && top < bottom) {
        Some(LTBounds(left, top,
            right-left,
            bottom-top
          ))
      } else None
    }

    def intersects(rhs:LTBounds):Boolean = {
      intersection(rhs).isDefined
    }

    def toPoint(dir: Dir): Point ={
      def centerX = (self.left+self.width/2)
      def centerY = (self.top+self.height/2)

      dir match {
        case Dir.Top         => Point(centerX, self.top)
        case Dir.Bottom      => Point(centerX, self.bottom)
        case Dir.Right       => Point(self.right, centerY)
        case Dir.Left        => Point(self.left, centerY)
        case Dir.TopLeft     => Point(self.left, self.top)
        case Dir.BottomLeft  => Point(self.left, self.bottom)
        case Dir.TopRight    => Point(self.right, self.top)
        case Dir.BottomRight => Point(self.right, self.bottom)
        case Dir.Center      => Point(centerX, centerY)
      }
    }

    def toLine(dir: Dir): Line = dir match {
      case Dir.Top         => Line(self.toPoint(Dir.TopLeft), self.toPoint(Dir.TopRight))
      case Dir.Bottom      => Line(self.toPoint(Dir.BottomLeft), self.toPoint(Dir.BottomRight))
      case Dir.Right       => Line(self.toPoint(Dir.TopRight), self.toPoint(Dir.BottomRight))
      case Dir.Left        => Line(self.toPoint(Dir.TopLeft), self.toPoint(Dir.BottomLeft))
      case Dir.TopLeft     => ???
      case Dir.BottomLeft  => ???
      case Dir.TopRight    => ???
      case Dir.BottomRight => ???
      case Dir.Center      => ???
    }


    def centerDistanceTo(other: LTBounds): Double = {
      val cx = (self.left+self.width/2).asDouble
      val cy = (self.top+self.height/2).asDouble
      val cx2 = (other.left+other.width/2).asDouble
      val cy2 = (other.top+other.height/2).asDouble

      math.sqrt(
        math.pow((cx-cx2), 2) + math.pow((cy-cy2), 2)
      )
    }

    def isOverlapping(other: LTBounds): Boolean = {
      val overlapLR = (
        self.left < other.right
          && self.right > other.left
      )
      val overlapTB = (
        self.top < other.bottom
          && self.bottom > other.top
      )

      overlapLR && overlapTB
    }

    def isWithin(other: LTBounds): Boolean = {
      val withinLR = (
        self.left >= other.left
          && self.right <= other.right
      )
      val withinTB = (
        self.top >= other.top
          && self.bottom <= other.bottom
      )

      withinLR && withinTB

    }
  }

}
