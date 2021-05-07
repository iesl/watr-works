package org.watrworks
package geometry

import utils.ExactFloats._
import utils.{RelativeDirection => Dir}

trait GeometricOps {
  implicit class GeometricOps_RicherRect(val self: Rect) {

    def area(): Double = (self.width*self.height).asDouble()

    def shave(delta: Double): Rect = shave(delta.toFloatExact())

    def shave(delta: Int@@FloatRep): Rect = {
      shave(Dir.TopLeft, delta)
        .shave(Dir.BottomRight, delta)
    }

    def shave(dir: Dir, delta: Int@@FloatRep): Rect = {
      dir match {
        case Dir.Top =>
          self.copy(
            top = self.top + delta,
            height = self.height - delta
          )
        case Dir.Bottom =>
          self.copy(
            height = self.height - delta
          )
        case Dir.Right =>
          self.copy(
            width=self.width-delta
          )
        case Dir.Left =>
          self.copy(
            left=self.left+delta,
            width=self.width-delta
          )

        case Dir.TopLeft     => self.shave(Dir.Top, delta).shave(Dir.Left, delta)
        case Dir.BottomLeft  => self.shave(Dir.Bottom, delta).shave(Dir.Left, delta)
        case Dir.TopRight    => self.shave(Dir.Top, delta).shave(Dir.Right, delta)
        case Dir.BottomRight => self.shave(Dir.Bottom, delta).shave(Dir.Right, delta)
        case Dir.Center      => ???
      }
    }

    def translate(pvec: Point): Rect = {
      translate(pvec.x, pvec.y)
    }

    def translate(x: Int@@FloatRep, y: Int@@FloatRep): Rect = {
      self.copy(
        left=self.left+x,
        top=self.top+y
      )
    }

    def translate(x: Double, y: Double): Rect = {
      self.copy(
        left=self.left+x,
        top=self.top+y
      )
    }

    def moveTo(x: Int@@FloatRep, y: Int@@FloatRep): Rect = {
      self.copy(left=x, top=y)
    }

    def moveToOrigin(): Rect = {
      moveTo(FloatRep(0), FloatRep(0))
    }

    def scale(byPercent: Double@@Percent): Rect = {
      val scale = byPercent.unwrap/100d
      val w = self.width + self.width*scale
      val h = self.height + self.height*scale
      self.copy(width=w, height=h)
    }


    def splitVertical(x: Int@@FloatRep): (Option[Rect], Option[Rect]) = {
      val Rect(left, top, width, height) = self

      val maybeLeft = if (left < x) {
        val leftWidth = min(x-left, width)
        Some(Rect(left, top, leftWidth, height))
      } else None

      val maybeRight = if (x < self.right) {
        val leftWidth = maybeLeft.map(_.width).getOrElse(0.toFloatExact())
        Some(Rect(left+leftWidth, top, width-leftWidth, height))
      } else None
      (maybeLeft, maybeRight)
    }

    def getHorizontalSlices(n: Int): Seq[Rect] = {
      val Rect(_, top, _, height) = self
      val sliceHeight: Int@@FloatRep = height / n

      val slices = (1 until n).map{ i =>
        val sliceAt = top + sliceHeight * i
        splitHorizontal(sliceAt)
      }

      slices.map(_._1.get) :+ slices.last._2.get
    }

    def getHorizontalSlice(y: Int@@FloatRep, h: Int@@FloatRep): Option[Rect] = {
      val (_, maybeLower) = splitHorizontal(y)
      maybeLower.flatMap { lowerRect =>
        val (maybeUpper, _) = lowerRect.splitHorizontal(y+h)
        maybeUpper
      }
    }

    def getVerticalSlice(x: Int@@FloatRep, w: Int@@FloatRep): Option[Rect] = {
      val (_, maybeRight) = splitVertical(x)
      maybeRight.flatMap { rightRect =>
        val (maybeLeft, _) = rightRect.splitVertical(x+w)
        maybeLeft
      }
    }

    def splitHorizontal(y: Int@@FloatRep): (Option[Rect], Option[Rect]) = {
      val Rect(left, top, width, height) = self


      val maybeTop = if (top < y) {
        val topHeight = min(y-top, height)
        Some(Rect(left, top, width, topHeight))
      } else None


      val maybeBottom = if (y < self.bottom) {
        val topHeight = maybeTop.map(_.height).getOrElse(0.toFloatExact())
        Some(Rect(left, top+topHeight, width, height-topHeight))
      } else None

      (maybeTop, maybeBottom)
    }


    def overlapsX(x: Int@@FloatRep): Boolean = {
      self.left <= x &&  x <= self.right
    }
    def overlapsY(y: Int@@FloatRep): Boolean = {
      self.top <= y && y <= self.bottom
    }

    def intersectsX(x: Int@@FloatRep):Boolean = {
      self.left <= x && x <= self.right
    }


    def union(b: Rect): Rect = {
      val left   = min(self.left, b.left)
      val top    = min(self.top, b.top)
      val right = max(self.right, b.right)
      val bottom = max(self.bottom, b.bottom)
      Rect(
        left, top,
        right-left,
        bottom-top
      )
    }

    def intersection(b: Rect): Option[Rect] = {
      val left   = max(self.left, b.left)
      val top    = max(self.top, b.top)
      val right = min(self.right, b.right)
      val bottom = min(self.bottom, b.bottom)
      if (left < right && top < bottom) {
        Some(Rect(left, top,
            right-left,
            bottom-top
          ))
      } else None
    }

    def intersects(rhs:Rect):Boolean = {
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

    def centerDistanceTo(other: Rect): Double = {
      val cx = (self.left+self.width/2).asDouble()
      val cy = (self.top+self.height/2).asDouble()
      val cx2 = (other.left+other.width/2).asDouble()
      val cy2 = (other.top+other.height/2).asDouble()

      math.sqrt(
        math.pow((cx-cx2), 2) + math.pow((cy-cy2), 2)
      )
    }

    def hasOverlappingArea(other: Rect): Boolean = {
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

    def hasSharedEdge(other: Rect): Boolean = {
      (self.left == other.left
        || self.right == other.right
        || self.top == other.top
        || self.bottom == other.bottom
      )
    }

    def hasNoSharedEdges(other: Rect): Boolean = {
      !hasSharedEdge(other)
    }


    def isContainedByVProjection(other: Rect): Boolean = {
      self.left >= other.left && self.right <= other.right
    }

    def isContainedByHProjection(other: Rect): Boolean = {
      self.top >= other.top && self.bottom <= other.bottom
    }

    def isContainedBy(other: Rect): Boolean = {
      isContainedByHProjection(other) && isContainedByVProjection(other)
    }

    def isNeitherAboveNorBelow(other: Rect): Boolean = {
      !isStrictlyAbove(other) && !isStrictlyBelow(other)
    }

    def isStrictlyAbove(other: Rect): Boolean = {
      val myBottom = self.toPoint(Dir.Bottom).y
      val otherTop = other.toPoint(Dir.Top).y
      myBottom < otherTop
    }

    def isStrictlyAbove(yVal: Int@@FloatRep): Boolean = {
      val myBottom = self.toPoint(Dir.Bottom).y
      myBottom < yVal
    }

    def isStrictlyBelow(yVal: Int@@FloatRep): Boolean = {
      val myTop = self.toPoint(Dir.Top).y
      myTop > yVal
    }

    def isStrictlyBelow(other: Rect): Boolean = {
      other.isStrictlyAbove(self)
    }

    def isStrictlyLeftOf(other: Rect): Boolean = {
      val rightEdge = self.toPoint(Dir.BottomRight).x
      val otherLeftEdge = other.toPoint(Dir.BottomLeft).x
      rightEdge < otherLeftEdge
    }

    def isStrictlyRightOf(other: Rect): Boolean = {
      other.isStrictlyLeftOf(self)
    }

    def isNeitherLeftNorRightOf(other: Rect): Boolean = {
      !isStrictlyLeftOf(other) && !isStrictlyRightOf(other)
    }

  }

}
