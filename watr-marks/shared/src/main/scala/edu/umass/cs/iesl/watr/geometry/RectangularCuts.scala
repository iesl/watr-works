package edu.umass.cs.iesl.watr
package geometry

import utils.ExactFloats._
import utils.{RelativeDirection => Dir}


trait RectangularCuts {


  class RectangularCutOps(innerRect: LTBounds, enclosingRect: LTBounds) {

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
          // for {
          //   (l, right) <- enclosingRect.splitVertical(innerRect.left)
          //   (left, r)  <- right.splitVertical(innerRect.right)
          //   (top, b)   <- left.splitHorizontal(innerRect.top)
          // } yield top

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

  implicit class RicherLTBounds(val self: LTBounds) {
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
  }


  // /* TODO this rectangle split function is useful but has bugs.
  //  Split this rectangle into 0-3 parts, depending on overlap, like so:
  //                    +-------------+
  //                    |             |  <- splitter bbox
  //    +---------------+-------------+-----------------+
  //    |               |             |                 |    <-- self bbox
  //    |               |             |                 |
  //    |     left      |     mid     |      right      |
  //    |     bbox      |    bbox     |       bbox      |
  //    |               |             |                 |
  //    +---------------+-------------+-----------------+
  //                    |             |
  //                    |             |
  //                    +-------------+

  //  */
  // def splitHorizontal(splitter: LTBounds): List[LTBounds] = {
  //   val leftX = splitter.toPoint(Dir.Left).x
  //   val rightX = splitter.toPoint(Dir.Right).x
  //   val self = self
  //   val leftRights = if (self.intersectsX(leftX)){
  //     val splitLeft = self.splitHorizontal(leftX)
  //     if (self.intersectsX(rightX)){
  //       val splitRight = self.splitHorizontal(rightX)
  //       splitLeft.head :: splitLeft.tail
  //     } else {
  //       List(splitLeft.head)
  //     }
  //   } else if (self.intersectsX(rightX)){
  //     self.splitHorizontal(rightX).tail
  //   } else {
  //     List()
  //   }

  //   leftRights
  // }
}
