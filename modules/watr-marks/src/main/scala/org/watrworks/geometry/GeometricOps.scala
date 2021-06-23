package org.watrworks
package geometry

import utils.{RelativeDirection => Dir}
import scalaz.Equal
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import TypeTags._
import utils.ExactFloats._

trait GeometricOps {

  implicit class GeometricOps_RicherRect(val self: Rect) {

    def area(): Double = (self.width * self.height).asDouble()

    def shave(delta: Double): Rect = shave(delta.toFloatExact())

    def shave(delta: Int @@ FloatRep): Rect = {
      shave(Dir.TopLeft, delta)
        .shave(Dir.BottomRight, delta)
    }

    def shave(dir: Dir, delta: Int @@ FloatRep): Rect = {
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
            width = self.width - delta
          )
        case Dir.Left =>
          self.copy(
            left = self.left + delta,
            width = self.width - delta
          )

        case Dir.TopLeft     => self.shave(Dir.Top, delta).shave(Dir.Left, delta)
        case Dir.BottomLeft  => self.shave(Dir.Bottom, delta).shave(Dir.Left, delta)
        case Dir.TopRight    => self.shave(Dir.Top, delta).shave(Dir.Right, delta)
        case Dir.BottomRight => self.shave(Dir.Bottom, delta).shave(Dir.Right, delta)
        case Dir.Center      => ???
      }
    }

    def expand(dir: Dir, delta: Int @@ FloatRep): Rect = {
      dir match {
        case Dir.Top =>
          self.copy(
            top = self.top - delta,
            height = self.height + delta
          )
        case Dir.Bottom =>
          self.copy(
            height = self.height + delta
          )
        case Dir.Right =>
          self.copy(
            width = self.width + delta
          )
        case Dir.Left =>
          self.copy(
            left = self.left - delta,
            width = self.width + delta
          )

        case Dir.TopLeft     => self.expand(Dir.Top, delta).expand(Dir.Left, delta)
        case Dir.BottomLeft  => self.expand(Dir.Bottom, delta).expand(Dir.Left, delta)
        case Dir.TopRight    => self.expand(Dir.Top, delta).expand(Dir.Right, delta)
        case Dir.BottomRight => self.expand(Dir.Bottom, delta).expand(Dir.Right, delta)
        case Dir.Center      => ???
      }
    }

    def translate(pvec: Point): Rect = {
      translate(pvec.x, pvec.y)
    }

    def translate(x: Int @@ FloatRep, y: Int @@ FloatRep): Rect = {
      self.copy(
        left = self.left + x,
        top = self.top + y
      )
    }

    def translate(x: Double, y: Double): Rect = {
      self.copy(
        left = self.left + x,
        top = self.top + y
      )
    }

    def moveTo(x: Int @@ FloatRep, y: Int @@ FloatRep): Rect = {
      self.copy(left = x, top = y)
    }

    def moveToOrigin(): Rect = {
      moveTo(FloatRep(0), FloatRep(0))
    }

    def scale(byPercent: Double @@ Percent): Rect = {
      val scale = byPercent.unwrap / 100d
      val w     = self.width + self.width * scale
      val h     = self.height + self.height * scale
      self.copy(width = w, height = h)
    }

    def splitVertical(x: Int @@ FloatRep): (Option[Rect], Option[Rect]) = {
      val Rect(left, top, width, height) = self

      val maybeLeft = if (left < x) {
        val leftWidth = min(x - left, width)
        Some(Rect(left, top, leftWidth, height))
      } else None

      val maybeRight = if (x < self.right) {
        val leftWidth = maybeLeft.map(_.width).getOrElse(0.toFloatExact())
        Some(Rect(left + leftWidth, top, width - leftWidth, height))
      } else None
      (maybeLeft, maybeRight)
    }

    def getHorizontalSlices(n: Int): Seq[Rect] = {
      val Rect(_, top, _, height)      = self
      val sliceHeight: Int @@ FloatRep = height / n

      val slices = (1 until n).map { i =>
        val sliceAt = top + sliceHeight * i
        splitHorizontal(sliceAt)
      }

      slices.map(_._1.get) :+ slices.last._2.get
    }

    def splitHorizontal(y: Int @@ FloatRep): (Option[Rect], Option[Rect]) = {
      val Rect(left, top, width, height) = self

      val maybeTop = if (top < y) {
        val topHeight = min(y - top, height)
        Some(Rect(left, top, width, topHeight))
      } else None

      val maybeBottom = if (y < self.bottom) {
        val topHeight = maybeTop.map(_.height).getOrElse(0.toFloatExact())
        Some(Rect(left, top + topHeight, width, height - topHeight))
      } else None

      (maybeTop, maybeBottom)
    }

    def clipLeftRight(x1: Int @@ FloatRep, x2: Int @@ FloatRep): Option[Rect] =
      for {
        rightHalf <- self.splitVertical(x1)._2
        leftHalf  <- rightHalf.splitVertical(x2)._1
      } yield leftHalf

    def clipLeftWidth(x: Int @@ FloatRep, width: Int @@ FloatRep): Option[Rect] =
      clipLeftRight(x, x + width)

    def clipTopBottom(y1: Int @@ FloatRep, y2: Int @@ FloatRep): Option[Rect] =
      for {
        bottomHalf <- self.splitHorizontal(y1)._2
        topHalf    <- bottomHalf.splitHorizontal(y2)._1
      } yield topHalf

    def clipTopHeight(y: Int @@ FloatRep, height: Int @@ FloatRep): Option[Rect] =
      clipTopBottom(y, y + height)

    def overlapsX(x: Int @@ FloatRep): Boolean = {
      self.left <= x && x <= self.right
    }
    def overlapsY(y: Int @@ FloatRep): Boolean = {
      self.top <= y && y <= self.bottom
    }

    def intersectsX(x: Int @@ FloatRep): Boolean = {
      self.left <= x && x <= self.right
    }

    def union(b: Rect): Rect = {
      val left   = min(self.left, b.left)
      val top    = min(self.top, b.top)
      val right  = max(self.right, b.right)
      val bottom = max(self.bottom, b.bottom)
      Rect(
        left,
        top,
        right - left,
        bottom - top
      )
    }

    def intersection(b: Rect): Option[Rect] = {
      val left   = max(self.left, b.left)
      val top    = max(self.top, b.top)
      val right  = min(self.right, b.right)
      val bottom = min(self.bottom, b.bottom)
      if (left < right && top < bottom) {
        Some(Rect(left, top, right - left, bottom - top))
      } else None
    }

    def intersects(rhs: Rect): Boolean = {
      intersection(rhs).isDefined
    }

    def toPoint(dir: Dir): Point = {
      def centerX = (self.left + self.width / 2)
      def centerY = (self.top + self.height / 2)

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
      val cx  = (self.left + self.width / 2).asDouble()
      val cy  = (self.top + self.height / 2).asDouble()
      val cx2 = (other.left + other.width / 2).asDouble()
      val cy2 = (other.top + other.height / 2).asDouble()

      math.sqrt(
        math.pow((cx - cx2), 2) + math.pow((cy - cy2), 2)
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
      || self.bottom == other.bottom)
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

    def isStrictlyAbove(yVal: Int @@ FloatRep): Boolean = {
      val myBottom = self.toPoint(Dir.Bottom).y
      myBottom < yVal
    }

    def isStrictlyBelow(yVal: Int @@ FloatRep): Boolean = {
      val myTop = self.toPoint(Dir.Top).y
      myTop > yVal
    }

    def isStrictlyBelow(other: Rect): Boolean = {
      other.isStrictlyAbove(self)
    }

    def isStrictlyLeftOf(other: Rect): Boolean = {
      val rightEdge     = self.toPoint(Dir.BottomRight).x
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

  implicit class RicherPoint(val self: Point) {

    def +(p: Point): Point = translate(p)
    def -(p: Point): Point = translate(-p)

    def unary_- : Point = {
      Point(-self.x, -self.y)
    }

    def lineTo(p1: Point): Line = {
      Line(self, p1)
    }

    def translate(x: Double, y: Double): Point = {
      Point(self.x + x, self.y + y)
    }
    def translate(p: Point): Point = {
      Point(self.x + p.x, self.y + p.y)
    }
    def translate(x: Int @@ FloatRep, y: Int @@ FloatRep): Point = {
      Point(self.x + x, self.y + y)
    }

    def hdist(p1: Point): Double    = (p1.x - self.x).asDouble()
    def hdistAbs(p1: Point): Double = math.abs(hdist(p1))

    def vdist(p1: Point): Double    = (p1.y - self.y).asDouble()
    def vdistAbs(p1: Point): Double = math.abs(vdist(p1))

    def dist(p1: Point): Double = {
      val x = (self hdist p1)
      val y = (self vdist p1)
      math.sqrt(x * x + y * y)
    }

    def angleTo(p1: Point): Double = {
      val dy = self.y - p1.y
      val dx = p1.x - self.x
      math.atan2(dy.asDouble(), dx.asDouble())
    }
    def prettyPrint: String = {
      s"""(${self.x.pp()}, ${self.y.pp()})"""
    }
  }

  implicit class RicherLine(val self: Line) {
    def prettyPrint(): String = {
      val p1 = self.p1.prettyPrint
      val p2 = self.p2.prettyPrint
      s"<$p1->$p2>"
    }

    def rise(): Double = (self.p2.y - self.p1.y).asDouble()

    def run(): Double = (self.p2.x - self.p1.x).asDouble()

    def angle(): Double = math.atan2(self.rise(), self.run())

    def slope(): Double = (self.rise()) / (self.run())

    def length(): Double = {
      math.sqrt(self.run() * self.run() + self.rise() * self.rise())
    }

    def ordered(l2: Line): (Line, Line) = {
      if (lineOrd.compare(self, l2) <= 0) (self, l2)
      else (l2, self)
    }

    def centerPoint: Point = Point(
      ((self.p1.x + self.p2.x) / 2),
      ((self.p1.y + self.p2.y) / 2)
    )

    def sortPointsAsc: Line = {
      val (p1x, p2x) =
        if (self.p1.x <= self.p2.x) (self.p1.x, self.p2.x) else (self.p2.x, self.p1.x)
      val (p1y, p2y) =
        if (self.p1.y <= self.p2.y) (self.p1.y, self.p2.y) else (self.p2.y, self.p1.y)

      Line(Point(p1x, p1y), Point(p2x, p2y))
    }

    def bounds(): Rect = {
      val nline = sortPointsAsc
      Rect(
        nline.p1.x,
        nline.p1.y,
        nline.p2.x - nline.p1.x,
        nline.p2.y - nline.p1.y
      )
    }

    def clipTo(b: Rect): Line = {
      val lnorm = self.sortPointsAsc
      val p1x   = max(lnorm.p1.x, b.left)
      val p2x   = min(lnorm.p2.x, b.left + b.width)
      val p1y   = max(lnorm.p1.y, b.top)
      val p2y   = min(lnorm.p2.y, b.left + b.width)
      Line(Point(p1x, p1y), Point(p2x, p2y))
    }

    def extendRightTo(x: Int @@ FloatRep): Line = {
      val Line(_, Point(_, y2)) = self
      self.copy(p2 = Point(x, y2))
    }

    def extendLeftTo(x: Int @@ FloatRep): Line = {
      val Line(Point(_, y1), _) = self
      self.copy(p1 = Point(x, y1))
    }

    def splitVertical(x: Int @@ FloatRep): Option[(Line, Line)] = {
      val Line(Point(x1, y1), Point(x2, y2)) = self.sortPointsAsc
      val overlaps                           = x1 < x && x < x2
      if (overlaps) {
        val left  = Line(Point(x1, y1), Point(x, y2))
        val right = Line(Point(x, y1), Point(x2, y2))

        Some((left, right))
      } else None
    }

    def translate(x: Double, y: Double): Line = {
      Line(
        self.p1.translate(x, y),
        self.p2.translate(x, y)
      )
    }

    def translate(x: Int @@ FloatRep, y: Int @@ FloatRep): Line = {
      Line(
        self.p1.translate(x, y),
        self.p2.translate(x, y)
      )
    }
  }

  implicit def EqualGeometricFigure: Equal[GeometricFigure] =
    Equal.equal((a, b) =>
      (a, b) match {

        case (g1: Rect, g2: Rect) => (
          g1.left === g2.left && g1.top === g2.top &&
            g1.width === g2.width && g1.height === g2.height
        )

        case (g1: Point, g2: Point) => g1.x === g2.x && g1.y === g2.y
        case (g1: Line, g2: Line)   => g1.p1 === g2.p1 && g1.p2 === g2.p2
        case (g1: Trapezoid, g2: Trapezoid) =>
          (g1.topLeft === g2.topLeft
            && g1.bottomLeft === g2.bottomLeft
            && g1.topWidth === g2.topWidth
            && g1.bottomWidth === g2.bottomWidth)

        case (_, _) => false
      }
    )

  implicit val EqualRect: Equal[Rect]           = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualPoint: Equal[Point]         = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualLine: Equal[Line]           = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualTrapezoid: Equal[Trapezoid] = Equal.equalBy(_.asInstanceOf[GeometricFigure])

  def minBoundingRect(fig: GeometricFigure): Rect = fig match {
    case f: Rect  => f
    case f: Point => Rect(f.x, f.y, FloatExact.epsilon, FloatExact.epsilon)
    case f: Line  => f.bounds()
    case f: Trapezoid =>
      val Trapezoid(Point(tlx, tly), twidth, Point(blx, bly), bwidth) = f
      val minx                                                        = min(tlx, blx)
      val maxx                                                        = max(tlx + twidth, blx + bwidth)

      val miny = min(tly, bly)
      val maxy = max(tly, bly)

      Rect(minx, miny, maxx - minx, maxy - miny)

    case x => sys.error(s"minBoundingRect unexpected case ${x}")
  }

  def intersectionMBR(f1: GeometricFigure, f2: GeometricFigure): Option[Rect] = {
    minBoundingRect(f1).intersection(minBoundingRect(f2))
  }

  def shapesIntersect(f1: GeometricFigure, f2: GeometricFigure): Boolean =
    intersectionMBR(f1, f2).isDefined

  def shapesOverlap(f1: GeometricFigure, f2: GeometricFigure): Boolean =
    intersectionMBR(f1, f2).exists(_.area() > 0)

  def shapesTouch(f1: GeometricFigure, f2: GeometricFigure): Boolean =
    intersectionMBR(f1, f2).exists(_.area() == 0)

  implicit val ptOrd: Ordering[Point] = Ordering.by { p: Point =>
    (p.x, p.y)
  }

  implicit val lineOrd: Ordering[Line] = Ordering.by { l: Line =>
    (l.p1, l.p2)
  }

}
