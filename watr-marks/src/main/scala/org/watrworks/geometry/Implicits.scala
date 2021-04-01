package org.watrworks
package geometry

import scalaz.Equal
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import TypeTags._
import utils.ExactFloats._

object GeometryImplicits extends RectangularCuts with GeometricOps {

  implicit def EqualGeometricFigure: Equal[GeometricFigure] =
    Equal.equal((a, b) =>
      (a, b) match {

        case (g1: LTBounds, g2: LTBounds) => (
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

  implicit val EqualLTBounds: Equal[LTBounds]   = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualPoint: Equal[Point]         = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualLine: Equal[Line]           = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualTrapezoid: Equal[Trapezoid] = Equal.equalBy(_.asInstanceOf[GeometricFigure])

  def minBoundingRect(fig: GeometricFigure): LTBounds = fig match {
    case f: LTBounds => f
    case f: Point    => LTBounds(f.x, f.y, FloatRep(0), FloatRep(0))
    case f: Line     => f.bounds()
    case f: Trapezoid =>
      val Trapezoid(Point(tlx, tly), twidth, Point(blx, bly), bwidth) = f
      val minx                                                        = min(tlx, blx)
      val maxx                                                        = max(tlx + twidth, blx + bwidth)

      val miny = min(tly, bly)
      val maxy = max(tly, bly)

      LTBounds(minx, miny, maxx - minx, maxy - miny)

    case x => sys.error(s"minBoundingRect unexpected case ${x}")
  }

  def intersectionMBR(f1: GeometricFigure, f2: GeometricFigure): Option[LTBounds] = {
    minBoundingRect(f1).intersection(minBoundingRect(f2))
  }

  def shapesIntersect(f1: GeometricFigure, f2: GeometricFigure): Boolean =
    intersectionMBR(f1, f2).isDefined

  def shapesOverlap(f1: GeometricFigure, f2: GeometricFigure): Boolean =
    intersectionMBR(f1, f2).exists(_.area() > 0)

  def shapesTouch(f1: GeometricFigure, f2: GeometricFigure): Boolean =
    intersectionMBR(f1, f2).exists(_.area() == 0)

  def makeFringeParts(fig: GeometricFigure, padding: Padding): List[GeometricFigure] = {

    val wbbox = minBoundingRect(fig)

    val leftGutter = wbbox.copy(
      width = padding.left
    )

    val rightGutter = wbbox.copy(
      left = (wbbox.right - padding.right),
      width = padding.right
    )

    val topGutter = wbbox.copy(
      left = wbbox.left + padding.left,
      width = wbbox.width - (padding.right + padding.left),
      height = padding.top
    )

    val bottomGutter = wbbox.copy(
      left = topGutter.left,
      top = wbbox.bottom - padding.bottom,
      width = topGutter.width,
      height = padding.bottom
    )

    List(leftGutter, rightGutter, topGutter, bottomGutter)
  }

  implicit class RicherPoint(val self: Point) extends AnyVal {

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

  implicit val ptOrd: Ordering[Point] = Ordering.by { p: Point =>
    (p.x, p.y)
  }

  implicit val lineOrd: Ordering[Line] = Ordering.by { l: Line =>
    (l.p1, l.p2)
  }

  implicit class RicherLine(val self: Line) extends AnyVal {
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

    def bounds(): LTBounds = {
      val nline = sortPointsAsc
      LTBounds(
        nline.p1.x,
        nline.p1.y,
        nline.p2.x - nline.p1.x,
        nline.p2.y - nline.p1.y
      )
    }

    def clipTo(b: LTBounds): Line = {
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

}
