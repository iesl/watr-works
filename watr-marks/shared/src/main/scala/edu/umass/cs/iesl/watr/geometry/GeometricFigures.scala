package edu.umass.cs.iesl.watr
package geometry

import scalaz.@@
import scalaz.Equal

import TypeTags._

sealed trait GeometricFigure
sealed trait Area

object GeometricFigure {
  import EnrichGeometricFigures._

  case class LTBounds(
    left: Double,
    top: Double,
    width: Double,
    height: Double
  ) extends GeometricFigure with Area {
    override def toString: String = this.prettyPrint
  }

  case class LBBounds(
    left: Double,
    bottom: Double,
    width: Double,
    height: Double
  ) extends GeometricFigure with Area {
    override def toString: String = this.prettyPrint
  }


  case class Point(
    x: Double, y: Double
  ) extends GeometricFigure {
    override def toString: String = this.prettyPrint
  }

  case class Line(
    p1: Point, p2: Point
  ) extends GeometricFigure{
    override def toString: String = this.prettyPrint
  }

  implicit def EqualGeometricFigure
      : Equal[GeometricFigure] =
    Equal.equal((a, b)  => (a, b) match {
      case (g1: LTBounds, g2: LTBounds) => g1.prettyPrint == g2.prettyPrint
      case (g1: LBBounds, g2: LBBounds) => g1.prettyPrint == g2.prettyPrint
      case (g1: Point, g2: Point)       => g1.prettyPrint == g2.prettyPrint
      case (g1: Line, g2: Line)         => g1.prettyPrint == g2.prettyPrint
      case (_, _)                       => false
    })

  implicit val EqualLTBounds: Equal[LTBounds] = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualLBBounds: Equal[LBBounds] = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualPoint: Equal[Point] = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualLine: Equal[Line] = Equal.equalBy(_.asInstanceOf[GeometricFigure])

}

import GeometricFigure._


object EnrichGeometricFigures {
  import utils.EnrichNumerics._
  import utils.CompassDirection


  implicit class RicherDouble_2(val d: Double) extends AnyVal {
    def toHLine: Line = Line(
      Point(Double.MinValue, d),
      Point(Double.MaxValue, d))

    def toVLine: Line = Line(
      Point(d, Double.MinValue),
      Point(d, Double.MaxValue))

  }

  implicit class RicherFigure(val figure: GeometricFigure) extends AnyVal {

    def targetTo(page: Int@@PageID): TargetFigure = {
      TargetFigure(
        RegionID(0), // TODO gen region id
        page, figure
      )
    }

  }


  implicit class RicherPoint(val p0: Point) extends AnyVal {

    def -(p1:Point): Point = {
      Point(p0.x-p1.x, p0.y-p1.y)
    }


    def hdist(p1: Point): Double = p1.x - p0.x
    def hdistAbs(p1: Point): Double = math.abs(hdist(p1))

    def vdist(p1: Point): Double = p1.y - p0.y
    def vdistAbs(p1: Point): Double = math.abs(vdist(p1))

    def dist(p1: Point): Double = {
      val x = (p0 hdist p1)
      val y = (p0 vdist p1)
      math.sqrt(x*x + y*y)
    }

    def angleTo(p1: Point): Double = {
      if (p0.x > p1.x) {
        math.atan2(p0.y - p1.y, p0.x - p1.x);
      } else {
        math.atan2(p1.y - p0.y, p1.x - p0.x);
      }
    }
    def prettyPrint: String = {
      s"""(${fmt(p0.x)}, ${fmt(p0.y)})"""
    }

  }

  implicit val ptOrd:Ordering[Point] = Ordering.by{ p: Point =>
    (p.x, p.y)
  }

  implicit val lineOrd: Ordering[Line] = Ordering.by{ l:Line =>
    (l.p1, l.p2)
  }

  implicit class RicherLine(val line: Line) extends AnyVal {
    def prettyPrint(): String = {
      val p1 = line.p1.prettyPrint
      val p2 = line.p2.prettyPrint
      s"<line:$p1->$p2>"
    }

    def rise(): Double = line.p2.y - line.p1.y

    def run(): Double =  line.p2.x - line.p1.x

    def angle(): Double = math.atan2(line.rise, line.run)

    def slope(): Double = (line.rise) / (line.run)

    def length(): Double = {
      math.sqrt(line.run*line.run + line.rise*line.rise)
    }

    def ordered(l2: Line): (Line, Line) = {
      if (lineOrd.compare(line, l2) <= 0) (line, l2)
      else (l2, line)
    }

    def centerPoint: Point = Point(
      ((line.p1.x+line.p2.x) / 2),
      ((line.p1.y+line.p2.y) / 2)
    )

    def setY(y: Double): Line = {
      line.copy(
        p1 = line.p1.copy(y=y),
        p2 = line.p2.copy(y=y)
      )
    }

    def normalizeOrder: Line = {
      val (p1x, p2x) = if (line.p1.x < line.p2.x) (line.p1.x, line.p2.x) else (line.p2.x, line.p1.x)
      val (p1y, p2y) = if (line.p1.y < line.p2.y) (line.p1.y, line.p2.y) else (line.p2.y, line.p1.y)

      Line(Point(p1x, p1y), Point(p2x, p2y))
    }


    def clipTo(b: LTBounds): Line = {
      val lnorm = line.normalizeOrder
      val p1x = math.max(lnorm.p1.x, b.left)
      val p2x = math.min(lnorm.p2.x, b.left+b.width)
      val p1y = math.max(lnorm.p1.y, b.top)
      val p2y = math.min(lnorm.p2.y, b.left+b.width)
      Line(Point(p1x, p1y), Point(p2x, p2y))

    }
  }

  implicit class RicherLTBounds(val tb: LTBounds) extends AnyVal {
    def area: Double = tb.width*tb.height

    def right = tb.left+tb.width
    def bottom = tb.top+tb.height

    def translate(x: Double, y: Double): LTBounds = {
      tb.copy(
        left=tb.left+x,
        top=tb.top+y
      )
    }

    def intersects(rhs:LTBounds):Boolean = {
      !(tb.left > rhs.right
        || tb.right < rhs.left
        || tb.top > rhs.bottom
        || tb.bottom < rhs.top)
    }

    def union(b: LTBounds): LTBounds = {
      val left   = math.min(tb.left, b.left)
      val top    = math.min(tb.top, b.top)
      val right = math.max(tb.right, b.right)
      val bottom = math.max(tb.bottom, b.bottom)
      LTBounds(
        left, top,
        right-left,
        bottom-top
      )
    }

    // Compass direction point (bbox -> left-center, right-center, top-corner, etc)
    def toWesternPoint: Point = Point((tb.left), (tb.top+tb.height/2))
    def toEasternPoint: Point = Point((tb.left+tb.width), (tb.top+tb.height/2))

    import CompassDirection._

    def toPoint(cd: CompassDirection): Point ={
      def centerX = (tb.left+tb.width/2)
      def centerY = (tb.top+tb.height/2)

      cd match {
        case N  => Point(centerX, tb.top)
        case S  => Point(centerX, tb.bottom)
        case E  => Point(tb.right, centerY)
        case W  => Point(tb.left, centerY)
        case NW => Point(tb.left, tb.top)
        case SW => Point(tb.left, tb.bottom)
        case NE => Point(tb.right, tb.top)
        case SE => Point(tb.right, tb.bottom)
      }
    }

    def toLine(cd: CompassDirection): Line = cd match {
      case N  => Line(tb.toPoint(NW), tb.toPoint(NE))
      case S  => Line(tb.toPoint(SW), tb.toPoint(SE))
      case E  => Line(tb.toPoint(NE), tb.toPoint(SE))
      case W  => Line(tb.toPoint(NW), tb.toPoint(SW))
      case NW => ???
      case SW => ???
      case NE => ???
      case SE => ???
    }

    def xProjection(): Line = Line(
      Point((tb.left), 0),
      Point((tb.right), 0)
    )

    def yProjection(): Line = Line(
      Point(0, (tb.top)),
      Point(0, (tb.bottom))
    )


    def toCenterPoint: Point = Point(
      (tb.left+tb.width/2),
      (tb.top+tb.height/2)
    )

    def centerDistanceTo(other: LTBounds): Double = {
      val cx = (tb.left+tb.width/2).toFloat
      val cy = (tb.top+tb.height/2).toFloat
      val cx2 = (other.left+other.width/2).toFloat
      val cy2 = (other.top+other.height/2).toFloat

      math.sqrt(
        math.pow((cx-cx2).toDouble, 2) + math.pow((cy-cy2).toDouble, 2)
      )
    }


    def toLBBounds: LBBounds = {
      LBBounds(
        left = tb.left,
        bottom =  tb.top+tb.height,
        width = tb.width,
        height = tb.height
      )
    }

    def prettyPrint: String = {
      val left = tb.left
      val top=  tb.top
      val width = tb.width
      val height = tb.height
      s"""(l:${fmt(left)}, t:${fmt(top)}, w:${fmt(width)}, h:${fmt(height)})"""
    }

    def lowLeftCornerPrint: String = {
      val left = tb.left
      val bottom = tb.bottom
      s"""[${left.pp}, ${bottom.pp}]"""
    }

    def compactPrint: String = {
      val left = tb.left
      val top=  tb.top
      val width = tb.width
      val height = tb.height
      s"""[${left.pp}, ${top.pp}, ${width.pp}, ${height.pp}]"""
    }

    def uriString: String = {
      val left = tb.left
      val top=  tb.top
      val width = tb.width
      val height = tb.height
      s"""${left.pp}+${top.pp}+${width.pp}+${height.pp}"""
    }

    def targetRegionTo(page: Int@@PageID): TargetRegion = {
      TargetRegion(
        RegionID(0), // TODO gen region id
        emptyDocId,
        page,tb
      )
    }
  }

  implicit class RicherLBBounds(val tb: LBBounds) extends AnyVal {
    def toLTBounds: LTBounds = {
      LTBounds(
        left = tb.left,
        top =  tb.bottom-tb.height,
        width = tb.width,
        height = tb.height
      )

    }
    def prettyPrint: String = {
      val left = tb.left
      val bottom=  tb.bottom
      val width = tb.width
      val height = tb.height
      s"""(l:${fmt(left)}, b:${fmt(bottom)}, w:${fmt(width)}, h:${fmt(height)})"""
    }
  }

  // def charBoxesBounds(charBoxes: Seq[AtomicComponent]): LTBounds = {
  //   if (charBoxes.isEmpty) {
  //     LTBounds(0, 0, 0, 0)
  //   } else {
  //     val cbs = charBoxes.sortBy(_.bounds.left)
  //     val top = cbs.map(_.bounds.top).min
  //     val bottom = cbs.map(_.bounds.bottom).max
  //     val l=cbs.head.bounds.left
  //     val r=cbs.last.bounds.right

  //     LTBounds(l, top, r-l, bottom-top)
  //   }
  // }
}
