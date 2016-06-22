package edu.umass.cs.iesl.watr
package spindex

import net.sf.jsi

sealed trait Bounds
sealed trait Shape

object Bounds {
  val empty = LTBounds(0, 0, 0, 0)
}

case class LTBounds(
  left: Double,
  top: Double,
  width: Double,
  height: Double
) extends Bounds with Shape

case class LBBounds(
  left: Double,
  bottom: Double,
  width: Double,
  height: Double
) extends Bounds with Shape


case class Borders(
  bleft: Double,
  btop: Double,
  bright: Double,
  bbottom: Double
)

case class Point(
  x: Double, y: Double
) extends Shape

case class Line(
  p1: Point, p2: Point
) extends Shape

object jsiRectangle {
  def apply(
    x: Double, y: Double, width: Double, height: Double
  ): jsi.Rectangle = apply(
    x.toFloat, y.toFloat, width.toFloat, height.toFloat
  )

  def apply(
    x: Float, y: Float, width: Float, height: Float
  ): jsi.Rectangle = new jsi.Rectangle(
    x, y, x+width, y+height
  )

  def toLTBounds(r: jsi.Rectangle): LTBounds = {
    LTBounds(
      left = r.minX.toDouble,
      top =  r.minY.toDouble,
      width = (r.maxX - r.minX).toDouble,
      height = (r.maxY - r.minY).toDouble
    )
  }
}



object IndexShapeOperations {
  import utils.CompassDirection

  def fmt = (d: Double) => f"${d}%1.2f"

  implicit class RicherDouble(val d: Double) extends AnyVal {
    def pp:String = fmt(d)

    // def ltFuzzy(tolerance: Double)(d2: Double): Boolean = {
    //   d > d2
    //   d < (d2+tolerance)
    // }

    // def gtFuzzy(tolerance: Double)(d2: Double): Boolean =
    //   compareFuzzy(tolerance)(d2) > 0

    def eqFuzzy(tolerance: Double)(d2: Double): Boolean =
      compareFuzzy(tolerance)(d2) == 0


    def compareFuzzy(tolerance: Double)(d2: Double): Int = {
      if (math.abs(d - d2) < tolerance) 0
      else if (d < d2) -1
      else 1
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
      s"""[${fmt(p0.x)}, ${fmt(p0.y)}]"""
    }

  }

  implicit val ptOrd:Ordering[Point] = Ordering.by{ p: Point =>
    (p.x, p.y)
  }

  implicit val lineOrd: Ordering[Line] = Ordering.by{ l:Line =>
    (l.p1, l.p2)
  }

  implicit class RicherLine(val line: Line) extends AnyVal {

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

    def toJsiRectangle: jsi.Rectangle = {
       jsiRectangle(tb.left, tb.top, tb.width, tb.height)
    }

    def jsiCenterPoint: jsi.Point = {
      new jsi.Point(
        (tb.left+tb.width/2).toFloat,
        (tb.top+tb.height/2).toFloat
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

    def compactPrint: String = {
      val left = tb.left
      val top=  tb.top
      val width = tb.width
      val height = tb.height
      s"""[${left.pp}, ${top.pp}, ${width.pp}, ${height.pp}]"""
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

  def charBoxesBounds(charBoxes: Seq[PageAtom]): LTBounds = {
    if (charBoxes.isEmpty) {
      LTBounds(0, 0, 0, 0)
    } else {
      val cbs = charBoxes.sortBy(_.region.bbox.left)
      val top = cbs.map(_.region.bbox.top).min
      val bottom = cbs.map(_.region.bbox.bottom).max
      val l=cbs.head.region.bbox.left
      val r=cbs.last.region.bbox.right

      LTBounds(l, top, r-l, bottom-top)
    }
  }
}
