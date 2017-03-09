package edu.umass.cs.iesl.watr
package geometry

import scalaz.Equal

sealed trait GeometricFigure

import GeometryImplicits._

case class LTBounds(
  left: Double,
  top: Double,
  width: Double,
  height: Double
) extends GeometricFigure  {
  override def toString: String = this.prettyPrint
}

case class LBBounds(
  left: Double,
  bottom: Double,
  width: Double,
  height: Double
) extends GeometricFigure {
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

case class GeometricGroup(
  figures: List[GeometricFigure]
)

case class Padding(
  left: Double,
  top: Double,
  right: Double,
  bottom: Double
)

object Padding {
  def apply(n: Double): Padding = {
    Padding(n, n, n, n)
  }
}


object GeometricFigure {

  import GeometryImplicits._

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


object GeometryImplicits {
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



  implicit class RicherPoint(val p0: Point) extends AnyVal {

    def unary_-(): Point = {
      Point(-p0.x, -p0.y)
    }

    def -(p1:Point): Point = {
      Point(p0.x-p1.x, p0.y-p1.y)
    }

    def translate(x: Double=0d, y: Double=0d): Point = {
      Point(p0.x+x, p0.y+y)
    }
    def translate(p: Point): Point = {
      Point(p0.x+p.x, p0.y+p.y)
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

  implicit class RicherLTBounds(val theBbox: LTBounds) extends AnyVal {
    def area: Double = theBbox.width*theBbox.height

    def right = theBbox.left+theBbox.width
    def bottom = theBbox.top+theBbox.height

    def translate(pvec: Point): LTBounds =
      translate(pvec.x, pvec.y)

    def translate(x: Double=0d, y: Double=0d): LTBounds = {
      theBbox.copy(
        left=theBbox.left+x,
        top=theBbox.top+y
      )
    }

    def moveTo(x: Double, y: Double): LTBounds = {
      theBbox.copy(left=x, top=y)
    }

    def moveToOrigin(): LTBounds = {
      moveTo(0d,0d)
    }

    def splitHorizontal(bbox: LTBounds): List[LTBounds] = {
      val leftX = bbox.toWesternPoint.x
      val rightX = bbox.toEasternPoint.x
      val trbbox = theBbox
      val leftRights = if (trbbox.intersectsX(leftX)){
        val splitLeft = trbbox.splitHorizontal(leftX)
        if (trbbox.intersectsX(rightX)){
          val splitRight = trbbox.splitHorizontal(rightX)
          splitLeft.head :: splitLeft.tail
        } else {
          List(splitLeft.head)
        }
      } else if (trbbox.intersectsX(rightX)){
        trbbox.splitHorizontal(rightX).tail
      } else {
        List()
      }

      leftRights
    }

    def splitHorizontal(x: Double): List[LTBounds] = {
      if (intersectsX(x)) {
        val leftHalf = theBbox.copy(width=x-theBbox.left)
        val rightHalf = theBbox.copy(left=x, width=theBbox.width-leftHalf.width)
        List(leftHalf, rightHalf)
      } else List(theBbox)
    }

    def intersectsX(x: Double):Boolean = {
      theBbox.left <= x &&  x <= theBbox.right
    }


    def union(b: LTBounds): LTBounds = {
      val left   = math.min(theBbox.left, b.left)
      val top    = math.min(theBbox.top, b.top)
      val right = math.max(theBbox.right, b.right)
      val bottom = math.max(theBbox.bottom, b.bottom)
      LTBounds(
        left, top,
        right-left,
        bottom-top
      )
    }

    def intersection(b: LTBounds): Option[LTBounds] = {
      val left   = math.max(theBbox.left, b.left)
      val top    = math.max(theBbox.top, b.top)
      val right = math.min(theBbox.right, b.right)
      val bottom = math.min(theBbox.bottom, b.bottom)
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

    // Compass direction point (bbox -> left-center, right-center, top-corner, etc)
    def toWesternPoint: Point = Point((theBbox.left), (theBbox.top+theBbox.height/2))
    def toEasternPoint: Point = Point((theBbox.left+theBbox.width), (theBbox.top+theBbox.height/2))

    import CompassDirection._

    def toPoint(cd: CompassDirection): Point ={
      def centerX = (theBbox.left+theBbox.width/2)
      def centerY = (theBbox.top+theBbox.height/2)

      cd match {
        case N  => Point(centerX, theBbox.top)
        case S  => Point(centerX, theBbox.bottom)
        case E  => Point(theBbox.right, centerY)
        case W  => Point(theBbox.left, centerY)
        case NW => Point(theBbox.left, theBbox.top)
        case SW => Point(theBbox.left, theBbox.bottom)
        case NE => Point(theBbox.right, theBbox.top)
        case SE => Point(theBbox.right, theBbox.bottom)
      }
    }

    def toLine(cd: CompassDirection): Line = cd match {
      case N  => Line(theBbox.toPoint(NW), theBbox.toPoint(NE))
      case S  => Line(theBbox.toPoint(SW), theBbox.toPoint(SE))
      case E  => Line(theBbox.toPoint(NE), theBbox.toPoint(SE))
      case W  => Line(theBbox.toPoint(NW), theBbox.toPoint(SW))
      case NW => ???
      case SW => ???
      case NE => ???
      case SE => ???
    }

    def xProjection(): Line = Line(
      Point((theBbox.left), 0),
      Point((theBbox.right), 0)
    )

    def yProjection(): Line = Line(
      Point(0, (theBbox.top)),
      Point(0, (theBbox.bottom))
    )


    def toCenterPoint: Point = Point(
      (theBbox.left+theBbox.width/2),
      (theBbox.top+theBbox.height/2)
    )

    def centerDistanceTo(other: LTBounds): Double = {
      val cx = (theBbox.left+theBbox.width/2).toFloat
      val cy = (theBbox.top+theBbox.height/2).toFloat
      val cx2 = (other.left+other.width/2).toFloat
      val cy2 = (other.top+other.height/2).toFloat

      math.sqrt(
        math.pow((cx-cx2).toDouble, 2) + math.pow((cy-cy2).toDouble, 2)
      )
    }


    def toLBBounds: LBBounds = {
      LBBounds(
        left = theBbox.left,
        bottom =  theBbox.top+theBbox.height,
        width = theBbox.width,
        height = theBbox.height
      )
    }

    def prettyPrint: String = {
      val left = theBbox.left
      val top=  theBbox.top
      val width = theBbox.width
      val height = theBbox.height
      s"""(l:${fmt(left)}, t:${fmt(top)}, w:${fmt(width)}, h:${fmt(height)})"""
    }

    def lowLeftCornerPrint: String = {
      val left = theBbox.left
      val bottom = theBbox.bottom
      s"""[${left.pp}, ${bottom.pp}]"""
    }

    def compactPrint: String = {
      val left = theBbox.left
      val top=  theBbox.top
      val width = theBbox.width
      val height = theBbox.height
      s"""[${left.pp}, ${top.pp}, ${width.pp}, ${height.pp}]"""
    }

    def uriString: String = {
      val left = theBbox.left
      val top=  theBbox.top
      val width = theBbox.width
      val height = theBbox.height
      s"""${left.pp}+${top.pp}+${width.pp}+${height.pp}"""
    }

  }

  implicit class RicherLBBounds(val theBbox: LBBounds) extends AnyVal {
    def toLTBounds: LTBounds = {
      LTBounds(
        left = theBbox.left,
        top =  theBbox.bottom-theBbox.height,
        width = theBbox.width,
        height = theBbox.height
      )

    }
    def prettyPrint: String = {
      val left = theBbox.left
      val bottom=  theBbox.bottom
      val width = theBbox.width
      val height = theBbox.height
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
