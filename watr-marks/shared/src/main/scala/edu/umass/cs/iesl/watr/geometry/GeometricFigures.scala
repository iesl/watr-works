package edu.umass.cs.iesl.watr
package geometry

// import scalaz.
import scalaz.Equal
import scalaz.syntax.equal._
// import scalaz.std.list._
import scalaz.std.anyVal._
import geometry.syntax._


import utils.Color
import TypeTags._

sealed trait GeometricFigure

case class LTBounds(
  left   : Int@@FloatRep,
  top    : Int@@FloatRep,
  width  : Int@@FloatRep,
  height : Int@@FloatRep
) extends GeometricFigure  {
  override def toString: String = this.prettyPrint
  // def left   : Double = left0.asDouble()
  // def top    : Double = top0.asDouble()
  // def width  : Double = width0.asDouble()
  // def height : Double = height0.asDouble()
  def right = left+width
  def bottom = top+height
}

object LTBounds {
  object IntReps {
    def apply(left: Int, top: Int, width: Int, height: Int): LTBounds =
      LTBounds(FloatRep(left), FloatRep(top), FloatRep(width), FloatRep(height))

    def unapply(bbox: LTBounds): Option[(Int, Int, Int, Int)] = Some((
      bbox.left.unwrap,
      bbox.top.unwrap,
      bbox.width.unwrap,
      bbox.height.unwrap
    ))
  }

  object Ints {
    def apply(left: Int, top: Int, width: Int, height: Int): LTBounds =
      LTBounds(left.toFloatRep(), top.toFloatRep, width.toFloatRep, height.toFloatRep)

    def unapply(bbox: LTBounds): Option[(Int, Int, Int, Int)] = Some((
      bbox.left.asInt,
      bbox.top.asInt,
      bbox.width.asInt,
      bbox.height.asInt
    ))
  }

  object Doubles {
    def apply(left: Double, top: Double, width: Double, height: Double): LTBounds =
      LTBounds(left.toFloatRep(), top.toFloatRep, width.toFloatRep, height.toFloatRep)

    def unapply(bbox: LTBounds): Option[(Double, Double, Double, Double)] = Some((
      bbox.left.asDouble,
      bbox.top.asDouble,
      bbox.width.asDouble,
      bbox.height.asDouble
    ))
  }

  object Floats {
    def apply(left: Float, top: Float, width: Float, height: Float): LTBounds =
      LTBounds(left.toFloatRep(), top.toFloatRep, width.toFloatRep, height.toFloatRep)

    def unapply(bbox: LTBounds): Option[(Float, Float, Float, Float)] = Some((
      bbox.left.asFloat(),
      bbox.top.asFloat(),
      bbox.width.asFloat(),
      bbox.height.asFloat()
    ))
  }


  val empty = IntReps.apply(0, 0, 0, 0)
  val zero = empty

}

case class LBBounds(
  left: Int@@FloatRep,
  bottom: Int@@FloatRep,
  width: Int@@FloatRep,
  height: Int@@FloatRep
) extends GeometricFigure {
  override def toString: String = this.prettyPrint
}


case class Point(
  x: Int@@FloatRep, y: Int@@FloatRep
) extends GeometricFigure {

  override def toString: String = this.prettyPrint
}

object Point {

  object IntReps {
    def apply(x: Int, y: Int): Point =
      Point(FloatRep(x), FloatRep(y))

    def unapply(p: Point): Option[(Int, Int)] =
      Some((p.x.unwrap, p.y.unwrap))
  }


  object Ints {
    def apply(x: Int, y: Int): Point =
      Point(x.toFloatRep(), y.toFloatRep())

    def unapply(p: Point): Option[(Int, Int)] =
      Some((p.x.asInt, p.y.asInt))
  }

  object Doubles {
    def apply(x: Double, y: Double): Point =
      Point(x.toFloatRep, y.toFloatRep)

    def unapply(p: Point): Option[(Double, Double)] = {
      Some((p.x.asDouble, p.y.asDouble))
    }
  }

  val origin = Ints(0, 0)
  val zero = origin

}

case class Line(
  p1: Point, p2: Point
) extends GeometricFigure {
  override def toString: String = this.prettyPrint
}

case class GeometricGroup(
  bounds: LTBounds,
  figures: List[GeometricFigure]
) extends GeometricFigure

case class Colorized(
  figure: GeometricFigure,
  fg: Color, bg: Color,
  fgOpacity: Float, bgOpacity: Float
) extends GeometricFigure

case class Padding(
  left: Int@@FloatRep,
  top: Int@@FloatRep,
  right: Int@@FloatRep,
  bottom: Int@@FloatRep
) {

  override def toString: String = {
    s"""pad[l:${left.pp}, t:${top.pp}, r:${right.pp}, b:${bottom.pp}]"""
  }
}



object Padding {
  object IntReps {
    def apply(left: Int, top: Int, right: Int, bottom: Int): Padding =
      Padding(FloatRep(left), FloatRep(top), FloatRep(right), FloatRep(bottom))

    def apply(p: Int): Padding = apply(p, p, p, p)

    def unapply(pad: Padding): Option[(Int, Int, Int, Int)] = {
      Some((
        pad.left.unwrap,
        pad.top.unwrap,
        pad.right.unwrap,
        pad.bottom.unwrap
      ))
    }
  }

  object Ints {
    def apply(left: Int, top: Int, right: Int, bottom: Int): Padding =
      Padding(left.toFloatRep, top.toFloatRep, right.toFloatRep, bottom.toFloatRep)

    def apply(p: Int): Padding = apply(p, p, p, p)

    def unapply(pad: Padding): Option[(Int, Int, Int, Int)] = {
      Some((
        pad.left.asInt,
        pad.top.asInt,
        pad.right.asInt,
        pad.bottom.asInt
      ))
    }
  }

  object Doubles {
    def apply(left: Double, top: Double, right: Double, bottom: Double): Padding =
      Padding(left.toFloatRep, top.toFloatRep, right.toFloatRep, bottom.toFloatRep)

    def apply(p: Double): Padding = apply(p, p, p, p)

    def unapply(pad: Padding): Option[(Double, Double, Double, Double)] = {
      Some((
        pad.left.asDouble,
        pad.top.asDouble,
        pad.right.asDouble,
        pad.bottom.asDouble
      ))
    }
  }
}


object GeometryImplicits {
  import utils.CompassDirection

  implicit def EqualGeometricFigure
      : Equal[GeometricFigure] =
    Equal.equal((a, b)  => (a, b) match {

      case (g1: LTBounds, g2: LTBounds) => (
        g1.left === g2.left && g1.top === g2.top &&
          g1.width === g2.width && g1.height === g2.height)

      case (g1: LBBounds, g2: LBBounds) => g1.toLTBounds === g2.toLTBounds
      case (g1: LBBounds, g2: LTBounds) => g1.toLTBounds === g2
      case (g1: LTBounds, g2: LBBounds) => g1 === g2.toLTBounds
      case (g1: Point, g2: Point)       => g1.x===g2.x && g1.y===g2.y
      case (g1: Line, g2: Line)         => g1.p1===g2.p1 && g1.p2===g2.p2

      case (_, _)                       => false
    })

  implicit val EqualLTBounds: Equal[LTBounds] = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualLBBounds: Equal[LBBounds] = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualPoint: Equal[Point] = Equal.equalBy(_.asInstanceOf[GeometricFigure])
  implicit val EqualLine: Equal[Line] = Equal.equalBy(_.asInstanceOf[GeometricFigure])

  def composeFigures(fig1: GeometricFigure, fig2: GeometricFigure): GeometricFigure = {
    val bbox1 = totalBounds(fig1)
    val bbox2 = totalBounds(fig2)
    val bbox12 = bbox1 union bbox2
    GeometricGroup(
      bbox12,
      List(fig1, fig2)
    )
  }

  def totalBounds(fig: GeometricFigure): LTBounds = fig match {
    case f: LTBounds       => f
    case f: LBBounds       => f.toLTBounds
    case f: Point          => LTBounds(f.x, f.y, FloatRep(0), FloatRep(0))
    case f: Line           => f.bounds()
    case f: GeometricGroup => f.bounds
    case f: Colorized => totalBounds(f.figure)
  }

  def makeFringeParts(fig: GeometricFigure, padding: Padding): List[GeometricFigure] = {

    val wbbox = totalBounds(fig)

    val leftGutter = wbbox.copy(
      width=padding.left
    )

    val rightGutter = wbbox.copy(
      left=(wbbox.right-padding.right),
      width=padding.right
    )

    val topGutter = wbbox.copy(
      left=wbbox.left+padding.left,
      width=wbbox.width-(padding.right+padding.left),
      height=padding.top
    )

    val bottomGutter = wbbox.copy(
      left=topGutter.left,
      top=wbbox.bottom-padding.bottom,
      width=topGutter.width,
      height=padding.bottom
    )

    List(leftGutter, rightGutter, topGutter, bottomGutter)
  }


  def makeFringe(fig: GeometricFigure, padding: Padding): GeometricFigure = {
    val fringe = makeFringeParts(fig, padding)
    val wbbox = totalBounds(fig)
    GeometricGroup(
      wbbox,
      fringe
    )
  }


  // implicit def FloatRepToDouble(floatRep: Int@@FloatRep): Double = {
  //   floatRep.unwrap/100d
  // }

  implicit class RicherFloatPrec2(val self: Int@@FloatRep) extends AnyVal {
    def +(r: Int@@FloatRep): Int@@FloatRep = FloatRep(self.unwrap+r.unwrap)
    def -(r: Int@@FloatRep): Int@@FloatRep = FloatRep(self.unwrap-r.unwrap)
    def *(r: Int@@FloatRep): Int@@FloatRep = (self.asDouble*r.asDouble).toFloatRep
    def /(r: Int@@FloatRep): Int@@FloatRep = (self.asDouble/r.asDouble).toFloatRep
    def unary_-(): Int@@FloatRep = FloatRep(-self.unwrap)

    def <(r: Int@@FloatRep): Boolean = self.unwrap<r.unwrap
    def <=(r: Int@@FloatRep): Boolean = self.unwrap<=r.unwrap
    def >=(r: Int@@FloatRep): Boolean = self.unwrap>=r.unwrap
    def >(r: Int@@FloatRep): Boolean = self.unwrap>r.unwrap


    def +(r: Double): Int@@FloatRep = self + r.toFloatRep()
    def -(r: Double): Int@@FloatRep = self - r.toFloatRep()
    def *(r: Double): Int@@FloatRep = (self.asDouble * r).toFloatRep
    def /(r: Double): Int@@FloatRep = (self.asDouble / r).toFloatRep

    def <(r:  Double): Boolean = self.unwrap<r
    def <=(r: Double): Boolean = self.unwrap<=r
    def >=(r: Double): Boolean = self.unwrap>=r
    def >(r: Double): Boolean  = self.unwrap>r

    def +(r: Float): Int@@FloatRep = self + r.toFloatRep
    def -(r: Float): Int@@FloatRep = self - r.toFloatRep
    def *(r: Float): Int@@FloatRep = (self.asFloat * r).toFloatRep
    def /(r: Float): Int@@FloatRep = (self.asFloat / r).toFloatRep


    def asFloat(): Float = { self.unwrap/100.0f }
    def asDouble(): Double = { self.unwrap/100.0d }
    def asInt(): Int = { self.unwrap/100 }

    def dblFormat(): String = {
      val digits = self.unwrap.toString.toList
      val(decR, wholeR) = digits.reverse.splitAt(2)
      val decPad = decR ++ List.fill(2-decR.length)('0')
      val dec = decPad.reverse.mkString
      val whole = if (wholeR.isEmpty) "0" else wholeR.reverse.mkString

      whole+"."+dec
    }
    def pp(): String = dblFormat()


    def eqFuzzy(tolerance: Double)(d2: Int@@FloatRep): Boolean =
      compareFuzzy(tolerance)(d2) == 0

    def compareFuzzy(tolerance: Double)(d20: Int@@FloatRep): Int = {
      val d2 = d20.asDouble
      val d1 = self.asDouble
      if (math.abs(d1 - d2) < tolerance) 0
      else if (d1 < d2) -1
      else 1
    }

  }


  implicit class RicherInt_2(val d: Int) extends AnyVal {
    def toFloatRep() = d.toFloat.toFloatRep()
  }

  implicit class RicherFloat_2(val d: Float) extends AnyVal {
    def float2fp2(d: Float): Int@@FloatRep = {
      FloatRep(
        (d*100.0d).toInt
      )
    }
    def toFloatRep() = float2fp2(d)
  }

  implicit class RicherDouble_2(val d: Double) extends AnyVal {
    def dbl2fp2(d: Double): Int@@FloatRep = {
      FloatRep(
        (d*100.0d).toInt
      )
    }
    def toFloatRep() = dbl2fp2(d)

  }



  implicit class RicherPoint(val self: Point) extends AnyVal {
    // def +(r: Double): Int@@FloatRep = self + r.toFloatRep()
    // def -(r: Double): Int@@FloatRep = self - r.toFloatRep()
    // def *(r: Double): Int@@FloatRep = (self.asDouble * r).toFloatRep
    // def /(r: Double): Int@@FloatRep = (self.asDouble / r).toFloatRep

    def +(p: Point): Point = translate(p)
    def -(p: Point): Point = translate(-p)
    // def *(r: Double): Int@@FloatRep = (self.asDouble * r).toFloatRep
    // def /(r: Double): Int@@FloatRep = (self.asDouble / r).toFloatRep

    def unary_-(): Point = {
      Point(-self.x, -self.y)
    }

    def lineTo(p1: Point): Line = {
      Line(self, p1)
    }

    def translate(x: Double=0d, y: Double=0d): Point = {
      Point(self.x+x, self.y+y)
    }
    def translate(p: Point): Point = {
      Point(self.x+p.x, self.y+p.y)
    }

    def hdist(p1: Point): Double = (p1.x - self.x).asDouble()
    def hdistAbs(p1: Point): Double = math.abs(hdist(p1))

    def vdist(p1: Point): Double = (p1.y - self.y).asDouble()
    def vdistAbs(p1: Point): Double = math.abs(vdist(p1))

    def dist(p1: Point): Double = {
      val x = (self hdist p1)
      val y = (self vdist p1)
      math.sqrt(x*x + y*y)
    }

    def angleTo(p1: Point): Double = {
      if (self.x > p1.x) {
        math.atan2((self.y - p1.y).asDouble, (self.x - p1.x).asDouble)
      } else {
        math.atan2((p1.y - self.y).asDouble, (p1.x - self.x).asDouble)
      }
    }
    def prettyPrint: String = {
      s"""(${self.x.pp}, ${self.y.pp})"""
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

    def rise(): Double = (line.p2.y - line.p1.y).asDouble

    def run(): Double =  (line.p2.x - line.p1.x).asDouble

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

    def normalizeOrder: Line = {
      val (p1x, p2x) = if (line.p1.x < line.p2.x) (line.p1.x, line.p2.x) else (line.p2.x, line.p1.x)
      val (p1y, p2y) = if (line.p1.y < line.p2.y) (line.p1.y, line.p2.y) else (line.p2.y, line.p1.y)

      Line(Point(p1x, p1y), Point(p2x, p2y))
    }

    def bounds(): LTBounds = {
      val nline = normalizeOrder
      LTBounds(
        nline.p1.x, nline.p1.y,
        nline.p2.x - nline.p1.x,
        nline.p2.y - nline.p1.y
      )
    }

    def clipTo(b: LTBounds): Line = {
      val lnorm = line.normalizeOrder
      val p1x = max(lnorm.p1.x, b.left)
      val p2x = min(lnorm.p2.x, b.left+b.width)
      val p1y = max(lnorm.p1.y, b.top)
      val p2y = min(lnorm.p2.y, b.left+b.width)
      Line(Point(p1x, p1y), Point(p2x, p2y))

    }
  }

  def max(d1:Int@@FloatRep, d2: Int@@FloatRep): Int@@FloatRep = FloatRep(math.max(d1.unwrap, d2.unwrap))
  def min(d1:Int@@FloatRep, d2: Int@@FloatRep): Int@@FloatRep = FloatRep(math.min(d1.unwrap, d2.unwrap))

  implicit class RicherLTBounds(val theBbox: LTBounds) extends AnyVal {
    def area: Double = (theBbox.width*theBbox.height).asDouble


    def translate(pvec: Point): LTBounds = {
      translate(pvec.x, pvec.y)
    }

    // def translate(x: Int@@FloatRep=fp2(0), y: Int@@FloatRep=fp2(0)): LTBounds = {

    def translate(x: Int@@FloatRep, y: Int@@FloatRep): LTBounds = {
      theBbox.copy(
        left=theBbox.left+x,
        top=theBbox.top+y
      )
    }

    def translate(x: Double, y: Double): LTBounds = {
      theBbox.copy(
        left=theBbox.left+x,
        top=theBbox.top+y
      )
    }

    def moveTo(x: Int@@FloatRep, y: Int@@FloatRep): LTBounds = {
      theBbox.copy(left=x, top=y)
    }

    def moveToOrigin(): LTBounds = {
      moveTo(FloatRep(0), FloatRep(0))
    }

    def scale(byPercent: Double@@Percent): LTBounds = {
      val scale = byPercent.unwrap/100d
      val w = theBbox.width + theBbox.width*scale
      val h = theBbox.height + theBbox.height*scale
      theBbox.copy(width=w, height=h)
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
    //   val leftX = splitter.toPoint(CDir.W).x
    //   val rightX = splitter.toPoint(CDir.E).x
    //   val self = theBbox
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

    def splitHorizontal(x: Int@@FloatRep): List[LTBounds] = {
      if (intersectsX(x)) {
        val leftHalf = theBbox.copy(width=x-theBbox.left)
        val rightHalf = theBbox.copy(left=x, width=theBbox.width-leftHalf.width)
        List(leftHalf, rightHalf)
      } else List(theBbox)
    }

    def intersectsX(x: Int@@FloatRep):Boolean = {
      theBbox.left <= x &&  x <= theBbox.right
    }


    def union(b: LTBounds): LTBounds = {
      val left   = min(theBbox.left, b.left)
      val top    = min(theBbox.top, b.top)
      val right = max(theBbox.right, b.right)
      val bottom = max(theBbox.bottom, b.bottom)
      LTBounds(
        left, top,
        right-left,
        bottom-top
      )
    }

    def intersection(b: LTBounds): Option[LTBounds] = {
      val left   = max(theBbox.left, b.left)
      val top    = max(theBbox.top, b.top)
      val right = min(theBbox.right, b.right)
      val bottom = min(theBbox.bottom, b.bottom)
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

    def toPointUpLeft(): Point = toPoint(NW)
    def toPointUpRight(): Point = toPoint(NE)
    def toPointDownLeft(): Point = toPoint(SW)
    def toPointDownRight(): Point = toPoint(SE)

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
      Point((theBbox.left), FloatRep(0)),
      Point((theBbox.right), FloatRep(0))
    )

    def yProjection(): Line = Line(
      Point(FloatRep(0), (theBbox.top)),
      Point(FloatRep(0), (theBbox.bottom))
    )


    def toCenterPoint: Point = Point(
      (theBbox.left+theBbox.width/2),
      (theBbox.top+theBbox.height/2)
    )

    def centerDistanceTo(other: LTBounds): Double = {
      val cx = (theBbox.left+theBbox.width/2).asDouble
      val cy = (theBbox.top+theBbox.height/2).asDouble
      val cx2 = (other.left+other.width/2).asDouble
      val cy2 = (other.top+other.height/2).asDouble

      math.sqrt(
        math.pow((cx-cx2), 2) + math.pow((cy-cy2), 2)
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
      s"""(l:${left.pp}, t:${top.pp}, w:${width.pp}, h:${height.pp})"""
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
      s"""(l:${left.pp}, b:${bottom.pp}, w:${width.pp}, h:${height.pp})"""
    }
  }
}
