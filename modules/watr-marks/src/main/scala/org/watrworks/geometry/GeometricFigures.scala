package org.watrworks
package geometry

import TypeTags._

import utils.ExactFloats._
import syntax._
import utils.{M3x3Position => M3}

sealed trait GeometricFigure { self =>
  lazy val minBounds = minBoundingRect(self)
}

case class Rect(
  left   : Int@@FloatRep,
  top    : Int@@FloatRep,
  width  : Int@@FloatRep,
  height : Int@@FloatRep
) extends GeometricFigure  {
  override def toString: String = this.prettyPrint
  def right = left+width
  def bottom = top+height

  val getLeft: Double = left.asDouble()
  val getTop: Double = top.asDouble()
  val getWidth: Double = width.asDouble()
  val getHeight: Double = height.asDouble()
  val getRight: Double = right.asDouble()
  val getBottom: Double = bottom.asDouble()
}

object Rect {
  def FromInts(l: Int, t: Int, w: Int, h: Int) = Ints(l, t, w, h)

  object IntReps {
    def apply(left: Int, top: Int, width: Int, height: Int): Rect =
      Rect(FloatRep(left), FloatRep(top), FloatRep(width), FloatRep(height))

    def unapply(bbox: Rect): Option[(Int, Int, Int, Int)] = Some((
      bbox.left.unwrap,
      bbox.top.unwrap,
      bbox.width.unwrap,
      bbox.height.unwrap
    ))
  }

  object Ints {

    def apply(left: Int, top: Int, width: Int, height: Int): Rect =
      Rect(left.toFloatExact(), top.toFloatExact(), width.toFloatExact(), height.toFloatExact())

    def unapply(bbox: Rect): Option[(Int, Int, Int, Int)] = Some((
      bbox.left.asInt(),
      bbox.top.asInt(),
      bbox.width.asInt(),
      bbox.height.asInt()
    ))
  }

  object Doubles {
    def apply(left: Double, top: Double, width: Double, height: Double): Rect =
      Rect(left.toFloatExact(), top.toFloatExact(), width.toFloatExact(), height.toFloatExact())

    def unapply(bbox: Rect): Option[(Double, Double, Double, Double)] = Some((
      bbox.left.asDouble(),
      bbox.top.asDouble(),
      bbox.width.asDouble(),
      bbox.height.asDouble()
    ))
  }

  object Floats {
    def apply(left: Float, top: Float, width: Float, height: Float): Rect =
      Rect(left.toFloatExact(), top.toFloatExact(), width.toFloatExact(), height.toFloatExact())

    def unapply(bbox: Rect): Option[(Float, Float, Float, Float)] = Some((
      bbox.left.asFloat(),
      bbox.top.asFloat(),
      bbox.width.asFloat(),
      bbox.height.asFloat()
    ))
  }

  val empty = IntReps.apply(0, 0, 0, 0)
  val zero = empty

  implicit class RicherRect(val theBbox: Rect) extends AnyVal {

    def prettyPrint: String = {
      val left = theBbox.left
      val top=  theBbox.top
      val width = theBbox.width
      val height = theBbox.height
      s"""(l:${left.pp()}, t:${top.pp()}, w:${width.pp()}, h:${height.pp()})"""
    }

    def lowLeftCornerPrint: String = {
      val left = theBbox.left
      val bottom = theBbox.bottom
      s"""[${left.pp()}, ${bottom.pp()}]"""
    }

    def compactPrint: String = {
      val left = theBbox.left
      val top=  theBbox.top
      val width = theBbox.width
      val height = theBbox.height
      s"""[${left.pp()}, ${top.pp()}, ${width.pp()}, ${height.pp()}]"""
    }

    def uriString: String = {
      val left = theBbox.left
      val top=  theBbox.top
      val width = theBbox.width
      val height = theBbox.height
      s"""${left.pp()}+${top.pp()}+${width.pp()}+${height.pp()}"""
    }
  }

}

case class Point(
  x: Int@@FloatRep,
  y: Int@@FloatRep
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
      Point(x.toFloatExact(), y.toFloatExact())

    def unapply(p: Point): Option[(Int, Int)] =
      Some((p.x.asInt(), p.y.asInt()))
  }

  object Doubles {
    def apply(x: Double, y: Double): Point =
      Point(x.toFloatExact(), y.toFloatExact())

    def unapply(p: Point): Option[(Double, Double)] = {
      Some((p.x.asDouble(), p.y.asDouble()))
    }
  }

  val origin = Ints(0, 0)
  val zero = origin
}

case class Line(
  p1: Point,
  p2: Point
) extends GeometricFigure {
  override def toString: String = this.prettyPrint()
}

case class Trapezoid(
  topLeft: Point,
  topWidth: Int@@FloatRep,
  bottomLeft: Point,
  bottomWidth: Int@@FloatRep
) extends GeometricFigure  {
  def height(): Int@@FloatRep = bottomLeft.y - topLeft.y
}

object Trapezoid {

  def isHorizontal(l: Line): Boolean = l.p1.y==l.p2.y

  def fromHorizontals(l1: Line, l2: Line): Trapezoid = {
    assume(isHorizontal(l1) && isHorizontal(l2))
    val Seq(ltop, lbottom) = Seq(l1, l2).sortBy(_.p1.y)
    val ltn = ltop.sortPointsAsc
    val lbn = lbottom.sortPointsAsc

    val tWidth = ltn.p2.x - ltn.p1.x
    val bWidth = lbn.p2.x - lbn.p1.x

    Trapezoid(
      ltn.p1, tWidth,
      lbn.p1, bWidth
    )
  }

  import utils.ExactFloats._
  import utils.Interval
  import utils.EnrichNumerics._
  implicit class RicherTrapezoid(val self: Trapezoid) {
    def leftBaseAngle(): Double = {
      toPoint(M3.BottomLeft).angleTo(toPoint(M3.TopLeft))
    }

    def rightBaseAngle(): Double = {
      math.Pi - toPoint(M3.BottomRight).angleTo(toPoint(M3.TopRight))
    }


    private val defaultAngleTolerance = 0.08d // ~ 4.6 deg.
    private val pi2 = math.Pi/2

    def leftBaseAngleType(tolerance: Double = defaultAngleTolerance): AngleType = {
      val lla = leftBaseAngle()
      val deg90 = Interval.DblBeginLen(pi2-tolerance, tolerance*2)

      if (lla.withinRange(deg90)) AngleType.Right
      else if (lla < pi2) AngleType.Acute
      else AngleType.Obtuse

    }

    def rightBaseAngleType(tolerance: Double = defaultAngleTolerance): AngleType = {
      val lra = rightBaseAngle()
      val deg90 = Interval.DblBeginLen(pi2-tolerance, tolerance*2)

      if (lra.withinRange(deg90)) AngleType.Right
      else if (lra < pi2) AngleType.Acute
      else AngleType.Obtuse
    }

    def classifyBaseAngles(tolerance: Double = defaultAngleTolerance): (AngleType, AngleType) = {
      (leftBaseAngleType(tolerance), rightBaseAngleType(tolerance))
    }

    def toPoint(dir: M3): Point = {
      val Trapezoid(Point(tlx, tly), twidth, Point(blx, bly), bwidth) = self

      dir match {
        case M3.Top         => ???
        case M3.Bottom      => ???
        case M3.Right       => ???
        case M3.Left        => ???
        case M3.TopLeft     => self.topLeft
        case M3.BottomLeft  => self.bottomLeft
        case M3.TopRight    => Point(tlx+twidth, tly)
        case M3.BottomRight => Point(blx+bwidth, bly)
        case M3.Center      => ???
      }
    }

    def toLine(dir: M3): Line = dir match {
      case M3.Top         => Line(toPoint(M3.TopLeft), toPoint(M3.TopRight))
      case M3.Bottom      => Line(toPoint(M3.BottomLeft), toPoint(M3.BottomRight))
      case M3.Right       => Line(toPoint(M3.TopRight), toPoint(M3.BottomRight))
      case M3.Left        => Line(toPoint(M3.TopLeft), toPoint(M3.BottomLeft))
      case M3.TopLeft     => ???
      case M3.BottomLeft  => ???
      case M3.TopRight    => ???
      case M3.BottomRight => ???
      case M3.Center      => ???
    }
    def prettyPrint(): String = {

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

      // val tline = self.toLine(M3.Top)
      // val bline = self.toLine(M3.Bottom)
      val lldeg = (lla * 180) / math.Pi
      val lrdeg = (lra * 180) / math.Pi

      s"${lls}◻${lrs}:<${lldeg.pp()}º,${lrdeg.pp()}º>mbr:${minBoundingRect(self)}"
    }

  }
}

// TODO this is a formatting/layout data type, move it from geometry types
case class Padding(
  left: Int@@FloatRep,
  top: Int@@FloatRep,
  right: Int@@FloatRep,
  bottom: Int@@FloatRep
) {
  override def toString: String = {
    s"""pad[l:${left.pp()}, t:${top.pp()}, r:${right.pp()}, b:${bottom.pp()}]"""
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
      Padding(left.toFloatExact(), top.toFloatExact(), right.toFloatExact(), bottom.toFloatExact())

    def apply(p: Int): Padding = apply(p, p, p, p)

    def unapply(pad: Padding): Option[(Int, Int, Int, Int)] = {
      Some((
        pad.left.asInt(),
        pad.top.asInt(),
        pad.right.asInt(),
        pad.bottom.asInt()
      ))
    }
  }

  object Doubles {
    def apply(left: Double, top: Double, right: Double, bottom: Double): Padding =
      Padding(left.toFloatExact(), top.toFloatExact(), right.toFloatExact(), bottom.toFloatExact())

    def apply(p: Double): Padding = apply(p, p, p, p)

    def unapply(pad: Padding): Option[(Double, Double, Double, Double)] = {
      Some((
        pad.left.asDouble(),
        pad.top.asDouble(),
        pad.right.asDouble(),
        pad.bottom.asDouble()
      ))
    }
  }
}


trait GeometricFigureCodecs extends TypeTagCodecs {
  import io.circe
  import io.circe._, io.circe.generic.auto._
  import circe.syntax._
  import cats.syntax.functor._

  type I2 = (Int, Int)
  type I3 = (Int, Int, Int)
  type I4 = (Int, Int, Int, Int)
  type FloatRep2 = (Int@@FloatRep, Int@@FloatRep)
  type FloatRep3 = (Int@@FloatRep, Int@@FloatRep, Int@@FloatRep)
  type FloatRep4 = (Int@@FloatRep, Int@@FloatRep, Int@@FloatRep, Int@@FloatRep)

  implicit val FloatRepEncoder: Encoder[Int@@FloatRep] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val FloatRepDecoder: Decoder[Int@@FloatRep] = Decoder.decodeInt.map(FloatRep(_))

  implicit val RectEncoder: Encoder[Rect] = Encoder[FloatRep4]
    .contramap(b => (b.left, b.top, b.width, b.height))

  implicit val RectDecoder: Decoder[Rect] = Decoder[I4]
    .map(b => Rect.IntReps(b._1, b._2, b._3, b._4))

  implicit val Enc_Point: Encoder[Point] =
    Encoder[FloatRep2].contramap(b => (b.x, b.y))

  implicit val Dec_Point: Decoder[Point] = Decoder[I2]
    .map(b => Point.IntReps(b._1, b._2))

  implicit val LineEncoder: Encoder[Line] = Encoder[(Point, Point)]
    .contramap(b => (b.p1, b.p2))

  implicit val LineDecoder: Decoder[Line] = Decoder[(Point, Point)]
    .map(b => Line(b._1, b._2))

  implicit val TrapezoidEnc: Encoder[Trapezoid] = Encoder[(Point, Int@@FloatRep, Point, Int@@FloatRep)]
    .contramap(b => (b.topLeft, b.topWidth, b.bottomLeft, b.bottomWidth))

  implicit val TrapezoidDec: Decoder[Trapezoid] = Decoder[(Point, Int@@FloatRep, Point, Int@@FloatRep)]
    .map(b => Trapezoid(b._1, b._2, b._3, b._4))

  implicit val GeometricFigureDecoder: Decoder[GeometricFigure] =
    List[Decoder[GeometricFigure]](
      Decoder[Point].widen,
      Decoder[Line].widen,
      Decoder[Rect].widen,
      Decoder[Trapezoid].widen,
    ).reduce(_ or _)

  implicit val GeometricFigureEncoder: Encoder[GeometricFigure] = Encoder.instance {
    case v: Point => v.asJson
    case v: Line => v.asJson
    case v: Rect => v.asJson
    case v: Trapezoid => v.asJson
  }

}

object GeometryCodecs extends GeometricFigureCodecs
