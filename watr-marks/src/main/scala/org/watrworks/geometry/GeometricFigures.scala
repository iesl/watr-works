package org.watrworks
package geometry

import scalaz.Equal
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import TypeTags._

import utils.ExactFloats._
import scala.scalajs.js.annotation._
import GeometryImplicits._

sealed trait GeometricFigure { self =>
  lazy val minBounds = GeometryImplicits.minBoundingRect(self)
}

@JSExportTopLevel("LTBounds")
case class LTBounds(
  left   : Int@@FloatRep,
  top    : Int@@FloatRep,
  width  : Int@@FloatRep,
  height : Int@@FloatRep
) extends GeometricFigure  {
  override def toString: String = this.prettyPrint
  def right = left+width
  def bottom = top+height

  @JSExport("left")   val getLeft: Double = left.asDouble
  @JSExport("top")    val getTop: Double = top.asDouble
  @JSExport("width")  val getWidth: Double = width.asDouble
  @JSExport("height") val getHeight: Double = height.asDouble
  @JSExport("right")  val getRight: Double = right.asDouble
  @JSExport("bottom") val getBottom: Double = bottom.asDouble
}


@JSExportTopLevel("LTBoundsCompanion")
object LTBounds {

  @JSExport("FromInts")
  def FromInts(l: Int, t: Int, w: Int, h: Int) = Ints(l, t, w, h)

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
      LTBounds(left.toFloatExact(), top.toFloatExact, width.toFloatExact, height.toFloatExact)

    def unapply(bbox: LTBounds): Option[(Int, Int, Int, Int)] = Some((
      bbox.left.asInt,
      bbox.top.asInt,
      bbox.width.asInt,
      bbox.height.asInt
    ))
  }

  object Doubles {
    def apply(left: Double, top: Double, width: Double, height: Double): LTBounds =
      LTBounds(left.toFloatExact(), top.toFloatExact, width.toFloatExact, height.toFloatExact)

    def unapply(bbox: LTBounds): Option[(Double, Double, Double, Double)] = Some((
      bbox.left.asDouble,
      bbox.top.asDouble,
      bbox.width.asDouble,
      bbox.height.asDouble
    ))
  }

  object Floats {
    def apply(left: Float, top: Float, width: Float, height: Float): LTBounds =
      LTBounds(left.toFloatExact(), top.toFloatExact, width.toFloatExact, height.toFloatExact)

    def unapply(bbox: LTBounds): Option[(Float, Float, Float, Float)] = Some((
      bbox.left.asFloat(),
      bbox.top.asFloat(),
      bbox.width.asFloat(),
      bbox.height.asFloat()
    ))
  }

  val empty = IntReps.apply(0, 0, 0, 0)
  val zero = empty

  implicit class RicherLTBounds(val theBbox: LTBounds) extends AnyVal {
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

}

case class LBBounds(
  left: Int@@FloatRep,
  bottom: Int@@FloatRep,
  width: Int@@FloatRep,
  height: Int@@FloatRep
) extends GeometricFigure {
  override def toString: String = this.prettyPrint

  def toLTBounds: LTBounds = {
    LTBounds(
      left = left,
      top = bottom-height,
      width = width, height = height
    )
  }
  def prettyPrint: String = {
    s"""(l:${left.pp}, b:${bottom.pp}, w:${width.pp}, h:${height.pp})"""
  }
}

object LBBounds {

  object Doubles {
    def apply(left: Double, bottom: Double, width: Double, height: Double): LBBounds =
      LBBounds(left.toFloatExact(), bottom.toFloatExact, width.toFloatExact, height.toFloatExact)

    def unapply(bbox: LBBounds): Option[(Double, Double, Double, Double)] = Some((
      bbox.left.asDouble,
      bbox.bottom.asDouble,
      bbox.width.asDouble,
      bbox.height.asDouble
    ))
  }

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
      Point(x.toFloatExact(), y.toFloatExact())

    def unapply(p: Point): Option[(Int, Int)] =
      Some((p.x.asInt, p.y.asInt))
  }

  object Doubles {
    def apply(x: Double, y: Double): Point =
      Point(x.toFloatExact, y.toFloatExact)

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

case class Trapezoid(
  topLeft: Point,
  topWidth: Int@@FloatRep,
  bottomLeft: Point,
  bottomWidth: Int@@FloatRep
) extends GeometricFigure  { self =>

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
}

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
      Padding(left.toFloatExact, top.toFloatExact, right.toFloatExact, bottom.toFloatExact)

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
      Padding(left.toFloatExact, top.toFloatExact, right.toFloatExact, bottom.toFloatExact)

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


trait GeometricFigureCodecs extends TypeTagCodecs {
  import io.circe
  import circe.generic.semiauto._
  import io.circe._, io.circe.generic.auto._
  import circe.syntax._

  import utils.DoOrDieHandlers._

  implicit val Enc_Int_FloatRep: Encoder[Int@@FloatRep] = Encoder.encodeInt.contramap(_.unwrap)
  implicit val Dec_Int_FloatRep: Decoder[Int@@FloatRep] = Decoder.decodeInt.map(FloatRep(_))

  // val Enc_LTBounds_v1: ObjectEncoder[LTBounds] = deriveEncoder
  val Dec_LTBounds_v1: Decoder[LTBounds] = deriveDecoder

  implicit val Enc_LTBounds: Encoder[LTBounds] = Encoder.instance { bbox =>
    val LTBounds.IntReps(l, t, w, h) = bbox
    List(l, t, w, h).asJson
  }

  val Dec_LTBounds_v2: Decoder[LTBounds] = Decoder.instance { hCursor =>
    val intVals = hCursor.focus.orDie().decodeOrDie[List[Int]]()
    intVals match {
      case List(l, t, w, h) =>
        Right(LTBounds.IntReps(l, t, w, h))

      case _ =>
        Left(DecodingFailure("",List()))
    }
  }

  implicit val Dec_LTBounds = Dec_LTBounds_v1.or(Dec_LTBounds_v2)

  implicit val Enc_LBBounds: Encoder.AsObject[LBBounds] = deriveEncoder
  implicit val Dec_LBBounds: Decoder[LBBounds] = deriveDecoder

  implicit val Enc_Point: Encoder.AsObject[Point] = deriveEncoder
  implicit val Dec_Point: Decoder[Point] = deriveDecoder

  implicit val Enc_Line: Encoder.AsObject[Line] = deriveEncoder
  implicit val Dec_Line: Decoder[Line] = deriveDecoder

  implicit val Enc_Trapezoid: Encoder.AsObject[Trapezoid] = deriveEncoder
  implicit val Dec_Trapezoid: Decoder[Trapezoid] = deriveDecoder

  implicit val Enc_GeometricFigure: Encoder.AsObject[GeometricFigure] = deriveEncoder
  implicit val Dec_GeometricFigure: Decoder[GeometricFigure] = deriveDecoder

}

object GeometryCodecs extends GeometricFigureCodecs
