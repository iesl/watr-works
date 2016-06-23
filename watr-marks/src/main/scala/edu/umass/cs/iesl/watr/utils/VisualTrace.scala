package edu.umass.cs.iesl.watr
package utils

import spindex._
// import scala.language.implicitConversions
import play.api.libs.json
import json._

import scala.collection.mutable
// import cats.free.Free
// import cats.free.{Trampoline}
// import cats.data.Xor
import cats.~>
// import cats.Id

// import scala.concurrent._
// import scala.concurrent.duration._
// import ExecutionContext.Implicits.global

// import cats.derived._, functor._, legacy._
// import cats.Functor
// import cats.std.future._
// import cats.std.option._
// import cats.std.list._
// import cats.syntax.traverse._

import freek._
import scala.language.existentials

import FabricShapes._


sealed trait TraceLogEntry



case class VisualTraceLog(
  viewport: LTBounds,
  traceOverlays: Seq[FabricShape],
  messages: Seq[String]
)

object TraceLog {
  import VisualTrace._
  val entries: mutable.MutableList[VisualTrace.DSL[_]] = mutable.MutableList()

  var currPRG = Noop

  def trace[T](p: => VisualTrace.DSL[T]): Unit = {
    entries += p
  }

  def traces[T](p: => Seq[VisualTrace.DSL[T]]): Unit = {
    entries ++= p
  }


  def getAndClearTraceLog(): Seq[VisualTrace.DSL[_]] = {
    val ret = Seq(entries:_*)
    entries.clear()
    ret
  }
}




object VisualTrace {
  sealed trait DSL[A]

  type PRG = VisualTrace.DSL :|: FXNil

  // implicit def ltBoundsToShape(b: LTBounds): LogShape = RectShape(b)
  // implicit def PointToShape(b: Point): LogShape = PointShape(b)

  case object Noop extends DSL[Unit]
  case class SetViewport(b: LTBounds) extends DSL[Unit]
  case class GetViewport() extends DSL[LTBounds]
  case class Show(s: Shape) extends DSL[Unit]
  case class ShowVDiff(d1: Double, d2: Double) extends DSL[Unit]
  case class FocusOn(s: Shape) extends DSL[Unit]
  case class HRuler(s: Double) extends DSL[Shape]
  case class Message(s: String) extends DSL[Unit]

  case class And[A](t1: DSL[A], t2: DSL[A]) extends DSL[Unit]
  case class AndThen[A](t1: DSL[A], t2: DSL[A]) extends DSL[Unit]


  // def translate(shape: LogShape) = for {
  //   viewport <- GetViewport().freek[PRG]
  // } yield shape

  // def hRuler(y: Double) = for {
  //   sh <- HRuler(y).freek[PRG]
  //   shape <- translate(sh)
  //   _ <- Show(shape).freek[PRG]
  // } yield ()

}



object FabricShapes {
  case class FabricShape(
    jsval: JsObject
  )

  def line(): FabricShape = {
    base
  }

  def base(): FabricShape = {
    FabricShape(Json.obj(
          //   "type"                     -> this.type,
          //   "originX"                  -> this.originX,
          //   "originY"                  -> this.originY,
          //   "left"                     -> toFixed(this.left, NUM_FRACTION_DIGITS),
          //   "top"                      -> toFixed(this.top, NUM_FRACTION_DIGITS),
          //   "width"                    -> toFixed(this.width, NUM_FRACTION_DIGITS),
          //   "height"                   -> toFixed(this.height, NUM_FRACTION_DIGITS),
          //   "fill"                     -> this.fill && this.fill.toObject) ? this.fill.toObject() : this.fill,
          //   "stroke"                   -> this.stroke && this.stroke.toObject) ? this.stroke.toObject() : this.stroke,
          //   "strokeWidth"              -> toFixed(this.strokeWidth, NUM_FRACTION_DIGITS),
          //   "strokeDashArray"          -> this.strokeDashArray ? this.strokeDashArray.concat() : this.strokeDashArray,
          //   "strokeLineCap"            -> this.strokeLineCap,
          //   "strokeLineJoin"           -> this.strokeLineJoin,
          //   "strokeMiterLimit"         -> toFixed(this.strokeMiterLimit, NUM_FRACTION_DIGITS),
          //   "scaleX"                   -> toFixed(this.scaleX, NUM_FRACTION_DIGITS),
          //   "scaleY"                   -> toFixed(this.scaleY, NUM_FRACTION_DIGITS),
          //   "angle"                    -> toFixed(this.getAngle(), NUM_FRACTION_DIGITS),
          //   "flipX"                    -> this.flipX,
          //   "flipY"                    -> this.flipY,
          //   "opacity"                  -> toFixed(this.opacity, NUM_FRACTION_DIGITS),
          //   "shadow"                   -> this.shadow && this.shadow.toObject) ? this.shadow.toObject() : this.shadow,
          //   "visible"                  -> this.visible,
          //   "clipTo"                   -> this.clipTo && String(this.clipTo),
          //   "backgroundColor"          -> this.backgroundColor,
          //   "fillRule"                 -> this.fillRule,
          //   "globalCompositeOperation" -> this.globalCompositeOperation,
          //   "transformMatrix"          -> this.transformMatrix ? this.transformMatrix.concat() : this.transformMatrix,
          //   "skewX"                    -> toFixed(this.skewX, NUM_FRACTION_DIGITS),
          //   "skewY"                    -> toFixed(this.skewY, NUM_FRACTION_DIGITS)
    ))

  }


// implicit class RicherLTBounds(val d: LTBounds) extends AnyVal {}
// implicit class RicherPoint(val d: Point) extends AnyVal {}
// implicit class RicherFloat(val d: Float) extends AnyVal {}
// implicit class RicherDouble(val d: Double) extends AnyVal {}

// Fabric base object:
          // object = {
          //   type:                     this.type,
          //   originX:                  this.originX,
          //   originY:                  this.originY,
          //   left:                     toFixed(this.left, NUM_FRACTION_DIGITS),
          //   top:                      toFixed(this.top, NUM_FRACTION_DIGITS),
          //   width:                    toFixed(this.width, NUM_FRACTION_DIGITS),
          //   height:                   toFixed(this.height, NUM_FRACTION_DIGITS),
          //   fill:                     (this.fill && this.fill.toObject) ? this.fill.toObject() : this.fill,
          //   stroke:                   (this.stroke && this.stroke.toObject) ? this.stroke.toObject() : this.stroke,
          //   strokeWidth:              toFixed(this.strokeWidth, NUM_FRACTION_DIGITS),
          //   strokeDashArray:          this.strokeDashArray ? this.strokeDashArray.concat() : this.strokeDashArray,
          //   strokeLineCap:            this.strokeLineCap,
          //   strokeLineJoin:           this.strokeLineJoin,
          //   strokeMiterLimit:         toFixed(this.strokeMiterLimit, NUM_FRACTION_DIGITS),
          //   scaleX:                   toFixed(this.scaleX, NUM_FRACTION_DIGITS),
          //   scaleY:                   toFixed(this.scaleY, NUM_FRACTION_DIGITS),
          //   angle:                    toFixed(this.getAngle(), NUM_FRACTION_DIGITS),
          //   flipX:                    this.flipX,
          //   flipY:                    this.flipY,
          //   opacity:                  toFixed(this.opacity, NUM_FRACTION_DIGITS),
          //   shadow:                   (this.shadow && this.shadow.toObject) ? this.shadow.toObject() : this.shadow,
          //   visible:                  this.visible,
          //   clipTo:                   this.clipTo && String(this.clipTo),
          //   backgroundColor:          this.backgroundColor,
          //   fillRule:                 this.fillRule,
          //   globalCompositeOperation: this.globalCompositeOperation,
          //   transformMatrix:          this.transformMatrix ? this.transformMatrix.concat() : this.transformMatrix,
          //   skewX:                    toFixed(this.skewX, NUM_FRACTION_DIGITS),
          //   skewY:                    toFixed(this.skewY, NUM_FRACTION_DIGITS)
          // };


// Circle
// return extend(this.callSuper('toObject', propertiesToInclude), {
//   radius: this.get('radius'),
//   startAngle: this.startAngle,
//   endAngle: this.endAngle
// });


// Text
// toObject: function(propertiesToInclude) {
//   var object = extend(this.callSuper('toObject', propertiesToInclude), {
//     text:                 this.text,
//     fontSize:             this.fontSize,
//     fontWeight:           this.fontWeight,
//     fontFamily:           this.fontFamily,
//     fontStyle:            this.fontStyle,
//     lineHeight:           this.lineHeight,
//     textDecoration:       this.textDecoration,
//     textAlign:            this.textAlign,
//     textBackgroundColor:  this.textBackgroundColor
//   });



// Line
// toObject: function(propertiesToInclude) {
//   return extend(this.callSuper('toObject', propertiesToInclude), this.calcLinePoints());
// },

// /**
//   * Recalculates line points given width and height
//   * @private
//   */
// calcLinePoints: function() {
//   var xMult = this.x1 <= this.x2 ? -1 : 1,
//     yMult = this.y1 <= this.y2 ? -1 : 1,
//     x1 = (xMult * this.width * 0.5),
//     y1 = (yMult * this.height * 0.5),
//     x2 = (xMult * this.width * -0.5),
//     y2 = (yMult * this.height * -0.5);

//   return {
//     x1: x1,
//     x2: x2,
//     y1: y1,
//     y2: y2
//   };
// },
// object VisualTraceInterpreter extends (VisualTrace ~> cats.Id) {
//   import VisualTrace._

//   def apply[A](a: VisualTrace[A]) = a match {
//     case SetViewport(b: LTBounds) =>
//     case Show(s: LogShape) =>
//     case ShowVDiff(d1: Double, d2: Double) =>
//     case FocusOn(s: LogShape) =>
//     case HRuler(s: Double) =>
//     case Message(s: String) =>
//     case And(t1, t2) =>
//     case AndThen(t1, t2) =>
//   }
// }

}

object interp {
  import VisualTrace._

  class EmitTracesInterpreter() extends (VisualTrace.DSL ~> cats.Id) {

    // val entries: mutable.MutableList[VisualTrace.DSL[_]] = mutable.MutableList()

    def apply[A](a: VisualTrace.DSL[A]) = a match {
      case Noop =>
      case SetViewport(b: LTBounds) =>
      case GetViewport() =>
        println("set!")
        Bounds.empty
      case Show(s: Shape) =>
        println("show!")
        Json.obj(
          "" -> ""
        )

        ()


      case ShowVDiff(d1: Double, d2: Double) =>
      case FocusOn(s: Shape) =>
      case HRuler(s: Double) =>
        println("rule!")
          ???
      case Message(s: String) =>
      case And(t1, t2) =>
      case AndThen(t1, t2) =>
    }
  }

}
