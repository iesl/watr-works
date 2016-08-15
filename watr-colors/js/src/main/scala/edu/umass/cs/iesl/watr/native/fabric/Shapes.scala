package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSName
// import scala.language.dynamics

@js.native @JSName("fabric.Object")
trait FabricObject extends ObjectProperties {

  def getClipTo(): js.Function = js.native
  def setClipTo(clipTo: js.Function): FabricObject = js.native

  def getTransformMatrix(): Array[Int] = js.native // transformMatrix
  def setTransformMatrix(transformMatrix: Array[Int]): FabricObject = js.native

  def getVisible(): Boolean = js.native
  def setVisible(visible: Boolean): FabricObject = js.native

  def getShadow(): FabricObject = js.native

  def getStroke(): String = js.native
  def setStroke(stroke: String): FabricObject = js.native

  def getStrokeWidth(): Int = js.native
  def setStrokeWidth(strokeWidth: Int): FabricObject = js.native

  def getOriginX(): String = js.native
  def setOriginX(originX: String): FabricObject = js.native
  def getOriginY(): String = js.native
  def setOriginY(originY: String): FabricObject = js.native


  def getFill(): String = js.native
  def setFill(fill: String): FabricObject = js.native

  def getOpacity(): Double = js.native
  def setOpacity(value: Double): FabricObject = js.native


  def getAngle(): Double = js.native

  // In pixels
  def getTop(): Int = js.native
  def setTop(px: Int): FabricObject = js.native


  // Left value (in pixels)
  def getLeft(): Int = js.native
  def setLeft(px: Int): FabricObject = js.native


  def getScaleX(): Double = js.native
  def setScaleX(value: Double): FabricObject = js.native
  def getScaleY(): Double = js.native
  def setScaleY(value: Double): FabricObject = js.native


  def getFlipX(): Boolean = js.native
  def setFlipX(value: Boolean): FabricObject = js.native
  def getFlipY(): Boolean = js.native
  def setFlipY(value: Boolean): FabricObject = js.native


  //  [: {Object} // options] Options object
  // def initialize(options) {

  // @param [: {Object} // options] Options object
  // @param [: {Object} // options] Options object
  // @param [: {Object} // options] Options object
  // @param [: {Object} // options] Options object
  // def setOptions(options) {


  // @param ctx: {CanvasRenderingContext2D} //  Context
  // @param fromLeft: Boolean //  When true, context is transformed to object's top/left corner. This is used when rendering text on Node
  // def transform(ctx, fromLeft) {


  // @param [: {Array} // propertiesToInclude] Any properties that you might want to additionally include in the output
  // : {Object} = js.native // Object representation of an instance
  // def toObject(propertiesToInclude) {


  // @param [: {Array} // propertiesToInclude] Any properties that you might want to additionally include in the output
  // : {Object} = js.native // Object representation of an instance
  // def toDatalessObject(propertiesToInclude) {


  // @param object: {Object} //
  // : String = js.native //
  // def toString() {


  // @param property: {String} //  Property name
  // : {Any} = js.native // value of a property
  // def get(property) {


  // @param key: {String|Object} //  Property name or object (if object, iterate over the object properties)
  // @param value: {Object|Function} //  Property value (if function, the value is passed into it and its return value is used as a new one)
  // : FabricObject = js.native
  // def set(key, value) {


  // @param key: {String} //
  // @param value: {Any} //
  // : FabricObject = js.native // thisArg
  // def setOnGroup() {


  // @param property: {String} //  Property to toggle
  // : FabricObject = js.native // thisArg
  // def toggle(property) {


  // @param value: {String} //  Value to set sourcePath to
  // : FabricObject = js.native // thisArg
  // def setSourcePath(value) {




  // @memberOf fabric.Object.prototype
  // def getViewportTransform
  // : Boolean = js.native // flipY value // TODO


  // def getViewportTransform() {


  // @param ctx: {CanvasRenderingContext2D} //  Context to render on
  // @param [: Boolean // noTransform] When true, context is not transformed
  // def render(ctx, noTransform) {


  // @param ctx: {CanvasRenderingContext2D} //  Context to render on
  // @param ctx: {CanvasRenderingContext2D} //  Context to set the dash line on
  // @param dashArray: Array[Int] //  array representing dashes
  // @param alternative: js.Function //  function to call if browaser does not support lineDash
  // @param ctx: {CanvasRenderingContext2D} //  Context to render on
  // @param [: Boolean // noTransform] When true, context is not transformed
  // @param ctx: {CanvasRenderingContext2D} //  Context to render on
  // @param ctx: {CanvasRenderingContext2D} //  Context to render on
  // @param ctx: {CanvasRenderingContext2D} //  Context to render on
  // @param ctx: {CanvasRenderingContext2D} //  Context to render on
  // @param callback: js.Function //  Callback is invoked with a clone as a first argument
  // @param [: {Array} // propertiesToInclude] Any properties that you might want to additionally include in the output
  // : FabricObject = js.native // clone of an instance
  // def clone(callback, propertiesToInclude) {


  // @param callback: js.Function //  callback, invoked with an instance as a first argument
  // : FabricObject = js.native // thisArg
  // def cloneAsImage(callback) {


  // @param options: {Object} //  Options object
  // @param [: {String} // options.format=png] The format of the output image. Either "jpeg" or "png"
  // @param [: {Number} // options.quality=1] Quality level (0..1). Only used for jpeg.
  // @param [: {Number} // options.multiplier=1] Multiplier to scale by
  // @param [: {Number} // options.left] Cropping left offset. Introduced in v1.2.14
  // @param [: {Number} // options.top] Cropping top offset. Introduced in v1.2.14
  // @param [: {Number} // options.width] Cropping width. Introduced in v1.2.14
  // @param [: {Number} // options.height] Cropping height. Introduced in v1.2.14
  // : {String} = js.native // Returns a data: URL containing a representation of the object in the format specified by options.format
  // def toDataURL(options) {


  // @param type: {String} //  Type to check against
  // : Boolean = js.native //
  // def isType(type) {


  // : {Number} = js.native // complexity of this instance
  // def complexity() {


  // @param [: {Array} // propertiesToInclude] Any properties that you might want to additionally include in the output
  // : {Object} = js.native // JSON
  // def toJSON(propertiesToInclude) {


  // def setGradient(
  //   strokeOrFill: String, // stroke|fill
  //   gtype: String, // radial|linear
  //   start: (Int, Int),
  //   end: (Int, Int),
  //   colorStops: js.Object,
  //   radii: Option[(Int, Int)] = None
  //   // gradientTransform: js.Object = js.Dynamic.literal()
  // ): FabricObject = {

  //   val rd1 = radii.map(_._1).getOrElse(0)
  //   val rd2 = radii.map(_._2).getOrElse(0)

  //   _setGradient(strokeOrFill,js.Dynamic.literal(
  //     `type` = gtype,
  //     x1 = start._1,
  //     x2 = start._2,
  //     y1 = end._1,
  //     y2 = end._2,
  //     r1 = rd1,
  //     r2 = rd2,
  //     colorStops = colorStops
  //     // gradientTransform = gradientTransform
  //   ))
  // }

  // @param property: {String} //  Property name 'stroke' or 'fill'
  // @param [: {Object} // options] Options object
  // @param [: {String} // options.type] Type of gradient 'radial' or 'linear'
  // @param [: {Number} // options.x1=0] x-coordinate of start point
  // @param [: {Number} // options.y1=0] y-coordinate of start point
  // @param [: {Number} // options.x2=0] x-coordinate of end point
  // @param [: {Number} // options.y2=0] y-coordinate of end point
  // @param [: {Number} // options.r1=0] Radius of start point (only for radial gradients)
  // @param [: {Number} // options.r2=0] Radius of end point (only for radial gradients)
  // @param [: {Object} // options.colorStops] Color stops object eg. {0: 'ff0000', 1: '000000'}
  // @param [: {Object} // options.gradientTransform] transforMatrix for gradient
  // : FabricObject = js.native // thisArg
  // def setGradient(property, options) {

  def setGradient(property: String, options: js.Object): FabricObject = js.native


  // @param options: {Object} //  Options object
  // @param options: {(String|HTMLImageElement)} // .source Pattern source
  // @param [: {String} // options.repeat=repeat] Repeat property of a pattern (one of repeat, repeat-x, repeat-y or no-repeat)
  // @param [: {Number} // options.offsetX=0] Pattern horizontal offset from object's left/top corner
  // @param [: {Number} // options.offsetY=0] Pattern vertical offset from object's left/top corner
  // : FabricObject = js.native // thisArg
  // def setPatternFill(options) {


  // @param [: {Object|String} // options] Options object or string (e.g. "2px 2px 10px rgba(0,0,0,0.2)")
  // @param [: {String} // options.color=rgb(0,0,0)] Shadow color
  // @param [: {Number} // options.blur=0] Shadow blur
  // @param [: {Number} // options.offsetX=0] Shadow horizontal offset
  // @param [: {Number} // options.offsetY=0] Shadow vertical offset
  // : FabricObject = js.native // thisArg
  // def setShadow(options) {


  // @param color: {String} //  Color value
  // : FabricObject = js.native // thisArg
  // def setColor(color) {


  // @param angle: {Number} //  Angle value (in degrees)
  // : FabricObject = js.native // thisArg
  // def setAngle(angle) {


  // : FabricObject = js.native // thisArg
  // def centerH() {


  // : FabricObject = js.native // thisArg
  // def viewportCenterH() {


  // : FabricObject = js.native // thisArg
  // def centerV() {


  // : FabricObject = js.native // thisArg
  // def viewportCenterV() {


  // : FabricObject = js.native // thisArg
  // def center() {


  // : FabricObject = js.native // thisArg
  // def viewportCenter() {


  // : FabricObject = js.native // thisArg
  // def remove() {


  // @param e: {Event} //  Event to operate upon
  // @param [: {Object} // pointer] Pointer to operate upon (instead of event)
  // : {Object} = js.native // Coordinates of a pointer (x, y)
  // @memberOf fabric.Object
  // @memberOf fabric.Object
  // def getLocalPointer(e, pointer) {


  // @param ctx: {CanvasRenderingContext2D} //  Rendering canvas context
  // @memberOf fabric.Object


  // TODO Use Dynamic trait for this...
  var title: String = js.native

  def animate(prop: String, delta: String, tweenFunc: js.Object): Unit = js.native
  def animate(prop: String, delta: String): Unit = js.native

}

@js.native @JSName("fabric.Line")
class Line extends FabricObject {
  var     x1: Number = js.native
  var     y1: Number = js.native
  var     x2: Number = js.native
  var     y2: Number = js.native
}

@js.native @JSName("fabric.Rect")
class Rect extends FabricObject {}


@js.native @JSName("fabric.Circle")
class Circle extends FabricObject {

  def getRadiusX(): Double = js.native
  def getRadiusY(): Double = js.native

  def setRadius(value: Double): Circle = js.native

  var     radius: Number = js.native
  var     startAngle: Number = js.native
  var     endAngle: Number = js.native // =  pi * 2,

}



@js.native
class PropBuilder() extends ObjectProperties  {
  // val accum = js.Dynamic
  // accum.global.sdf = 23

  // def updateDynamic(name: String)(value: Any): PropBuilder = {
  //   // map += name -> value
  //   // accum.literal(name = value)
  //   this
  // }
}

object props {
  def apply() = {
    new PropBuilder()
  }
}


object Rect {
  def apply(): Rect = new Rect()
}


// @js.native
// trait RectOpts extends ShapeOpts {
//   def width       : Double = js.native
//   def height      : Double = js.native
// }

// @js.native
// trait PathOpts extends ShapeOpts {}



// object RectOpts {
//   def apply(
//     top: Double,
//     left: Double,
//     width: Double,
//     height: Double,
//     fill: String = "",
//     strokeWidth: Int = 1,
//     stroke: String = "blue"
//   ): RectOpts =
//     js.Dynamic.literal(
//       top = top,
//       left = left,
//       width = width,
//       height = height,
//       fill = fill,
//       strokeWidth = strokeWidth,
//       stroke = stroke
//     ).asInstanceOf[RectOpts]
// }

// @js.native @JSName("fabric.Path")
// class PathNative(
//   opts: PathOpts
// ) extends FabricObject
