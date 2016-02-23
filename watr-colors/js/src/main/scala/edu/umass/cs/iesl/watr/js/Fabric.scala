package edu.umass.cs.iesl.watr
package js

import scala.scalajs.js
import js.annotation.JSName


@js.native
object fabric extends js.Object {
  def version: String = js.native
  def isTouchSupported: Boolean = js.native
  def isLikelyNode: Boolean = js.native
  def DPI: Int = js.native
  def devicePixelRatio: Float = js.native
}

@js.native
@JSName("fabric.Rect")
class RectNative(
  opts: RectOpts
) extends Rect


@js.native
trait Rect extends js.Object

object Rect {
  def apply(top: Double, left: Double, width: Double, height: Double): Rect = {
    new RectNative(RectOpts(top, left, width, height))
  }
}

@js.native
trait RectOpts extends js.Object {
  def top         : Double = js.native
  def left       : Double = js.native
  def width       : Double = js.native
  def height      : Double = js.native
  def fill        : String = js.native
  def strokeWidth : Int = js.native
  def stroke      : String = js.native
}

object RectOpts {
  def apply(
    top: Double,
    left: Double,
    width: Double,
    height: Double,
    fill: String = "",
    strokeWidth: Int = 1,
    stroke: String = "blue"
  ): RectOpts =
    js.Dynamic.literal(
      top = top,
      left = left,
      width = width,
      height = height,
      fill = fill,
      strokeWidth = strokeWidth,
      stroke = stroke
    ).asInstanceOf[RectOpts]
}


@js.native @JSName("fabric.Canvas")
class CanvasNative(
  el: String,
  options: CanvasOpts
) extends Canvas

@js.native
trait Canvas extends js.Object {
  def add(r: Rect): Unit = js.native
}

object Canvas {
  def apply(el: String, opts: CanvasOpts): Canvas = {
    new CanvasNative(el, CanvasOpts())
  }

}

@js.native trait CanvasOpts extends js.Object

object CanvasOpts {
  def apply(): CanvasOpts =
    js.Dynamic.literal(
    ).asInstanceOf[CanvasOpts]
}
