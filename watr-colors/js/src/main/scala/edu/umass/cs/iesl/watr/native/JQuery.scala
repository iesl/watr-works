package edu.umass.cs.iesl.watr
package native

import scala.scalajs.js
import js.annotation._

@ScalaJSDefined
trait JQueryPosition extends js.Object {
  val left:Double
  val top:Double
}


// import scala.scalajs.js.`|`

@ScalaJSDefined
trait SelectionRect extends js.Object {
  def x1: Int
  def x2: Int
  def y1: Int
  def y2: Int
}

@ScalaJSDefined
trait ClickPoint extends js.Object {
  def x: Int
  def y: Int
}

@ScalaJSDefined
trait DragSelectEvent extends js.Object {
  def point: js.UndefOr[ClickPoint]
  def rect: js.UndefOr[SelectionRect]
  def move: js.UndefOr[SelectionRect]
}

@js.native @JSGlobalScope
object DOMGlobalScope extends js.Object {
  def initD3DragSelect(callback: js.Function1[DragSelectEvent, Unit]): Unit = js.native
}
