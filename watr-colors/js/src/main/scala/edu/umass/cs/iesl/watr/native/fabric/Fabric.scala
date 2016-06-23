package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSName


@js.native
object fabric extends js.Object {
  def version: String = js.native
  def isTouchSupported: Boolean = js.native
  def isLikelyNode: Boolean = js.native
  def DPI: Int = js.native
  def devicePixelRatio: Float = js.native

  // def loadSVGFromURL(url: String, cb: js.Function2[]): Unit = {
  // }

  // def on(event: String, f:js.Function): Event = js.native
}



@js.native
trait Event extends js.Object {
  def clientX: Int = js.native
  def clientY: Int = js.native
  def pageX: Int = js.native
  def pageY: Int = js.native
  def `type`: String = js.native
}


@js.native
trait Options extends js.Object {
  def target: FabricObject = js.native
  def e: Event = js.native
}


