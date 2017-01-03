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

  // def loadSVGFromURL(url: String, cb: js.Function2[]): Unit = {// }
  // def on(event: String, f:js.Function): Event = js.native

}

@js.native @JSName("fabric.util")
object util extends js.Object {
  /**
    * Loads image element from given url and passes it to a callback
    * @memberOf fabric.util
    * @param {String} url URL representing an image
    * @param {Function} callback Callback; invoked with loaded image
    * @param {Any} [context] Context to invoke callback in
    * @param {Object} [crossOrigin] crossOrigin value to set image element to
    */
  def loadImage(url: String, callback: js.Function): Unit = js.native

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


