package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSName


@js.native @JSName("fabric.Image")
class Image(
  options: js.Object
) extends FabricObject {}

@js.native @JSName("fabric.Image")
object Image extends js.Object {

  def fromURL(
    url: String,
    cb: js.Function1[Image, Unit],
    options: js.Object = js.Dynamic.literal()
  ): Unit = js.native

}

