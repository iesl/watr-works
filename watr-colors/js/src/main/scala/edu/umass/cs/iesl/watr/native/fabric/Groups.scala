package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSName
import js.JSConverters._

@js.native @JSName("fabric.Group")
class Group(
  objs: js.Array[FabricObject],
  options: js.Object,
  isAlreadyGrouped: Boolean
) extends FabricCollection with FabricObject {}

object Group {
  def apply(
    objs: Seq[FabricObject],
    options: js.Object = js.Dynamic.literal()
  ): Group = new Group(
    objs.toJSArray, options, false
  )
}
