package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSGlobal
import js.JSConverters._

@js.native @JSGlobal("fabric.Group")
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
