package edu.umass.cs.iesl.watr
package native
package fabric

import scala.scalajs.js
import js.annotation.JSName
// import scala.language.dynamics

@js.native @JSName("fabric.Object")
trait FabricObject extends ObjectProperties {

  def setFill(color: String): FabricObject = js.native
  def setStroke(color: String): FabricObject = js.native


  // TODO Use Dynamic trait for this...
  var title: String = js.native
}

@js.native @JSName("fabric.Rect")
class Rect extends FabricObject {

}

// @js.native @JSName("fabric.Path")
// class PathNative(
//   opts: PathOpts
// ) extends FabricObject


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
  // def apply(
  //   top: Double, left: Double, width: Double, height: Double,
  //   fill: String = "",
  //   stroke: String = "",
  //   strokeWidth: Int = 1
  // ): Rect = {
  //   new RectNative(RectOpts(top, left, width, height, fill, strokeWidth, stroke))
  // }
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
