package edu.umass.cs.iesl.watr
package watrcolors
package client
package parts


import scala.scalajs.js
import labeling._
import geometry._
import native._

object DragSelectHandlers {

  // def apply(handleGesture: PartialFunction[Gesture, Unit], handleDrag: LTBounds => Unit): js.Function1[DragSelectEvent, Unit]= {
  def apply(handleGesture: PartialFunction[Gesture, Unit], handleDrag: LTBounds => Unit): Unit = {

    val dragSelectCallback: js.Function1[DragSelectEvent, Unit] = (event: DragSelectEvent) => {
      event.rect.foreach { rect =>
        val l = rect.x1
        val t = rect.y1
        val w = rect.x2-rect.x1
        val h = rect.y2-rect.y1
        handleGesture(SelectRegion(LTBounds.Doubles(l, t, w, h)))
      }

      event.point.foreach { point =>
        val x = point.x
        val y = point.y
        handleGesture(Click(Point.Doubles(x, y)))
      }

      event.move.foreach { rect =>
        val l = rect.x1
        val t = rect.y1
        val w = rect.x2-rect.x1
        val h = rect.y2-rect.y1
        handleDrag(LTBounds.Doubles(l, t, w, h))
      }

      ()

    }

    DOMGlobalScope.initD3DragSelect(dragSelectCallback)
  }

}
