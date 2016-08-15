package edu.umass.cs.iesl.watr
package watrcolors
package client

// import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import native.fabric

import scala.concurrent.Future
import scala.async.Async.{async, await}
import scala.collection.mutable

import GeometricFigure._

trait FabricCanvasOperations {
  // import org.scalajs.jquery.jQuery

  def fabricCanvas: fabric.Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  def addShape(shape: GeometricFigure, color: String, bg: String, opacity: Float): fabric.FabricObject = {
    shape match {
      case p: Point =>
        val radius = 20

        val c = new fabric.Circle()
        c.left = p.x - radius
        c.top = p.y - radius
        c.radius = radius
        c.stroke      = color
        c.strokeWidth = 2
        c.fill        = bg
        c.hasControls = false
        c.hasBorders  = false
        c.selectable  = false
        // c.opacity = opacity

        fabricCanvas.add(c)
        c

      case Line(p1: Point, p2: Point) =>
        val l = new fabric.Line()
        l.x1 = p1.x
        l.y1 = p1.y
        l.x2 = p2.x
        l.y2 = p2.y
        l.stroke      = color
        l.strokeWidth = 2
        l.fill        = bg
        // l.opacity = opacity

        fabricCanvas.add(l)
        l

      case b:LTBounds => addLTBoundsRect(b, color, bg, opacity)

      case b:LBBounds =>
        val lt = LTBounds(b.left, b.bottom-b.height, b.width, b.height)
        addLTBoundsRect(lt, color, bg, opacity)

    }
  }


  def addLTBoundsRect(bbox: LTBounds, color: String, bg: String, opacity: Float): fabric.FabricObject = {

    val rect = fabric.Rect()
    rect.top         = bbox.top
    rect.left        = bbox.left
    rect.width       = bbox.width
    rect.height      = bbox.height
    rect.stroke      = color
    rect.strokeWidth = 1
    rect.fill        = bg
    rect.hasControls = false
    rect.hasBorders  = false
    rect.selectable  = false
    rect.opacity = opacity

    fabricCanvas.add(rect)
    rect

    // rect.animate('angle', '-=5', { onChange: canvas.renderAll.bind(canvas) });
    // val canv = js.Dynamic.literal(
    //   c=jQuery("#fabric-canvas")
    // )

    // rect.animate("opacity", "-=0.02")

    // rect.animate("top", "+=10", js.Dynamic.literal(
    //   onChange=canv.c.renderAll.bind(canv.c),
    //   duration=2000
    // ))

  }

}

object handlers {


  def getUserPath(c: fabric.Canvas): Future[Seq[Point]] = {

    val chan = CanvasMouseChannels(c)

    val path = mutable.ArrayBuffer[(Int, Int)]()

    async {
      var res = await(chan.mousedown())

      path.append((res.e.pageX, res.e.pageY))

      res = await(chan.mousemove | chan.mouseup)
      while(res.e.`type` == "mousemove"){
        path.append((res.e.pageX, res.e.pageY))
        res = await(chan.mousemove | chan.mouseup)
      }

      path.append((res.e.pageX, res.e.pageY))

      path.map(xy => Point(xy._1.toDouble, xy._2.toDouble))
    }
  }



  def getUserLTBounds(c: fabric.Canvas): Future[LTBounds] = {
    println("getUserLTBounds")

    val chan = CanvasMouseChannels(c)

    async {
      var res = await(chan.mousedown())
      val px1: Int = res.e.pageX
      val py1: Int = res.e.pageY

      res = await(chan.mousemove | chan.mouseup)
      while(res.e.`type` == "mousemove"){
        res = await(chan.mousemove | chan.mouseup)
      }

      val px2 = res.e.pageX
      val py2 = res.e.pageY
      val x = math.min(px1, px2)
      val y = math.min(py1, py2)
      val w = math.abs(px2 - px1)
      val h = math.abs(py2 - py1)

      LTBounds(x.toDouble, y.toDouble, w.toDouble, h.toDouble)
    }
  }

  def translateLTBounds(x0: Double, y0: Double, bb: LTBounds): LTBounds = {
    bb.copy(
      left = bb.left + x0,
      top = bb.top + y0
    )
  }

  def translatePath(x0: Double, y0: Double, ps: Seq[Point]): Seq[Point] = {
    ps.map(p => Point(p.x + x0, p.y + y0))
  }

}
