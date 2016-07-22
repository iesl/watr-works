package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import native.fabric


import scala.concurrent.Future
import scala.async.Async.{async, await}

import scala.collection.mutable

import GeometricFigure._
import TraceLog._

trait FabricCanvasOperations {
  import org.scalajs.jquery.jQuery

  def fabricCanvas: fabric.Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  def addShape(shape: GeometricFigure, color: String): Unit = {
    shape match {
      case  p: Point =>
        // addCircle(p, color)
        addLTBoundsRect(LTBounds(
          p.x-1, p.y-1, 2, 2
        ), color)

      case  Line(p1: Point, p2: Point) =>

      case b:LTBounds =>
        addLTBoundsRect(b, color)
    }

  }

  def addCircle(point: Point, color: String): Unit = {
    val radius = 3

    val c = new fabric.Circle()
    c.left = point.x - radius
    c.top = point.y - radius
    c.radius = radius
    // c.top         =.y
    // c.left        = bbox.x
    // c.width       = bbox.width
    // c.height      = bbox.height
    c.stroke      = color
    c.strokeWidth = 1
    c.fill        = ""
    c.hasControls = false
    c.hasBorders  = false
    c.selectable  = false

    fabricCanvas.add(c)
  }

  def addLTBoundsRect(bbox: LTBounds, color: String): Unit = {

    val rect = fabric.Rect()
    rect.top         = bbox.top
    rect.left        = bbox.left
    rect.width       = bbox.width
    rect.height      = bbox.height
    rect.stroke      = color
    rect.strokeWidth = 1
    rect.fill        = ""
    rect.hasControls = false
    rect.hasBorders  = false
    rect.selectable  = false

    fabricCanvas.add(rect)
  }

  def addPath(path: Seq[Point], color: String): Unit = {
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
