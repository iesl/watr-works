package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import native.fabric


import scala.concurrent.Future
import scala.async.Async.{async, await}

import scala.collection.mutable


trait FabricCanvasOperations {
  import org.scalajs.jquery.jQuery

  def fabricCanvas: fabric.Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  def addShape(shape: Overlay, color: String): Unit = {
    shape match {
      case  Point(x: Double, y: Double) =>

      case  Line(p1: Point, p2: Point) =>

      case b:BBox => addBBoxRect(b, color)
    }

  }


  def addBBoxRect(bbox: BBox, color: String): Unit = {

    val rect = fabric.Rect()
    rect.top         = bbox.y
    rect.left        = bbox.x
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



  def getUserBBox(c: fabric.Canvas): Future[BBox] = {

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

      BBox(x.toDouble, y.toDouble, w.toDouble, h.toDouble)
    }
  }

  def translateBBox(x0: Double, y0: Double, bb: BBox): BBox = {
    bb.copy(
      x = bb.x + x0,
      y = bb.y + y0
    )
  }

  def translatePath(x0: Double, y0: Double, ps: Seq[Point]): Seq[Point] = {
    ps.map(p => Point(p.x + x0, p.y + y0))
  }

}
