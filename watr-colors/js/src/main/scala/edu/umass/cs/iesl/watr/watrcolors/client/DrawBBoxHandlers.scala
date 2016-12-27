package edu.umass.cs.iesl.watr
package watrcolors
package client

// import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import native.fabric

import scala.concurrent.Future
import scala.async.Async.{async, await}
import scala.collection.mutable

import geometry._
import GeometricFigure._

trait FabricCanvasOperations extends HtmlCanvasRendering {

  def addShape(shape: GeometricFigure, color: String, bg: String, opacity: Float): fabric.FabricObject = {
    val cshape = createShape(shape, color, bg, opacity)
    getFabric("fabric-canvas").add(cshape)

    cshape
  }

  def addLTBoundsRect(bbox: LTBounds, color: String, bg: String, opacity: Float): fabric.FabricObject = {
    val cshape = addLTBoundsRect(bbox, color, bg, opacity)
    getFabric("fabric-canvas").add(cshape)
    cshape
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
