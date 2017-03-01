package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
// import scala.scalajs.js.annotation.JSExport

import native.fabric
import native.fabric._

import scala.concurrent.Future
import scala.async.Async.{async, await}
import scala.collection.mutable
// import scala.scalajs.js

import geometry._
import geometry.syntax._
import org.scalajs.dom
import org.querki.jquery.JQueryEventObject
// import org.querki.jquery.JQuery

trait MouseGestures extends HtmlCanvasRendering {

  def fabricCanvas: fabric.Canvas


  def addShape(shape: GeometricFigure, color: String, bg: String, opacity: Float): fabric.FabricObject = {
    val cshape = createShape(shape, color, bg, opacity)
    fabricCanvas.add(cshape)

    cshape
  }

  def canvasDivOffset() = jQuery(s"#canvas-container")
    .offset()
    .asInstanceOf[native.JQueryPosition]

  def translatePath(x0: Double, y0: Double, ps: Seq[Point]): Seq[Point] = {
    ps.map(p => Point(p.x + x0, p.y + y0))
  }

  def divOffset(): Point = {
    val offset = canvasDivOffset()
    Point(offset.left, offset.top)
  }

  def alignPointToDiv(divID: String, point: Point): Point = {
    point.translate(-divOffset())
  }

  def alignBboxToDiv(divID: String, bbox: LTBounds): LTBounds = {
    bbox.translate(-divOffset())
  }

  def getCanvasPoint(pageX: Int, pageY: Int): Point = {
    Point(pageX.doubleValue(), pageY.doubleValue())
      .translate(-divOffset)
  }

  def handleClick(canvasPoint: Point): Unit

  def defaultMouseHandler(): Unit = {
    val callback: (JQueryEventObject, Any) => Any =
      (event: JQueryEventObject, arg1: Any) => {
        dom.console.log(event)
        if (event.shiftKey.exists(_ == true)) {
          handleClick(
            getCanvasPoint(event.pageX, event.pageY)
          )
          false
        } else {
          true
        }
      }

    jQuery("#canvas-container").on("click", callback)

  }

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


  def updateShape(fobj: FabricObject, x: Int, y: Int, w: Int, h: Int): Unit = {
    fobj.left=x
    fobj.top=y
    fobj.width=w
    fobj.height=h
    fabricCanvas.renderAll()
  }

  def bounds(p1: Point, p2: Point): LTBounds = {
    val wh = p2 - p1
    LTBounds(p1.x, p1.y, wh.x, wh.y)
  }

  def getUserLTBounds(c: fabric.Canvas): Future[LTBounds] = {

    val chan = CanvasMouseChannels(c)


    async {
      var res = await(chan.mousedown())
      val point1 = getCanvasPoint(res.e.pageX, res.e.pageY)
      val px1: Int = res.e.pageX
      val py1: Int = res.e.pageY

      // val sel = createShape(LTBounds(point1.x.doubleValue(), point1.y.doubleValue(), 0, 0), "blue", "blue", 0.1f)
      val sel = createShape(bounds(point1, point1), "blue", "blue", 0.1f)
      fabricCanvas.add(sel)

      res = await(chan.mousemove | chan.mouseup)
      while(res.e.`type` == "mousemove"){
        val point2 = getCanvasPoint(res.e.pageX, res.e.pageY)
        // val px2 = res.e.pageX
        // val py2 = res.e.pageY
        // val x = math.min(px1, px2)
        // val y = math.min(py1, py2)
        // val w = math.abs(px2 - px1)
        // val h = math.abs(py2 - py1)
        // updateShape(sel, x, y, w, h)
        sel
        res = await(chan.mousemove | chan.mouseup)
      }

      fabricCanvas.remove(sel)

      val px2 = res.e.pageX
      val py2 = res.e.pageY
      val x = math.min(px1, px2)
      val y = math.min(py1, py2)
      val w = math.abs(px2 - px1)
      val h = math.abs(py2 - py1)

      LTBounds(x.toDouble, y.toDouble, w.toDouble, h.toDouble)
    }
  }


}
