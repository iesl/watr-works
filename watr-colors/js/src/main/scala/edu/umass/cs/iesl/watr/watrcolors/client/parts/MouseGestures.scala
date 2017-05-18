package edu.umass.cs.iesl.watr
package watrcolors
package client
package parts

import wiring._

// import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
// import scala.scalajs.js.annotation.JSExport


import scala.concurrent.Future
import scala.async.Async.{async, await}
// import scala.collection.mutable
// import scala.scalajs.js

import geometry._
// import geometry.syntax._
// import org.scalajs.dom
// import org.querki.jquery.JQueryEventObject
// import org.querki.jquery.JQuery

trait MouseGestures {



  // def addShape(shape: GeometricFigure, color: String, bg: String, opacity: Float): fabric.FabricObject = {
  //   val cshape = createShape(shape, color, bg, opacity)
  //   fabricCanvas.add(cshape)

  //   cshape
  // }

  // def canvasDivOffset() = jQuery(s"#canvas")
  //   .offset()
  //   .asInstanceOf[native.JQueryPosition]

  // def translatePath(x0: Double, y0: Double, ps: Seq[Point]): Seq[Point] = {
  //   ps.map(p => Point(p.x + x0, p.y + y0))
  // }

  // def divOffset(): Point = {
  //   val offset = canvasDivOffset()
  //   Point(offset.left, offset.top)
  // }

  // def getCanvasPoint(pageX: Int, pageY: Int): Point = {
  //   Point(pageX.doubleValue(), pageY.doubleValue())
  //     .translate(-divOffset)
  // }



  // def updateShape(fobj: FabricObject, bbox: LTBounds): Unit = {
  //   fobj.left=bbox.left
  //   fobj.top=bbox.top
  //   fobj.width=bbox.width
  //   fobj.height=bbox.height
  //   fabricCanvas.renderAll(false)
  // }

  def bounds(p1: Point, p2: Point): LTBounds = {
    val x = math.min(p1.x, p2.x)
    val y = math.min(p1.y, p2.y)
    val w = math.abs(p2.x - p1.x)
    val h = math.abs(p2.y - p1.y)
    LTBounds(x, y, w, h)
  }

  // def mouseChannels = D3MouseChannels(fabricCanvas)
  // def mouseChannels: D3MouseChannels

  // def getUserInteraction(initPoint: Point): Future[GeometricFigure] = {


  //   async {

  //     // var res = await(mouseChannels.mousedown())
  //     // val point1 = getCanvasPoint(res.e.pageX, res.e.pageY)

  //     var res = await(mouseChannels.mousemove | mouseChannels.mouseup)

  //     if(res.e.`type` == "mouseup"){
  //       println(s"getUserInteraction: Point: ${initPoint}")
  //       initPoint
  //     } else {
  //       var point2 = getCanvasPoint(res.e.pageX, res.e.pageY)
  //       var selectionShape: FabricObject =  createShape(bounds(initPoint, point2), "blue", "blue", 0.1f)
  //       fabricCanvas.add(selectionShape)

  //       while(res.e.`type` == "mousemove"){
  //         point2 = getCanvasPoint(res.e.pageX, res.e.pageY)
  //         updateShape(selectionShape, bounds(initPoint, point2))
  //         res = await(mouseChannels.mousemove | mouseChannels.mouseup)
  //       }

  //       fabricCanvas.remove(selectionShape)
  //       // fabricCanvas.renderAll()
  //       fabricCanvas.renderAll(false)
  //       val r = bounds(initPoint, point2)
  //       println(s"getUserInteraction: Rect: ${r}")
  //       r
  //     }
  //   }
  // }

}
