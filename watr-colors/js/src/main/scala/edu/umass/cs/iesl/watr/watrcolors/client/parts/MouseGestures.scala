package edu.umass.cs.iesl.watr
package watrcolors
package client
package parts

// import wiring._

// import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
// import scala.scalajs.js.annotation.JSExport

// import native.fabric
// import native.fabric._

// import scala.concurrent.Future
// import scala.async.Async.{async, await}
// import scala.collection.mutable
// import scala.scalajs.js

// import geometry._
// import geometry.syntax._
// import org.scalajs.dom
// import org.querki.jquery.JQueryEventObject
// import org.querki.jquery.JQuery

// trait MouseGestures extends HtmlCanvasRendering {

//   def fabricCanvas: fabric.Canvas


//   def addShape(shape: GeometricFigure, color: String, bg: String, opacity: Float): fabric.FabricObject = {
//     val cshape = createShape(shape, color, bg, opacity)
//     fabricCanvas.add(cshape)

//     cshape
//   }

//   def canvasDivOffset() = jQuery(s"#canvas")
//     .offset()
//     .asInstanceOf[native.JQueryPosition]

//   def translatePath(x0: Double, y0: Double, ps: Seq[Point]): Seq[Point] = {
//     ps.map(p => Point(p.x + x0, p.y + y0))
//   }

//   def divOffset(): Point = {
//     val offset = canvasDivOffset()
//     Point(offset.left, offset.top)
//   }

//   def getCanvasPoint(pageX: Int, pageY: Int): Point = {
//     Point(pageX.doubleValue(), pageY.doubleValue())
//       .translate(-divOffset)
//   }

//   def getUserPath(c: fabric.Canvas): Future[Seq[Point]] = {

//     val chan = CanvasMouseChannels(c)

//     val path = mutable.ArrayBuffer[(Int, Int)]()

//     async {
//       var res = await(chan.mousedown())

//       path.append((res.e.pageX, res.e.pageY))

//       res = await(chan.mousemove | chan.mouseup)
//       while(res.e.`type` == "mousemove"){
//         path.append((res.e.pageX, res.e.pageY))
//         res = await(chan.mousemove | chan.mouseup)
//       }

//       path.append((res.e.pageX, res.e.pageY))

//       path.map(xy => Point(xy._1.toDouble, xy._2.toDouble))
//     }
//   }


//   def updateShape(fobj: FabricObject, bbox: LTBounds): Unit = {
//     fobj.left=bbox.left
//     fobj.top=bbox.top
//     fobj.width=bbox.width
//     fobj.height=bbox.height
//     fabricCanvas.renderAll(false)
//   }

//   def bounds(p1: Point, p2: Point): LTBounds = {
//     val x = math.min(p1.x, p2.x)
//     val y = math.min(p1.y, p2.y)
//     val w = math.abs(p2.x - p1.x)
//     val h = math.abs(p2.y - p1.y)
//     LTBounds(x, y, w, h)
//   }


//   def getUserInteraction(initPoint: Point): Future[GeometricFigure] = {

//     val chan = CanvasMouseChannels(fabricCanvas)

//     async {

//       // var res = await(chan.mousedown())
//       // val point1 = getCanvasPoint(res.e.pageX, res.e.pageY)

//       var res = await(chan.mousemove | chan.mouseup)

//       if(res.e.`type` == "mouseup"){
//         println(s"getUserInteraction: Point: ${initPoint}")
//         initPoint
//       } else {
//         var point2 = getCanvasPoint(res.e.pageX, res.e.pageY)
//         var selectionShape: FabricObject =  createShape(bounds(initPoint, point2), "blue", "blue", 0.1f)
//         fabricCanvas.add(selectionShape)

//         while(res.e.`type` == "mousemove"){
//           point2 = getCanvasPoint(res.e.pageX, res.e.pageY)
//           updateShape(selectionShape, bounds(initPoint, point2))
//           res = await(chan.mousemove | chan.mouseup)
//         }

//         fabricCanvas.remove(selectionShape)
//         // fabricCanvas.renderAll()
//         fabricCanvas.renderAll(false)
//         val r = bounds(initPoint, point2)
//         println(s"getUserInteraction: Rect: ${r}")
//         r
//       }
//     }
//   }

//   def getUserSelection(c: fabric.Canvas): Future[LTBounds] = {

//     val chan = CanvasMouseChannels(c)

//     async {
//       var res = await(chan.mousedown())
//       val point1 = getCanvasPoint(res.e.pageX, res.e.pageY)
//       var point2 = point1

//       val sel = createShape(bounds(point1, point1), "blue", "blue", 0.1f)
//       fabricCanvas.add(sel)

//       res = await(chan.mousemove | chan.mouseup)
//       while(res.e.`type` == "mousemove"){
//         point2 = getCanvasPoint(res.e.pageX, res.e.pageY)
//         updateShape(sel, bounds(point1, point2))
//         res = await(chan.mousemove | chan.mouseup)
//       }
//       println(s"getUserSelection: ${point1} -> $point2")

//       fabricCanvas.remove(sel)

//       bounds(point1, point2)
//     }
//   }


// }
