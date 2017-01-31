package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
// import scala.scalajs.js.annotation.JSExport

import native.fabric

import scala.concurrent.Future
import scala.async.Async.{async, await}
import scala.collection.mutable

import geometry._

object MouseGestures {

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


}

// def canvasBorder: Int = 10 // ???
// def canvasH: Int = fabricCanvas.getHeight
// def canvasW: Int = fabricCanvas.getWidth
// def canvasX: Int = canvasOffset.left.toInt
// def canvasY: Int = canvasOffset.top.toInt



// def selectViaLine(): Boolean = {
//   for {
//     userPath <- getUserPath(self.fabricCanvas)
//   } yield {
//     val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
//     val pathAbs = translatePath(-offset.left, -offset.top, userPath)

//     server.onDrawPath(artifactId, pathAbs).call().foreach{ applyHtmlUpdates(_) }
//   }

//   true
// }

// // val jqOverlayContainer = jQuery("#overlay-container")

// val c = new fabric.Canvas("fabric-canvas", fabric.CanvasOptions)


// jQuery("#fabric-canvas").prop("fabric", c)
// c.uniScaleTransform = true

// var imgCount = jQuery(".page-image").length
// var imgReady = 0

// jQuery(".page-image").map({ (elem: Element) =>

//   def loaded(): Unit = {
//     imgReady += 1
//     if (imgReady == imgCount) {
//       val h = jQuery("#img-container").height()
//       fabricCanvas.setHeight(h.toInt+1)
//       val w = jQuery("#img-container").width()
//       fabricCanvas.setWidth(w.toInt+1)

//       println(s"fabric canvas loaded h/w = ${h}/${w}")

//     }
//   }

//   elem.addEventListener("load", {(e: Event) => loaded() })
//     elem.addEventListener("error", (e: Event) => {
//       println("image load encountered an error")
//     })

// })
