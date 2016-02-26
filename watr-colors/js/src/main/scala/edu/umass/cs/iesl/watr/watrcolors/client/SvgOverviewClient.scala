package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.{js => sjs}

import autowire._
import boopickle.DefaultBasic._

import Picklers._

import org.scalajs.jquery.jQuery
import scala.concurrent.Promise

import org.scalajs.dom
import dom.html.{Canvas => JsCanvas}

import native.fabric
import native.mousetrap._

@JSExport
class SvgOverview(
  svgFilename: String
) extends ClientView {

  val server = ServerWire("svg")[SvgOverviewApi]

  lazy val fabricCanvas: fabric.Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  lazy val upperCanvas: JsCanvas = {
    jQuery("#fabric-canvas").prop("fabric").upperCanvasEl.asInstanceOf[JsCanvas]
  }
  lazy val jsCanvas: JsCanvas = {
    jQuery("#fabric-canvas").asInstanceOf[JsCanvas]
  }

  override val initKeys = Keybindings(List(
    "a" -> ((e: MousetrapEvent) => createCharLevelOverlay()),
    "b" -> ((e: MousetrapEvent) => createCermineOverlay()),
    "d" -> ((e: MousetrapEvent) => setupSVGPaneHandlers())
  ))


  def addBBoxRect(bbox: BBox, color: String): Unit = {

    val rect = fabric.Rect(
      top         = bbox.y,
      left        = bbox.x,
      width       = bbox.width,
      height      = bbox.height,
      stroke      = color
    )
    rect.hasControls = false
    rect.hasBorders = false
    rect.selectable = false

    fabricCanvas.add(rect)
  }

  def createCermineOverlay(): Boolean = {
    server.getCermineOverlay(svgFilename).call().foreach{ overlays =>
      overlays.foreach { bbox =>
        addBBoxRect(bbox, "green")
      }
    }
    true
  }

  def createCharLevelOverlay(): Boolean = {
    val bboxes = time("get bboxes from server") {
      server.getCharLevelOverlay(svgFilename).call()
    }

    fabricCanvas.renderOnAddRemove = false

    bboxes.foreach{ overlays =>
      overlays.foreach { bbox =>
        addBBoxRect(bbox, "blue")
      }
      fabricCanvas.renderAll()
      fabricCanvas.renderOnAddRemove = true
    }

    true
  }

  import rx._

  import scala.async.Async.{async, await}

  def setupSVGPaneHandlers(): Boolean = {

    val canvas: JsCanvas = upperCanvas
    // val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    // def rect = canvas.getBoundingClientRect()

    type ME = dom.MouseEvent

    val mousemove = new Channel[ME](canvas.onmousemove = _)
    val mouseup = new Channel[ME](canvas.onmouseup = _)
    val mousedown = new Channel[ME](canvas.onmousedown = _)

    println("setting up async")
    val _ = async {

      while(true){
        val start = await(mousedown())
        println("async: mousedown")
        // renderer.beginPath()
        // renderer.moveTo(
        //   start.clientX - rect.left,
        //   start.clientY - rect.top
        // )

        var res = await(mousemove | mouseup)
        println("async: mousemove | up")
        while(res.`type` == "mousemove"){
          // renderer.lineTo(
          //   res.clientX - rect.left,
          //   res.clientY - rect.top
          // )
          // renderer.stroke()
          res = await(mousemove | mouseup)
        }

        // renderer.fill()
        await(mouseup())
        println("async: mouseup")
        // renderer.clearRect(0, 0, 1000, 1000)
      }
    }

    true
  }

  def createView(): Unit = {
    server.createView(svgFilename).call().foreach(applyHtmlUpdates(_))
  }
}



// fabricCanvas.on("mouse:over", {(e:Event) =>
//   e.target.setStroke("red")
//   fabricCanvas.renderAll()
//   true
// })


// fabricCanvas.on("mouse:out", ({(e:Event) =>
//   e.target.setStroke("blue")
//   fabricCanvas.renderAll()
//   true
// }))
