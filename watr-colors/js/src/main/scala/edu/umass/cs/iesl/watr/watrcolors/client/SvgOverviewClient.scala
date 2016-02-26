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
import js._
import org.scalajs.dom
import dom.html.Canvas

@JSExport
class SvgOverview(
  svgFilename: String
) extends ClientView {

  val server = ServerWire("svg")[SvgOverviewApi]

  lazy val fabricCanvas: Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[Canvas]
  }

  // TODO create new bbox
  // On hover over bbox, display bbox info in sidebar
  override val initKeys = Keybindings(List(
    "a" -> ((e: MousetrapEvent) => createCharLevelOverlay()),
    "b" -> ((e: MousetrapEvent) => createCermineOverlay())
  ))


  def addBBoxRect(bbox: BBox, color: String): Unit = {

    val rect = Rect(
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

  def setupSVGPaneHandlers(): Unit = {

    // val c = fabricCanvas.getElement()

    val canvas: Canvas = null
    val renderer = canvas.getContext("2d")
                     .asInstanceOf[dom.CanvasRenderingContext2D]

    def rect = canvas.getBoundingClientRect()

    type ME = dom.MouseEvent

    val mousemove = new Channel[ME](canvas.onmousemove = _)
    val mouseup = new Channel[ME](canvas.onmouseup = _)
    val mousedown = new Channel[ME](canvas.onmousedown = _)

    val _ = async {

      while(true){
        val start = await(mousedown())
        renderer.beginPath()
        renderer.moveTo(
          start.clientX - rect.left,
          start.clientY - rect.top
        )

        var res = await(mousemove | mouseup)
        while(res.`type` == "mousemove"){
          renderer.lineTo(
            res.clientX - rect.left,
            res.clientY - rect.top
          )
          renderer.stroke()
          res = await(mousemove | mouseup)
        }

        renderer.fill()
        await(mouseup())
        renderer.clearRect(0, 0, 1000, 1000)
      }
    }

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
