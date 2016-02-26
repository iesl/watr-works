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

import js._

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
    // "c" -> ((e: MousetrapEvent) => createCanvas()),
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

  def timex[R](block: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    val ms = (t1 - t0)/1000000.0d
    (result,  ms)
  }

  def time[R](prefix: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"${prefix}: " + (t1 - t0)/1000000.0d + "ms")
    result
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
    fabricCanvas.on("mouse:over", {(e:Event) =>
      e.target.setStroke("red")
      fabricCanvas.renderAll()
      true
    })


    fabricCanvas.on("mouse:out", ({(e:Event) =>
      e.target.setStroke("blue")
      fabricCanvas.renderAll()
      true
    }))


    val bboxes = time("get bboxes from server") {
      server.getCharLevelOverlay(svgFilename).call()
    }

    println(s" fabricCanvas.renderOnAddRemove = ${fabricCanvas.renderOnAddRemove} ")
    var totalT:Double = 0
    fabricCanvas.renderOnAddRemove = false
    println(s" (2) fabricCanvas.renderOnAddRemove = ${fabricCanvas.renderOnAddRemove} ")


    bboxes.foreach{ overlays =>
      overlays.foreach { bbox =>
        val (_, t) = timex {
          addBBoxRect(bbox, "blue")
        }
        totalT = totalT + t
      }
      println(s"avg bbox addition took ${totalT / overlays.length}")
      fabricCanvas.renderAll()
      fabricCanvas.renderOnAddRemove = true
      println(s" (2) fabricCanvas.renderOnAddRemove = ${fabricCanvas.renderOnAddRemove} ")
    }

    true
  }

  def createCanvas(): Boolean = {
    val canvas = Canvas(
      "test-canvas",
      CanvasOpts()
    )


    val rect = Rect(
      top         = 3,
      left        = 3,
      width       = 20,
      height      = 30
        // fill        = "red",
        // stroke      = "black",
        // strokeWidth = 2
    )

    canvas.add(rect)
    true
  }

  def createView(): Unit = {
    server.createView(svgFilename).call().foreach(applyHtmlUpdates(_))
  }
}
