package edu.umass.cs.iesl.watr
package watrcolors

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport

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
    "a" -> ((e: MousetrapEvent) => createCharLevelOverlay())
  ))

  def addBBoxRect(bbox: BBox): Unit = {

    val rect = Rect(
      top         = bbox.y,
      left        = bbox.x,
      width       = bbox.width,
      height      = bbox.height
    )

    fabricCanvas.add(rect)
  }

  def createCharLevelOverlay(): Boolean = {

    server.getCharLevelOverlay(svgFilename).call().foreach{ overlays =>
      overlays.foreach { bbox =>
        addBBoxRect(bbox)
      }
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
