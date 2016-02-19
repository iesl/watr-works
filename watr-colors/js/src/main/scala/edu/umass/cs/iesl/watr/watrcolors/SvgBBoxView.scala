package edu.umass.cs.iesl.watr
package watrcolors

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport

import autowire._
import boopickle.DefaultBasic._
import Picklers._

@JSExport
class SvgOverview(
  svgFilename: String
) extends ClientView {
  val server = ServerWire("svg")[SvgOverviewApi]


  // TODO create new bbox
  // On hover over bbox, display bbox info in sidebar
  override val initKeys = Keybindings(List(
    "c" -> ((e: MousetrapEvent) => createCanvas)
  ))

  def createCanvas(): Boolean = {
    val canvas = Canvas(
      "test-canvas",
       CanvasOpts()
    )

    println("canvas: "+ canvas)
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

    println(s"""
Fabric values:
    ${fabric.DPI}
    ${fabric.isLikelyNode}
    ${fabric.isTouchSupported}

""")
    // ${fabric.devicePixelRatio}

  }
}
