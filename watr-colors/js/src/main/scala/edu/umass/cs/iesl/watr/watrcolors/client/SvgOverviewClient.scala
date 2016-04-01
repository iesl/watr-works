package edu.umass.cs.iesl.watr
package watrcolors
package client

// import scala.concurrent.{Future, Promise}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.{js => sjs}

import org.scalajs.dom
import dom.html.{Canvas => JsCanvas}
import org.scalajs.jquery.jQuery

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.fabric
import native.mousetrap._

import rx._

import scala.async.Async.{async, await}

import scalatags.JsDom.all._
object util {


  implicit def rxFrag[T](r: Rx[T])(implicit ev: (T) => Frag, ctx: Ctx.Data, d: Ctx.Owner): Frag = {
    def rSafe: dom.Node = span(r()).render
    var last = rSafe
    r.triggerLater {
      val newLast = rSafe
      sjs.Dynamic.global.last = last
      last.parentNode.replaceChild(newLast, last)
      last = newLast
    }

    last
  }
}


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

  val clientX = Var(0d)
  val clientY = Var(0d)
  val screenX = Var(0d)
  val screenY = Var(0d)
  val pageX = Var(0d)
  val pageY = Var(0d)

  def setupSVGPaneHandlers(): Boolean = {

    // // def rxpanel(x: Double, y: Double)(implicit ctx: Ctx.Owner) = Rx {
    // //   div(
    // //     span("mouse x: ", x),
    // //     span("mouse y: ", y)
    // //   )
    // // }
    // import util._

    // val xx = Rx.unsafe {

    //   println("rx here")
    //   // val asdf = rxpanel(mouseX(), mouseY())(implicitly[Ctx.Owner])()
    //   Rx {
    //     val panel = div(
    //       span("mouse x: ", clientX()),
    //       span("mouse y: ", clientY()),
    //       span("screen x: ", screenX()),
    //       span("screen y: ", screenY()),
    //       span("page x: ", pageX()),
    //       span("page y: ", pageY())
    //     )
    //     jQuery("#rxinfo").replaceWith(panel.render)
    //     panel
    //   }

    //   println("rx there")
    //   panel
    // }

    // println("rx outside")

    // screenX() = 99d

    infoHoverHandlers()
    true
  }


  def infoHoverHandlers(): Unit = {
    val _ = async {
      val chan = CanvasMouseChannels(upperCanvas)

      while(true){
        println("init mouse move")
        var res = await(chan.mousemove())
        while(res.`type` == "mousemove"){
          println("mouse move")
          clientX() = res.clientX
          clientY() = res.clientY
          screenX() = res.screenX
          pageX() = res.pageX

          res = await(chan.mousemove())
        }
      }
    }
  }

  def createView(): Unit = {
    server.createView(svgFilename).call().foreach(applyHtmlUpdates(_))
  }
}
