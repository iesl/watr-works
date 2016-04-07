package edu.umass.cs.iesl.watr
package watrcolors
package client

// import scala.concurrent.{Future, Promise}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.{js => sjs}

import org.scalajs.dom
import org.scalajs.jquery.jQuery

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.fabric
import native.mousetrap._

import rx._

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
  artifactId: String
) extends ClientView { self =>

  val server = ServerWire("svg")[SvgOverviewApi]

  lazy val fabricCanvas: fabric.Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  override val initKeys = Keybindings(List(
    "a" -> ((e: MousetrapEvent) => createCharLevelOverlay()),
    "b" -> ((e: MousetrapEvent) => createCermineOverlay()),
    "t" -> ((e: MousetrapEvent) => initSelection()),
    "d" -> ((e: MousetrapEvent) => initDeletion())
  ))

  var eventHandler: EventHandler = _

  def initDeletion(): Boolean = {
    eventHandler = new DeleteBBoxHandlers{
      def fabricCanvas: fabric.Canvas = self.fabricCanvas
      def server = self.server
      def addBBoxRect(bbox: BBox, color: String): Unit = self.addBBoxRect(bbox, color)
      def artifactId = self.artifactId
      def clientView = self
    }
    eventHandler.installHandlers()
    true
  }
  def initSelection(): Boolean = {
    eventHandler = new DrawBBoxHandlers{
      def fabricCanvas: fabric.Canvas = self.fabricCanvas
      def server = self.server
      def addBBoxRect(bbox: BBox, color: String): Unit = self.addBBoxRect(bbox, color)
      def artifactId = self.artifactId
      def clientView = self
    }
    eventHandler.installHandlers()
    true
  }


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
    server.getCermineOverlay(artifactId).call().foreach{ overlays =>
      overlays.foreach { bbox =>
        addBBoxRect(bbox, "green")
      }
    }
    true
  }

  def createCharLevelOverlay(): Boolean = {
    val bboxes = time("get bboxes from server") {
      server.getCharLevelOverlay(artifactId).call()
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

  def createView(): Unit = {
    server.createView(artifactId).call().foreach(applyHtmlUpdates(_))
  }
}
