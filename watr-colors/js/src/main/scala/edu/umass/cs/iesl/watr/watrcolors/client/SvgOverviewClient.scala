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

  def fabricCanvas: fabric.Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  override val initKeys = Keybindings(List(
    "c" -> ((e: MousetrapEvent) => createCharLevelOverlay()),
    "b" -> ((e: MousetrapEvent) => createCermineOverlay()),
    "t" -> ((e: MousetrapEvent) => initSelection()),
    "d" -> ((e: MousetrapEvent) => initDeletion())
  ))


  import handlers._
  def initDeletion(): Boolean = {

    for {
      bbox <- getUserBBox(self.fabricCanvas)
    } yield {
      val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
    }

    true
  }

  def initSelection(): Boolean = {
    for {
      // TODO alter cursor to reflect selection mode
      bbox <- getUserBBox(fabricCanvas)
    } yield {
      println(s"got user bbox: ${bbox}")
      val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
      val bboxAbs = translateBBox(-offset.left, -offset.top, bbox)
      server.onSelectBBox(artifactId, bboxAbs).call().foreach{ applyHtmlUpdates(_) }
      addBBoxRect(bboxAbs, "black")
    }
    true
  }




  def addBBoxRect(bbox: BBox, color: String): Unit = {
    // bbox.info

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
    rect.title = bbox.info

    fabricCanvas.add(rect)
  }



  def createCermineOverlay(): Boolean = {
    server.getCermineOverlay(artifactId).call().foreach{ overlays =>
      overlays.foreach { bbox =>
        addBBoxRect(bbox, "green")
      }
    }

    // var hoverTarget: fabric.FabricObject = _

    fabricCanvas.on("mouse:over", ((options: fabric.Options) => {
      val title = options.target.title
      jQuery("#selection-info").html(s"title: ${title}")
    }))

    fabricCanvas.on("mouse:out", ((options: fabric.Options) => {
      jQuery("#selection-info").html(s"title: ")
    }))
    true
  }


  def alignBboxToDiv(divID: String, bbox: BBox): BBox = {
    val offset = jQuery(divID).offset().asInstanceOf[native.JQueryPosition]
    translateBBox(-offset.left, -offset.top, bbox)
  }

  def createCharLevelOverlay(): Boolean = {
    for {
      bbox <- getUserBBox(self.fabricCanvas)
    } yield {
      val aligned = alignBboxToDiv("#overlay-container", bbox)
      val bboxes = server.getCharLevelOverlay(artifactId, aligned).call()

      fabricCanvas.renderOnAddRemove = false

      bboxes.foreach{ overlays =>
        overlays.foreach { bbox =>
          addBBoxRect(bbox, "blue")
        }
        fabricCanvas.renderAll()
        fabricCanvas.renderOnAddRemove = true
      }
    }



    true
  }

  def createView(): Unit = {
    server.createView(artifactId).call().foreach(applyHtmlUpdates(_))
  }
}
