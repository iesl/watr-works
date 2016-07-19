package edu.umass.cs.iesl.watr
package watrcolors
package client


import scala.async.Async.{async, await}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.scalajs.dom._

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.mousetrap._
import native.fabric



@JSExport
class SvgOverview(
  artifactId: String
) extends ClientView { self =>

  val server = ServerWire("svg")[SvgOverviewApi]

  override val initKeys = Keybindings(List(
    // "b" -> ((e: MousetrapEvent) => createDocumentOverlay()),
    "t" -> ((e: MousetrapEvent) => initSelection()),
    "z" -> ((e: MousetrapEvent) => selectViaLine()),
    "d" -> ((e: MousetrapEvent) => initDeletion())
  ))

  import handlers._
  def selectViaLine(): Boolean = {
    for {
      userPath <- getUserPath(self.fabricCanvas)
    } yield {
      val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
      val pathAbs = translatePath(-offset.left, -offset.top, userPath)

      server.onDrawPath(artifactId, pathAbs).call().foreach{ applyHtmlUpdates(_) }
    }

    true

  }
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

      async {
        val res = await { server.onSelectBBox(artifactId, bboxAbs).call() }
        applyHtmlUpdates(res)
        addBBoxRect(bboxAbs, "black")
      }
    }
    true
  }

// var path = new fabric.Path('M 0 0 L 200 100 L 170 200 z');



  // def createDocumentOverlay(): Boolean = {
  //   server.getDocumentOverlay(artifactId).call().foreach{ overlays =>
  //     overlays.foreach { bbox =>
  //       addBBoxRect(bbox, "green")
  //     }
  //   }


  //   fabricCanvas.on("mouse:over", ((options: fabric.Options) => {
  //     val title = options.target.title
  //     jQuery("#selection-info").html(s"title: ${title}")
  //   }))

  //   fabricCanvas.on("mouse:out", ((options: fabric.Options) => {
  //     jQuery("#selection-info").html(s"title: ")
  //   }))
  //   true
  // }


  def alignBboxToDiv(divID: String, bbox: BBox): BBox = {
    val offset = jQuery(divID).offset().asInstanceOf[native.JQueryPosition]
    translateBBox(-offset.left, -offset.top, bbox)
  }


  def createView(): Unit = async {

    val res = await {
      server.createView(artifactId).call()
    }
    applyHtmlUpdates(res)

    val jqOverlayContainer = jQuery("#overlay-container")

    val c = new fabric.Canvas("fabric-canvas", fabric.CanvasOptions)


    jQuery("#fabric-canvas").prop("fabric", c)
    c.uniScaleTransform = true

    var imgCount = jQuery(".page-image").length
    var imgReady = 0

    jQuery(".page-image").map({ (i: Int, elem: Element) =>

      def loaded(): Unit = {
        imgReady += 1
        println(s"img count = ${imgReady} / ${imgCount}")
        if (imgReady == imgCount) {
          val h = jQuery("#img-container").height()
          fabricCanvas.setHeight(h.toInt+1)

          val w = jQuery("#img-container").width()
          fabricCanvas.setWidth(w.toInt+1)

          println(s"fabric canvas loaded h/w = ${h}/${w}")

        }
      }

      elem.addEventListener("load", {(e: Event) => loaded() })
        elem.addEventListener("error", (e: Event) => {
          println("image load encountered an error")
        })

    })

  }
}
