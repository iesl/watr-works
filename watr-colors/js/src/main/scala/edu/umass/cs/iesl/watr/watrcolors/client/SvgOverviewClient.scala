package edu.umass.cs.iesl.watr
package watrcolors
package client


import scala.async.Async.{async, await}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom._

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.mousetrap._
import native.fabric



import GeometricFigure._
import TraceLog._

@JSExport
class SvgOverview(
  artifactId: String
) extends ClientView with VisualTraceOperations { self =>
  import handlers._

  val server = ServerWire("svg")[SvgOverviewApi]

  override val initKeys = Keybindings(List(
    "b" -> ((e: MousetrapEvent) => getLabelOverlay()),
    "t" -> ((e: MousetrapEvent) => initSelection()),
    "z" -> ((e: MousetrapEvent) => selectViaLine()),
    "d" -> ((e: MousetrapEvent) => initDeletion())
  ))

  def canvasOffset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]

  def canvasBorder: Int = 10 // ???
  def canvasH: Int = fabricCanvas.getHeight
  def canvasW: Int = fabricCanvas.getWidth
  def canvasX: Int = canvasOffset.left.toInt
  def canvasY: Int = canvasOffset.top.toInt


  def getLabelOverlay(): Boolean = {
    async {
      // Set geometries for each page image, for coord scaling/translation:
      pageImageGeometries.clear()

      jQuery(".page-image").map({ (elem: Element) =>
        val igeom = LTBounds(
          elem.clientLeft.toDouble,
          elem.clientTop.toDouble,
          elem.clientWidth.toDouble,
          elem.clientHeight.toDouble
        )
        pageImageGeometries += igeom
      })


      server
        .getLabelOverlay(artifactId).call()
        .map(runTrace(_))

    }

    true
  }

  def alignBboxToDiv(divID: String, bbox: LTBounds): LTBounds = {
    val offset = jQuery(divID).offset().asInstanceOf[native.JQueryPosition]
    translateLTBounds(-offset.left, -offset.top, bbox)
  }

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
      bbox <- getUserLTBounds(self.fabricCanvas)
    } yield {
      val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
    }

    true
  }

  def initSelection(): Boolean = {
    for {
      // TODO alter cursor to reflect selection mode
      bbox <- getUserLTBounds(fabricCanvas)
    } yield {
      val bboxAbs = alignBboxToDiv("#overlay-container", bbox)

      async {
        val res = await { server.onSelectLTBounds(artifactId, bboxAbs).call() }
        applyHtmlUpdates(res)
        addLTBoundsRect(bboxAbs, "black", "#000")
      }
    }
    true
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

    jQuery(".page-image").map({ (elem: Element) =>

      def loaded(): Unit = {
        imgReady += 1
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
