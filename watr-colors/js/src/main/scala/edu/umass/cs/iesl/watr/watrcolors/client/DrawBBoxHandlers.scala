package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.jquery.jQuery

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.fabric

import scalatags.JsDom.all._


trait EventHandler {
  def fabricCanvas: fabric.Canvas
  def server: autowire.ClientProxy[SvgOverviewApi, java.nio.ByteBuffer, Pickler, Pickler]
  def addBBoxRect(bbox: BBox, color: String): Unit

  def clientView: ClientView
  def handerFunctions: Seq[(String, (fabric.Options)=>Unit)]


  def installHandlers() = handerFunctions.map{hf =>
    fabricCanvas.on(hf._1, hf._2)
  }

  def removeHandlers() = handerFunctions.map{hf =>
    fabricCanvas.off(hf._1, hf._2)
  }
}

trait DeleteBBoxHandlers extends EventHandler {

  // cb(context, object, index, objects)
  fabricCanvas.forEachObject({(o:fabric.FabricObject, i: Int) =>
    println(s"object #$i")
    o.hasControls = true
    o.hasBorders = true
    o.selectable = true
  })


  val handerFunctions = Seq(
    // ("mouse:down", onDown),
  )

}

import scala.concurrent.Future
import scala.async.Async.{async, await}

object handlers {

  def getUserBBox(c: fabric.Canvas): Future[BBox] = {

    val chan = CanvasMouseChannels(c)

    async {
      var res = await(chan.mousedown())
      val px1: Int = res.e.pageX
      val py1: Int = res.e.pageY

      res = await(chan.mousemove | chan.mouseup)
      while(res.e.`type` == "mousemove"){
        res = await(chan.mousemove | chan.mouseup)
      }

      val px2 = res.e.pageX
      val py2 = res.e.pageY
      val x = math.min(px1, px2)
      val y = math.min(py1, py2)
      val w = math.abs(px2 - px1)
      val h = math.abs(py2 - py1)

      BBox(x.toDouble, y.toDouble, w.toDouble, h.toDouble, "")
    }
  }

  def translateBBox(x0: Double, y0: Double, bb: BBox): BBox = {
    bb.copy(
      x = bb.x + x0,
      y = bb.y + y0
    )
  }

}



trait DrawBBoxHandlers extends EventHandler {
  def artifactId: String

  var p1: (Int, Int) = _

  val onDown = {(opts: fabric.Options) =>
    p1 = (opts.e.pageX, opts.e.pageY)
  }
  val onUp = {(opts: fabric.Options) =>
    val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]

    val x1 = p1._1 - offset.left
    val y1 = p1._2 - offset.top
    val x2 = opts.e.pageX - offset.left
    val y2 = opts.e.pageY - offset.top

    val bbox = BBox(
      x = x1,
      y = y1,
      width = x2 - x1,
      height = y2 - y1,
      ""
    )

    server.onSelectBBox(artifactId, bbox).call().foreach{ clientView.applyHtmlUpdates(_) }

    removeHandlers()
    addBBoxRect(bbox, "black")
  }

  val handerFunctions = Seq(
    ("mouse:down", onDown),
    ("mouse:up", onUp)
  )

}
