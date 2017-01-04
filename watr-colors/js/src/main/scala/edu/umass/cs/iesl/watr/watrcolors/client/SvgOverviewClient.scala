package edu.umass.cs.iesl.watr
package watrcolors
package client


// import scala.async.Async.await
import scala.async.Async.async
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.scalajs.js.annotation.JSExport
// import org.scalajs.dom._

// import autowire._
// import boopickle.DefaultBasic._
// import Picklers._

// import native.mousetrap._
// import native.fabric


// import geometry._
// import GeometricFigure._

@JSExport
class SvgOverview(
  artifactId: String
) extends ClientView { self =>
  // import handlers._

  // val server = ServerWire("svg")[SvgOverviewApi]

  override val initKeys = Keybindings(List(
  //   // "b" -> ((e: MousetrapEvent) => getLabelOverlay()),
  //   // "t" -> ((e: MousetrapEvent) => initSelection()),
  //   // "w" -> ((e: MousetrapEvent) => getTextOverlay()),
  //   // "z" -> ((e: MousetrapEvent) => selectViaLine()),
  //   // "d" -> ((e: MousetrapEvent) => initDeletion())
  ))

  // def canvasOffset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
  def fabricCanvas = getFabric("fabric-canvas")

  // def canvasBorder: Int = 10 // ???
  // def canvasH: Int = fabricCanvas.getHeight
  // def canvasW: Int = fabricCanvas.getWidth
  // def canvasX: Int = canvasOffset.left.toInt
  // def canvasY: Int = canvasOffset.top.toInt

  // def getTextOverlay(): Boolean = {
  //   // Clear the canvas
  //   Mousetrap.bind("c", ((e: MousetrapEvent) => {
  //     fabricCanvas.forEachObject({(obj: native.fabric.FabricObject) =>
  //       fabricCanvas.remove(obj)
  //       fabricCanvas
  //     })
  //     true
  //   }))
  //   async {

  //     printlog(s"getting text overlay")
  //     server
  //       .getTextOverlay(artifactId).call()
  //       .foreach({ case (pageGeometries,  textOverlays) =>

  //         setupPageGeometries(pageGeometries)

  //         var totalLineNum = 0

  //         for {
  //           (page, pagenum) <- textOverlays.zipWithIndex
  //           (line, linenum) <- page.zipWithIndex
  //         } {
  //           val msg = line.content.getOrElse("")
  //           val target = line.targetRegion
  //           // printlog(s"target region: ${target} -> ?")
  //           val ttrans = transformTargetRegion(target)

  //           val cid = s"p${pagenum}l${linenum}"

  //           jQuery("#messages").append(
  //             s"""<li id="${cid}"><small>${totalLineNum}. <pre>${msg}</pre></small></li>"""
  //           )

  //           val hin = (e:JQueryEventObject) => { addShape(ttrans.bbox, "black", "yellow", 0.1f) }
  //           val hout = (e:JQueryEventObject) => {}
  //           jQuery(s"#${cid}").hover(hin, hout)
  //           totalLineNum += 1
  //         }

  //       })

  //   }
  //   true
  // }

  // def getLabelOverlay(): Boolean = {
  //   async {
  //     setupImageGeometries()

  //     printlog(s"getting overlay")
  //     server
  //       .getLabelOverlay(artifactId).call()
  //       .map(runTrace(_))
  //   }

  //   true
  // }

  // def alignBboxToDiv(divID: String, bbox: LTBounds): LTBounds = {
  //   val offset = jQuery(divID).offset().asInstanceOf[native.JQueryPosition]
  //   translateLTBounds(-offset.left, -offset.top, bbox)
  // }

  // def selectViaLine(): Boolean = {
  //   for {
  //     userPath <- getUserPath(self.fabricCanvas)
  //   } yield {
  //     val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
  //     val pathAbs = translatePath(-offset.left, -offset.top, userPath)

  //     server.onDrawPath(artifactId, pathAbs).call().foreach{ applyHtmlUpdates(_) }
  //   }

  //   true
  // }

  // def initDeletion(): Boolean = {
  //   for {
  //     bbox <- getUserLTBounds(self.fabricCanvas)
  //   } yield {
  //     val offset = jQuery("#overlay-container").offset().asInstanceOf[native.JQueryPosition]
  //   }

  //   true
  // }

  // def initSelection(): Boolean = {
  //   for {
  //     // TODO alter cursor to reflect selection mode
  //     bbox <- getUserLTBounds(fabricCanvas)
  //   } yield {
  //     val bboxAbs = alignBboxToDiv("#overlay-container", bbox)

  //     async {
  //       val res = await { server.onSelectLTBounds(artifactId, bboxAbs).call() }
  //       applyHtmlUpdates(res)
  //       addLTBoundsRect(bboxAbs, "black", "#000", 0.1f)
  //     }
  //   }
  //   true
  // }

  def createView(): Unit = async {

    // val res = await {
    //   server.createView(artifactId).call()
    // }
    // applyHtmlUpdates(res)

    // // val jqOverlayContainer = jQuery("#overlay-container")

    // val c = new fabric.Canvas("fabric-canvas", fabric.CanvasOptions)


    // jQuery("#fabric-canvas").prop("fabric", c)
    // c.uniScaleTransform = true

    // var imgCount = jQuery(".page-image").length
    // var imgReady = 0

    // jQuery(".page-image").map({ (elem: Element) =>

    //   def loaded(): Unit = {
    //     imgReady += 1
    //     if (imgReady == imgCount) {
    //       val h = jQuery("#img-container").height()
    //       fabricCanvas.setHeight(h.toInt+1)
    //       val w = jQuery("#img-container").width()
    //       fabricCanvas.setWidth(w.toInt+1)

    //       println(s"fabric canvas loaded h/w = ${h}/${w}")

    //     }
    //   }

    //   elem.addEventListener("load", {(e: Event) => loaded() })
    //     elem.addEventListener("error", (e: Event) => {
    //       println("image load encountered an error")
    //     })

    // })

  }
}
