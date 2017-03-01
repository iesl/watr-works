package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.async.Async
import scala.concurrent.Future

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.ext._

import autowire._
import upickle.{default => UPickle}
import UPickle._

import geometry._
import labeling._

import native.mousetrap._

import watrmarks.{StandardLabels => LB}
import watrmarks.Label

@JSExport
object WatrColors extends LabelerRendering {

  var selectType: Label = LB.Title

  val keybindings: List[(String, (MousetrapEvent) => Unit)] = List(
    "l a" -> ((e: MousetrapEvent) => {selectType = LB.Authors}),
    "l t" -> ((e: MousetrapEvent) => {selectType = LB.Title}),
    "l f" -> ((e: MousetrapEvent) => {selectType = LB.Affiliation}),
    "l s" -> ((e: MousetrapEvent) => {selectType = LB.Abstract}),
    "s" -> ((e: MousetrapEvent) => doSelection())
  )

  def initKeybindings() = {
    Mousetrap.reset()
    keybindings.foreach { case (str, fn) =>
      val bindFunc: MousetrapEvent => Boolean = e => {
        fn(e); true
      }

      Mousetrap.bind(str, bindFunc, "keypress")
    }
  }

  override def handleClick(canvasPoint: Point): Unit = {

  }

  @JSExport
  def doSelection(): Unit = {

    fabricCanvas.defaultCursor = "crosshair"
    // fabricCanvas.skipTargetFind = true
    // fabricCanvas.isDrawingMode = true
    fabricCanvas.renderAll()

    for {
      bboxRel <- getUserLTBounds(fabricCanvas)
    } yield {
      val bbox = alignBboxToDiv("#canvas-container", bboxRel)

      fabricCanvas.defaultCursor = "default"

      shell.onSelectLTBounds(selectType.fqn, bbox)
        .foreach({ bboxes =>
          bboxes.foreach{ bbox =>
            // println(s"got reponse: ${bbox}")
            addShape(bbox, "black", "", 1f)
          }
        })
    }

  }


  object shell {
    val Client = new WebsideClient("autowire")
    val api = Client[WatrShellApi]


    def onClick(p: Point): Future[List[LTBounds]] = {
      api.onClick(p).call()
    }

    def onSelectLTBounds(artifactId: String, bbox: LTBounds): Future[List[LTBounds]] = {
      api.onSelectLTBounds(artifactId, bbox).call()
    }
  }

  @JSExport
  var interval: Double = 1000

  object WatrColorsApiListeners extends WatrColorsApi {

    @JSExport
    def helloColors(msg: String): Unit = {
      println(msg)
    }

    @JSExport
    override def clear(): Unit = {
      fabricCanvas.clear()
    }

    @JSExport
    override def print(level: String, msg: String): Unit = {
      jQuery("#main").append(msg+" and more!")

      level match {
        case "error" => dom.console.error(msg)
        case "warn" => dom.console.warn(msg)
        case "info" => dom.console.info(msg)
        case "log" => dom.console.log(msg)
        case      _ => dom.console.log(level + ":" + msg)
      }
    }


    @JSExport
    override def echoLabeler(lwidget: List[AbsPosAttr]) = Async.async {
      fabricCanvas.renderOnAddRemove = false
      clear()
      val (bbox, fobjs) = renderLabelWidget(lwidget)
      fabricCanvas.setWidth(bbox.width.toInt)
      fabricCanvas.setHeight(bbox.height.toInt)


      fobjs.foreach{os =>
        os.foreach(fabricCanvas.add(_))
      }

      fabricCanvas.renderAll()
      fabricCanvas.renderOnAddRemove = true
    }


  }

  @JSExport
  def main(host: String="localhost", port: Int=9999): Unit = {

    initKeybindings()

    val websideServer = new WebsideServer(WatrColorsApiListeners)

    var success = false

    def rec(): Unit = {

      Ajax.post(s"http://$host:$port/notifications").onComplete {

        case util.Success(data) =>
          if (!success) println("WatrTable connected via POST /notifications")
          success = true
          interval = 1000


          websideServer.wire(data.responseText)
          rec()
        case util.Failure(e) =>
          if (success) println("Workbench disconnected " + e)
          success = false
          interval = math.min(interval * 2, 30000)
          dom.window.setTimeout(() => rec(), interval)
      }
    }

    dom.window.addEventListener("load", (event: dom.Event) => {
      // defaultMouseHandler()
      rec()
    })
  }





}
