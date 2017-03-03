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

import TypeTagPicklers._

@JSExport
object WatrColors extends LabelerRendering {

  var uiState: Option[UIState] = None // UIState()

  val keybindings: List[(String, (MousetrapEvent) => Unit)] = List(
    // "l a" -> ((e: MousetrapEvent) => {alterSelectionType(LB.Authors)}),
    // "l t" -> ((e: MousetrapEvent) => {alterSelectionType(LB.Title)}),
    // "l f" -> ((e: MousetrapEvent) => {alterSelectionType(LB.Affiliation)}),
    // "l s" -> ((e: MousetrapEvent) => {alterSelectionType(LB.Abstract)}),
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

  // override def handleClick(canvasPoint: Point): Unit = {}


  @JSExport
  def doSelection(): Unit = {
    uiState.map{ state =>
      fabricCanvas.defaultCursor = "crosshair"
      // fabricCanvas.skipTargetFind = true
      // fabricCanvas.isDrawingMode = true
      fabricCanvas.renderAll()

      for {
        bboxRel <- getUserLTBounds(fabricCanvas)
      } yield {
        val bbox = alignBboxToDiv("#canvas-container", bboxRel)

        fabricCanvas.defaultCursor = "default"
        shell
          .uiRequest(UIRequest(state, SelectRegion(bbox)))
          .foreach{ uiResponse =>
            uiResponse.changes.foreach{
              // case (action, bbox) =>
              case bbox =>
                addShape(bbox, "black", "", 1f)
            }
          }
      }
    }

  }

  object shell {
    val Client = new WebsideClient("autowire")
    val api = Client[WatrShellApi]

    def uiRequest(r: UIRequest): Future[UIResponse] = {
      api.uiRequest(r).call()
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
    override def echoLabeler(lwidget: List[AbsPosAttr], labelOptions: LabelOptions): Unit = Async.async {
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
      val controls = createLabelerControls(labelOptions)
      val c = controls.render
      val statusBar = dom.document.getElementById("status-bar")
      statusBar.childNodes.foreach(
        statusBar.removeChild(_)
      )
      statusBar.appendChild(c)
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
