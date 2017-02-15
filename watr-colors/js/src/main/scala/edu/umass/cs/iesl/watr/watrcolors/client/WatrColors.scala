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

import textreflow.data._
import geometry._
import labeling._

import native.mousetrap._

@JSExport
object WatrColors extends LabelerRendering {

  val keybindings: List[(String, (MousetrapEvent) => Unit)] = List(
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


  @JSExport
  def doSelection(): Unit = {
    println("getUserLTBounds")

    fabricCanvas.defaultCursor = "crosshair"
    // fabricCanvas.skipTargetFind = true
    // fabricCanvas.isDrawingMode = true
    fabricCanvas.renderAll()

    for {
      bboxRel <- getUserLTBounds(fabricCanvas)
    } yield {
      val bbox = alignBboxToDiv("#overlay-container", bboxRel)

      fabricCanvas.defaultCursor = "default"

      shell.onSelectLTBounds("some-artifact", bbox)
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

    @JSExport
    def hello(msg: String): Unit = {
      api.helloShell(msg).call()
    }

    def onSelectLTBounds(artifactId: String, bbox: LTBounds): Future[List[LTBounds]] = {
      api.onSelectLTBounds(artifactId, bbox).call()
    }
  }



  @JSExport
  lazy val shadowBody = dom.document.body.cloneNode(deep = true)

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
      // dom.document.asInstanceOf[js.Dynamic].body = shadowBody.cloneNode(true)
      // for(i <- 0 until 100000){
      //   dom.window.clearTimeout(i)
      //   dom.window.clearInterval(i)
      // }
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
      clear()
      val (bbox, fobjs) = renderLabelWidget(lwidget)
      fabricCanvas.setWidth(bbox.width.toInt)
      fabricCanvas.setHeight(bbox.height.toInt)

      fobjs.foreach{os =>
        os.foreach(fabricCanvas.add(_))
      }

      fabricCanvas.renderAll()
    }


    @JSExport
    override def echoTextReflows(textReflows: List[TextReflow]): Unit = {
      // vcatWidgets(textReflows)
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

    // Trigger shadowBody to get captured when the page first loads
    dom.window.addEventListener("load", (event: dom.Event) => {
      dom.console.log("Loading Workbench")
      shadowBody
      rec()
    })
  }





}
