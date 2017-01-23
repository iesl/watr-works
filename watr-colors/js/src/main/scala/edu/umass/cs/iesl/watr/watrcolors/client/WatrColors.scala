package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.async.Async

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.ext._

import autowire._

import textreflow._
import geometry._
import display._

import native.mousetrap._


@JSExport
object WatrColors extends TextReflowExamples {

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

    for {
      bbox <- MouseGestures.getUserLTBounds(fabricCanvas)
    } yield {
      println(s"getUserLTBounds: got ${bbox}")
      addLTBoundsRect(bbox, "black", "#000", 0.3f)

      shell.onSelectLTBounds("some-artifact", bbox)

      // val bboxAbs = alignBboxToDiv("#overlay-container", bbox)
      // async {
      //   val res = await { server.onSelectLTBounds(artifactId, bboxAbs).call() }
      //   applyHtmlUpdates(res)
      //   addLTBoundsRect(bboxAbs, "black", "#000", 0.1f)
      // }
    }
  }


  object shell {
    val Client = new WebsideClient("autowire")
    val api = Client[WatrShellApi]

    @JSExport
    def hello(msg: String): Unit = {
      api.helloShell(msg).call()
    }

    def onSelectLTBounds(artifactId: String, bbox: LTBounds): Unit = {
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
    def echoLabeler(lwidget: LabelWidget) = Async.async {
      clear()
      val labeler = Async.await {
        renderLabelWidget(lwidget)
      }

      val lw = labeler.width
      fabricCanvas.setWidth(lw.intValue)

      val lh = labeler.height
      fabricCanvas.setHeight(lh.intValue)

      // fabricCanvas.setBackgroundColor("yellow")
      fabricCanvas.add(labeler)
      fabricCanvas.renderAll()
    }


    @JSExport
    override def echoTextReflows(textReflows: List[TextReflow]): Unit = {
      vcatWidgets(textReflows)
    }
  }

  @JSExport
  def main(host: String="localhost", port: Int=9999): Unit = {

    initKeybindings()

    val websideServer = new WebsideServer(WatrColorsApiListeners)
      // @JSExport

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
