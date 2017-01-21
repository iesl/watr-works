package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.async.Async

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
// import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.ext._

import autowire._

import textreflow._
import geometry._
import display._

import native.mousetrap._


@JSExport
object WatrColors extends WatrColorsApi with WatrShellApi with TextReflowExamples {

  val keybindings: List[(String, (MousetrapEvent) => Unit)] = List(
    "s" -> ((e: MousetrapEvent) => doSelection()),
    "r" -> ((e: MousetrapEvent) => helloShell("Hello from WatrColors!"))
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

  val WebClient = new WebsideClient("autowire")
  val shellCall = WebClient[WatrShellApi]

  @JSExport
  def doSelection(): Unit = {
    println("getUserLTBounds")

    for {
      bbox <- MouseGestures.getUserLTBounds(fabricCanvas)
    } yield {
      println(s"getUserLTBounds: got ${bbox}")
      addLTBoundsRect(bbox, "black", "#000", 0.3f)

      // val bboxAbs = alignBboxToDiv("#overlay-container", bbox)
      // async {
      //   val res = await { server.onSelectLTBounds(artifactId, bboxAbs).call() }
      //   applyHtmlUpdates(res)
      //   addLTBoundsRect(bboxAbs, "black", "#000", 0.1f)
      // }
    }
  }

  @JSExport
  def helloColors(msg: String): Unit = {
    println(msg)
  }

  @JSExport
  def onSelectLTBounds(artifactId: String, bbox: LTBounds): Unit = {

  }

  @JSExport
  def onDrawPath(artifactId: String, path: Seq[Point]): Unit = {

  }

  @JSExport
  def helloShell(msg: String): Unit = {
    shellCall.helloShell(msg).call()
  }


  @JSExport
  lazy val shadowBody = dom.document.body.cloneNode(deep = true)

  @JSExport
  var interval: Double = 1000

  @JSExport
  var success = false


  val websideServer = new WebsideServer(this)

  @JSExport
  def main(host: String="localhost", port: Int=9999): Unit = {

    initKeybindings()

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

    fabricCanvas.add(labeler)
    fabricCanvas.renderAll()
  }


  @JSExport
  override def echoTextReflows(textReflows: List[TextReflow]): Unit = {
    vcatWidgets(textReflows)
  }



}
