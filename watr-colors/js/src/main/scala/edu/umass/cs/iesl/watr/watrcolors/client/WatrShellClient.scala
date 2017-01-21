package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.async.Async
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.dom.ext._

import textreflow._
import geometry._
import display._

// import autowire._

import upickle.{default => UPickle}
import UPickle._
import TypeTagPicklers._

import native.mousetrap._

object ServerSite extends autowire.Server[String, UPickle.Reader, UPickle.Writer] {
  def routes = ServerSite.route[WatrTableApi](WatrTableClient)

  def wire(incoming: String): Unit = {

    val remoteCalls = UPickle.read[List[RemoteCall]](incoming)

    remoteCalls.foreach { case RemoteCall(callPath, callArgs) =>
      println(s"Server: Remote Call recv'd ${callPath}")

      val req = new Request(
        callPath,
        callArgs.toMap
      )

      routes.apply(req)
    }
  }

  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
}

@JSExport
object WatrTableClient extends ClientView with WatrTableApi with TextReflowExamples {

  @JSExport
  def doSelection(): Boolean = {
    println("getUserLTBounds")

    for {
      bbox <- handlers.getUserLTBounds(fabricCanvas)
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
    true
  }

  @JSExport
  def hello(): Boolean = {
    println(s"Hello!!!")
    true
  }

  override val initKeys = Keybindings(List(
    "s" -> ((e: MousetrapEvent) => doSelection()),
    "r" -> ((e: MousetrapEvent) => hello())
  ))

  def createView(): Unit = {}

  @JSExport
  lazy val shadowBody = dom.document.body.cloneNode(deep = true)

  @JSExport
  var interval: Double = 1000

  @JSExport
  var success = false

  @JSExport
  def main(host: String="localhost", port: Int=9999): Unit = {
    setKeybindings(initKeys)

    def rec(): Unit = {

      Ajax.post(s"http://$host:$port/notifications").onComplete {

        case util.Success(data) =>
          if (!success) println("WatrTable connected via POST /notifications")
          success = true
          interval = 1000


          ServerSite.wire(data.responseText)
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
