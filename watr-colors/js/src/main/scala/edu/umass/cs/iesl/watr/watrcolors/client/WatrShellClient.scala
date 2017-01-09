package edu.umass.cs.iesl.watr
package watrcolors
package client

import java.nio.ByteOrder
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport


import org.scalajs.dom
import org.scalajs.dom.ext._

import java.nio.ByteBuffer
import textreflow._
import geometry._


import boopickle.DefaultBasic._

object ServerSite extends autowire.Server[ByteBuffer, Pickler, Pickler] with TextReflowBoopicklers {

  def wire(incoming: Array[Byte]): Unit = {
    val inbytes = ByteBuffer
      .wrap(incoming)
      .order(ByteOrder.LITTLE_ENDIAN)

    val unpacked = read[List[RemoteCall]](inbytes)

    println(s"  Server byte order: ${inbytes.order}")
    unpacked.foreach { case RemoteCall(callPath, callArgs) =>
      println(s"Server: Remote Call recv'd ${callPath}")
      // callArgs.foreach({case (param, value) =>
      //   println(s"${param}:")
      //   val iarr = value.array.map(_.toInt).mkString(", ")
      //   println(s"  [$iarr]")
      // })
      val req = new Request(
        callPath,
        callArgs
          .map(b => (
            b._1,
            ByteBuffer.wrap(b._2).order(ByteOrder.LITTLE_ENDIAN)
          )
        ).toMap
      )
      println("Server: req unpacked")
      ServerSite.route[WatrTableApi](WatrTableClient).apply(req)
      println("Server: req routed")
    }
  }

  override def read[R: Pickler](p: ByteBuffer) = Unpickle[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)
}

@JSExport
object WatrTableClient extends ClientView with WatrTableApi with TextReflowExamples {

  override val initKeys = Keybindings(List(
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
    def rec(): Unit = {

      Ajax.post(s"http://$host:$port/notifications").onComplete {

        case util.Success(data) =>
          if (!success) println("WatrTable connected via POST /notifications")
          success = true
          interval = 1000


          ServerSite.wire(data.responseText.getBytes)
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
  def echoTargetRegion(tr: TargetRegion): Unit = {
    println(s"got TargetRegion ${tr}")
  }

  @JSExport
  def echoDouble(d: Double): Unit = {
    println(s"got Double ${d}")
  }

  @JSExport
  def echoLTBounds(bbox: LTBounds): Unit = {
    println(s"got LTBounds ${bbox}")
  }

  @JSExport
  def echoCharAtom(charAtom: CharAtom): Unit = {
    println(s"got CharAtom ${charAtom}")
  }

  @JSExport
  override def echo(textReflow: TextReflow): Unit = {
    println(s"got into  Echo w/${textReflow}")
    // textReflow

    vcatWidgets(Seq(
      textReflow,
      textReflow
    ))

  }

  @JSExport
  override def echo2(textReflows: List[TextReflow]): Unit = {
    // println(s"got into  Echo w/${textReflow}")
    println("got into  Echo2")
    vcatWidgets(textReflows)
  }



}
