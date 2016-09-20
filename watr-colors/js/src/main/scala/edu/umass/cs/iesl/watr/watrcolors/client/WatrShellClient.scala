package edu.umass.cs.iesl.watr
package watrcolors
package client


// import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scala.scalajs.js.annotation.JSExport

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.mousetrap._

import scala.concurrent.Future
import scala.async.Async.{async, await}
import org.scalajs.dom
import org.scalajs.dom.ext._
import scala.scalajs.js

import java.nio.ByteBuffer

object Wire extends autowire.Server[ByteBuffer, Pickler, Pickler]  with RemoteCallPicklers {


  def wire(incoming: Array[Byte]): Unit = {

    val inbytes = ByteBuffer.wrap(incoming)

    println(s"trying to unpack... ")
    val unpacked = read[
      List[RemoteCall]
    ](inbytes)

    println(s"unpacked... ")
    unpacked.foreach { case RemoteCall(callPath, callArgs) =>
      val req = new Request(callPath, callArgs.map(b => (b._1, ByteBuffer.wrap(b._2))).toMap)
      Wire.route[WatrShellApi](WatrShellClient).apply(req)
    }
  }

  // def wire(parsed: (List[String], Map[String, ByteBuffer])): Unit = {
  def wire0(parsed: ByteBuffer): Unit = {

    // read[(List[String], Seq[(String, ByteBuffer)])
    //   val Js.Arr(path, args: Js.Obj) = parsed
    // val (path, args) = parsed
    // val req = new Request(upickle.readJs[Seq[String]](path), args.value.toMap)
    // val req = new Request(read[Seq[String]](path), args.value.toMap)
    //   Wire.route[Api](WatrShellClient).apply(req)
  }

  override def read[R: Pickler](p: ByteBuffer) = Unpickle[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)
}

// object Wire extends autowire.Server[Js.Value, upickle.Reader, upickle.Writer] with ReadWrite{
//   def wire(parsed: Js.Arr): Unit = {
//     val Js.Arr(path, args: Js.Obj) = parsed
//     val req = new Request(upickle.readJs[Seq[String]](path), args.value.toMap)
//     Wire.route[Api](WorkbenchClient).apply(req)
//   }
// }

@JSExport
object WatrShellClient extends ClientView with WatrShellApi {


  override val initKeys = Keybindings(List(
  ))

  def createView(): Unit = {
    // server.startShell().call().foreach { _ =>
    //   println("server shell started")
    // }
  }
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
          if (!success) println("WatrShell connected via POST /notifications")
          success = true
          interval = 1000


          Wire.wire(data.responseText.getBytes)
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
    dom.document.asInstanceOf[js.Dynamic].body = shadowBody.cloneNode(true)
    for(i <- 0 until 100000){
      dom.window.clearTimeout(i)
      dom.window.clearInterval(i)
    }
  }

  @JSExport
  override def print(level: String, msg: String): Unit = {
    jQuery("#main").append(msg)

    level match {
      case "error" => dom.console.error(msg)
      case "warn" => dom.console.warn(msg)
      case "info" => dom.console.info(msg)
      case "log" => dom.console.log(msg)
    }
  }

  // @JSExport
  // override def reload(): Unit = {
  //   dom.console.log("Reloading page...")
  //   dom.location.reload()
  // }
  // @JSExport
  // override def run(path: String, bootSnippet: Option[String]): Unit = {
  //   val tag = dom.document.createElement("script").asInstanceOf[HTMLElement]
  //   var loaded = false

  //   tag.setAttribute("src", path)
  //   bootSnippet.foreach{ bootSnippet =>
  //     tag.onreadystatechange = (e: dom.Event) => {
  //       if (!loaded) {
  //         dom.console.log("Workbench reboot")
  //         js.eval(bootSnippet)
  //       }
  //       loaded = true
  //     }
  //     tag.asInstanceOf[js.Dynamic].onload = tag.onreadystatechange
  //   }
  //   dom.document.head.appendChild(tag)
  // }

}
