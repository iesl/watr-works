package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
// import scala.scalajs.{js => sjs}

// import org.scalajs.dom
import org.scalajs.jquery.jQuery

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.fabric
import native.mousetrap._

// import rx._

// import scalatags.JsDom.all._




@JSExport
class VisualTraceClient(
  artifactId: String
) extends ClientView { self =>

  val server = ServerWire("vtrace")[VisualTraceApi]

  def createView(): Unit = {
    server.createView().call().foreach(applyHtmlUpdates(_))
  }


  override val initKeys = Keybindings(List(
    "t" -> ((e: MousetrapEvent) => runTrace())
    // "b" -> ((e: MousetrapEvent) => createDocumentOverlay()),
  ))

  def runTrace(): Boolean = {
    import VisualTrace._

    println("running trace, hold on...")

    server.runTrace().call().foreach({ traceEntries =>

      traceEntries.foreach({ _ match {
        case Noop =>
        case SetViewport(b: BBox) =>
          fabricCanvas.setWidth(b.width.toInt)
        case GetViewport() =>
          println("set!")
        case Show(s: Overlay) =>
          addShape(s, "blue")

        case ShowVDiff(d1: Double, d2: Double) =>
        case FocusOn(s: Overlay) =>
        case HRuler(s: Double) =>
          println(s"rule! ${s}")

        case Message(s: String) =>
          println(s"Message: ${s}")
          // case And(t1, t2) =>
          // case AndThen(t1, t2) =>
      }})

    })

    true
  }
}
