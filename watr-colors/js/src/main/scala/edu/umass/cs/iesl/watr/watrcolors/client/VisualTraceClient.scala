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

  def fabricCanvas: fabric.Canvas = {
    jQuery("#fabric-canvas").prop("fabric").asInstanceOf[fabric.Canvas]
  }

  override val initKeys = Keybindings(List(
    "t" -> ((e: MousetrapEvent) => runTrace())
    // "b" -> ((e: MousetrapEvent) => createDocumentOverlay()),
  ))

  def runTrace(): Boolean = {
    import VisualTrace._

    server.runTrace().call().foreach({ traceEntries =>

      traceEntries.foreach({ _ match {
        case Noop =>
        case SetViewport(b: BBox) =>
        case GetViewport() =>
          println("set!")
        case Show(s: Overlay) =>
          println("show!")

        case ShowVDiff(d1: Double, d2: Double) =>
        case FocusOn(s: Overlay) =>
        case HRuler(s: Double) =>
          println("rule!")

        case Message(s: String) =>
          // case And(t1, t2) =>
          // case AndThen(t1, t2) =>
      }})

    })

    true
  }
}
