package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
// import scala.scalajs.{js => sjs}

// import org.scalajs.dom
// import org.scalajs.jquery.jQuery

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

    var canvasX: Int = 0
    var canvasY: Int = 0
    var canvasW: Int = 0
    var canvasH: Int = 0

    var canvasXScale = 4
    var canvasYScale = 4


    def scaleY(v: Double): Int = {
      (canvasYScale * v).toInt
    }
    def scaleX(v: Double): Int = {
      (canvasXScale * v).toInt
    }

    def translate(shape: Overlay): Overlay = {
      shape match {
        case s: BBox =>
          BBox(
            scaleX(s.x)-canvasX, scaleY(s.y)-canvasY,
            scaleX(s.width), scaleY(s.height)
          )
        case s: Point =>
          Point(
            scaleX(s.x)-canvasX, scaleY(s.y)-canvasY
          )
        case s: Line =>
          Line(
            translate(s.p1).asInstanceOf[Point],
            translate(s.p2).asInstanceOf[Point]
          )
      }

    }

    import scala.scalajs.js

    def setGradient(
      obj: fabric.FabricObject,
      strokeOrFill: String, // stroke|fill
      gtype: String, // radial|linear
      start: (Int, Int),
      end: (Int, Int),
      colorStops: js.Object,
      radii: Option[(Int, Int)] = None
        // gradientTransform: js.Object = js.Dynamic.literal()
    ): fabric.FabricObject = {

      val rd1 = radii.map(_._1).getOrElse(0)
      val rd2 = radii.map(_._2).getOrElse(0)

      obj.setGradient(strokeOrFill,
        js.Dynamic.literal(
          "type"       -> gtype,
          "x1"         -> start._1,
          "x2"         -> start._2,
          "y1"         -> end._1,
          "y2"         -> end._2,
          "r1"         -> rd1,
          "r2"         -> rd2,
          "colorStops" -> colorStops,
          "gradientTransform" -> js.Array[Int](1, 0, 0, 1, 0, 0)
        ))
    }


    server.runTrace().call().foreach({ traceEntries =>

      traceEntries.foreach({ _ match {
        case Noop =>
        case SetViewport(b: BBox) =>
          // put a border around the canvas
          // put message area below canvas
          // set total width/height
          canvasX = scaleX(b.x)
          canvasY = scaleY(b.y)
          canvasW = scaleX(b.width)
          canvasH = scaleY(b.height)

          fabricCanvas.setWidth(canvasW)
          fabricCanvas.setHeight(canvasH)

          addShape(translate(b), "black")
          // fabricCanvas.setBackgroundColor("green", () => {})

          println("b = " + translate(b))

        case GetViewport() =>
          println("set!")
        case Show(s: Overlay) =>
          addShape(translate(s), "blue")

        case ShowVDiff(d1: Double, d2: Double) =>
        case FocusOn(s: Overlay) =>
        case HRuler(s: Double) =>
          println(s"rule! ${s} scaled: ${scaleY(s)}, inplace = ${scaleY(s) - canvasY}}")

          import scala.scalajs.js
          val r = fabric.Rect()
          r.left = 10
          r.top =  scaleY(s) - canvasY
          r.width = canvasW
          r.height = 2
          r.stroke      = "red"
          r.strokeWidth = 1
          r.fill        = "rgb(100, 30, 52)"
          r.opacity = 0.2

          setGradient(r,
            "fill",
            "linear",
            (0, 0),
            (50, 15),
            js.Dynamic.literal(
              "0" -> "#222",
              "1" -> "#888"
            )
          )

          fabricCanvas.add(r)

        case Message(s: String) =>
          println(s"Message: ${s}")
          // case And(t1, t2) =>
          // case AndThen(t1, t2) =>
      }})

    })

    true
  }
}
