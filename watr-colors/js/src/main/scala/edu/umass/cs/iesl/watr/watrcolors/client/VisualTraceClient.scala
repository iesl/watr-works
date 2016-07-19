package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import native.fabric
import native.mousetrap._


import scala.async.Async.{async, await}


@JSExport
class VisualTraceClient(
  artifactId: String
) extends ClientView { self =>

  val server = ServerWire("vtrace")[VisualTraceApi]

  def createView(): Unit = async {
    val res = await {
      server.createView().call()
    }
    applyHtmlUpdates(res)
  }


  override val initKeys = Keybindings(List(
    "t" -> ((e: MousetrapEvent) => runTrace())
    // "b" -> ((e: MousetrapEvent) => createDocumentOverlay()),
  ))

  def runTrace(): Boolean = {
    import VisualTrace._

    println("running trace, hold on...")

    val canvasBorder = 10

    var canvasX: Int = 0
    var canvasY: Int = 0
    var canvasW: Int = canvasBorder*2
    var canvasH: Int = canvasBorder*2

    var canvasXScale = 4
    var canvasYScale = 4

    def offsetX(v: Double): Double = {
      v - canvasX + canvasBorder
    }

    def offsetY(v: Double): Double = {
      v - canvasY + canvasBorder
    }
    def scaleY(v: Double): Double = {
      (canvasYScale * v)
    }
    def scaleX(v: Double): Double = {
      (canvasXScale * v)
    }

    def transformX(v: Double) = offsetX(scaleX(v))
    def transformY(v: Double) = offsetY(scaleY(v))

    def translate(shape: Overlay): Overlay = {
      shape match {
        case s: BBox =>
          BBox(
            transformX(s.x), transformY(s.y),
            scaleX(s.width), scaleY(s.height)
          )
        case s: Point =>
          Point(
            transformX(s.x), transformY(s.y)
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


    async{
      val traceEntries = await {
        server.runTrace().call()
      }


      traceEntries.foreach({ _ match {
        case Noop =>
        case SetViewport(b: BBox) =>
          // put a border around the canvas
          // put message area below canvas
          // set total width/height
          canvasX = scaleX(b.x).toInt
          canvasY = scaleY(b.y).toInt
          canvasW = scaleX(b.width).toInt
          canvasH = scaleY(b.height).toInt

          fabricCanvas.setWidth(canvasW+canvasBorder*2+1)
          fabricCanvas.setHeight(canvasH+canvasBorder*2+1)

          addShape(BBox(
            canvasX, canvasY, canvasW, canvasH
          ), "black")
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

          val r = fabric.Rect()
          r.left = 0
          r.top =  transformY(s)
          r.width = canvasW+canvasBorder
          r.height = 1
          r.stroke      = "red"
          r.strokeWidth = 1
          r.fill        = "rgb(100, 30, 52)"
          r.opacity = 0.2

          // setGradient(r,
          //   "fill",
          //   "linear",
          //   (0, 0),
          //   (50, 15),
          //   js.Dynamic.literal(
          //     "0" -> "#222",
          //     "1" -> "#888"
          //   )
          // )

          fabricCanvas.add(r)

        case Message(s: String) =>
          println(s"Message: ${s}")
          // case And(t1, t2) =>
          // case AndThen(t1, t2) =>
      }})

    }
    true
  }
}
