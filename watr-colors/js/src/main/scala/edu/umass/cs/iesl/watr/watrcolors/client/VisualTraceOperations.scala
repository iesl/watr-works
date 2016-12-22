package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.collection.mutable
// import org.querki.jquery._
import org.scalajs.dom._
import textboxing.{TextBoxing => TB}

trait VisualTraceOperations extends FabricCanvasOperations {

  import geometry._
  import GeometricFigure._
  import TraceLog._
  import TypeTags._

  val pageGeometry = mutable.Map[Int, PageGeometry]()

  val pageOffsets = mutable.MutableList[LTBounds]()

  val pageImageGeometries = mutable.MutableList[LTBounds]()


  def totalPagesHeight: Double = pageOffsets.lastOption
    .map{ p => p.top + p.height }
    .getOrElse { 0d }

  def canvasBorder: Int
  def canvasX: Int
  def canvasY: Int
  def canvasW: Int
  def canvasH: Int

  var xxx = 10
  def transformTargetRegion(tr: TargetRegion): TargetRegion = {
    val offsetPage = pageOffsets(tr.target.unwrap)
    val pageImgGeometry = pageImageGeometries(tr.target.unwrap)

    val s = tr.bbox

    val pageTopTrans = (offsetPage.top * canvasH / totalPagesHeight) + canvasY
    val ltrans = (s.left * pageImgGeometry.width / offsetPage.width)
    val ttrans = (s.top * pageImgGeometry.height / offsetPage.height) + pageTopTrans
    val wtrans = (s.width * pageImgGeometry.width / offsetPage.width)
    val htrans = (s.height * pageImgGeometry.height / offsetPage.height)

    val bounds = LTBounds(ltrans, ttrans, wtrans, htrans)
    if (xxx > 0) {
      xxx -= 1
      println(s""" transformTargetRegion: ${s} -> ${bounds}  """)
      println(s"""     offsetPage: ${offsetPage} """)
      println(s"""     pageTopTrans: ${pageTopTrans} """)
      println(s"""     canvas x,y,w,h: $canvasX, $canvasY, $canvasW, $canvasH """)
    }

    tr.copy(bbox = bounds)
  }

  def transformTargetFigure(tr: TargetFigure): TargetFigure = {
    val offsetPage = pageOffsets(tr.page.unwrap)
    val pageImgGeometry = pageImageGeometries(tr.page.unwrap)

    def transformPoint(p: Point): Point = {
      val pageTopTrans = (offsetPage.top * canvasH / totalPagesHeight) + canvasY
      val xtrans = (p.x * pageImgGeometry.width / offsetPage.width)
      val ytrans = (p.y * pageImgGeometry.height / offsetPage.height) + pageTopTrans
      p
    }

    val ftrans = tr.figure match {
      case f: LTBounds =>
        val lt = transformPoint(Point(f.left, f.top))
        val wh = transformPoint(Point(f.width, f.height))
        LTBounds(lt.x, lt.y, wh.x, wh.y)

      case f: LBBounds =>
        val lt = transformPoint(Point(f.left, f.bottom))
        val wh = transformPoint(Point(f.width, f.height))
        LBBounds(lt.x, lt.y, wh.x, wh.y)

      case f: Point => transformPoint(f)
      case f: Line => Line(transformPoint(f.p1), transformPoint(f.p2))
    }

    tr.copy(figure = ftrans)
  }

  def printlog(msg: String): Unit = {
    jQuery("#messages").prepend(
      s"""<li><small>${msg}</small></li>"""
    )
  }

  def printtree(msg: String, indent: Int=0): Unit = {
    val padl = "&nbsp;" * (indent * 4)
    jQuery("#messages").append(
      s"""<li><small>${padl}${msg}</small></li>"""
    )
  }

  def setupImageGeometries(): Unit = {
    pageImageGeometries.clear()

    jQuery(".page-image").map({ (elem: Element) =>
      val igeom = LTBounds(
        elem.clientLeft.toDouble,
        elem.clientTop.toDouble,
        elem.clientWidth.toDouble,
        elem.clientHeight.toDouble
      )
      printlog(s"page image geom = ${igeom} ")
      pageImageGeometries += igeom
    })

  }
  def setupPageGeometries(pageGeometries: Seq[PageGeometry]): Unit = {
    setupImageGeometries()
    pageGeometry.clear()
    pageOffsets.clear()

    pageGeometries.foreach { geom =>
      pageGeometry.put(geom.id.unwrap, geom)
    }

    pageGeometry.keys.toList.sorted.foreach{k =>
      val geom = pageGeometry(k)

      pageOffsets.lastOption match {
        case Some(lastPageGeom) =>
          pageOffsets += geom.bounds.copy(
            top=geom.bounds.top + lastPageGeom.top + lastPageGeom.height
          )
        case None =>
          pageOffsets += geom.bounds
      }
    }

  }

  // def drawGroupTree(traceEntries: Seq[TraceLog], level: Int): Unit = {
  //   val groupStack = mutable.Stack[Group]()
  //   traceEntries.zipWithIndex.foreach({ case (traceEntry, i) => traceEntry match {
  //     case SetPageGeometries(b: Seq[PageGeometry]) =>
  //       setupPageGeometries(b)

  //     case a:Group =>
  //       groupStack.push(a)
  //       val slen = groupStack.length

  //       val button = s"""<button id="group${slen}">${i}. ${a.name}</button>"""

  //       printtree(button, level)
  //       drawGroupTree(a.ts, level+1)

  //       $(s"#group${slen}").on("click", {(ev: JQueryEventObject) =>
  //         // fabricCanvas.renderOnAddRemove = false
  //         printlog(s"rendering visual trace")
  //         runTrace(Seq(a))
  //         // fabricCanvas.renderAll()
  //         // fabricCanvas.renderOnAddRemove = true
  //         printlog(s"finished visual trace")
  //         // removeTraceKeybindings()
  //       })
  //     case _ =>

  //   }})
  // }


  // TODO create State class
  var classNum = 0

  val classStack = mutable.Stack[String]()

  var currR = 0
  var currG = 0
  var currB = 0

  def nextRGB(): String  = {
    currR += 1; currR = currR % 16
    currG += 3; currG = currG % 16
    currB += 5; currB = currB % 16
    val r = currR.toHexString
    val g = currG.toHexString
    val b = currB.toHexString
    s"#$r$g$b"
  }

  var currRGB = nextRGB()


  // import scala.concurrent.Future
  import scala.concurrent.Promise
  import scala.async.Async.{async, await}
  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  import scala.collection.mutable
  import native.mousetrap._

  def runTrace(traces: Seq[TraceLog]): Unit = {
    // drawGroupTree(traces, 0)

    var stepper: Promise[Int] = Promise[Int]

    Mousetrap.bind("n", ((e: MousetrapEvent) => {
      val lastStepper = stepper
      stepper = Promise[Int]()
      lastStepper.success(1)
      true
    }))

    Mousetrap.bind("q", ((e: MousetrapEvent) => {
      val lastStepper = stepper
      stepper = Promise[Int]()
      lastStepper.success(0)
      true
    }))


    // Clear the canvas
    Mousetrap.bind("c", ((e: MousetrapEvent) => {
      fabricCanvas.forEachObject({(obj: native.fabric.FabricObject) =>
        fabricCanvas.remove(obj)
        fabricCanvas
      })
      true
    }))

    val execStack = mutable.Stack[mutable.Queue[TraceLog]]()

    def pushLogs(ls: Seq[TraceLog]): Unit = {
      execStack.push(mutable.Queue(ls:_*))
    }

    pushLogs(traces)

    async {

      var res = await { stepper.future }
      while (res > 0 && !execStack.isEmpty) {
        if (execStack.top.isEmpty) {
          execStack.pop()
        } else {
          val t = execStack.top.dequeue()
          printlog(s"running ${t}")
          if (execStack.top.isEmpty) {
            execStack.pop()
          }
          stepTraceLog(t)
        }

        fabricCanvas.renderAll()
        res = await { stepper.future }
      }
    }

    def stepTraceLog(logEntry: TraceLog): Unit = {
      logEntry match {
        case Noop =>
        case SetPageGeometries(b: Seq[PageGeometry]) => // set above

        case Show(s: Seq[TargetRegion]) =>
          val rs = s.map(transformTargetRegion(_))

          rs.foreach(tr => addShape(tr.bbox, "blue", currRGB, 0.1f))


        case ShowZone(s: Zone) =>
          println(s"ShowZone! ${s}")

        // case ShowComponent(s: Component) =>
        //   val ttrans = transformTargetRegion(s.targetRegion)
        //   addShape(ttrans.bbox, "blue", currRGB, 0.1f)

        // case ShowLabel(s: Label) =>

        //   val cls = "."+classStack.top
        //   val content = s"""<li><button class="$cls">${s.ns}:${s.key}</button></li>"""

        //   jQuery("#messages").append(content)

        case FocusOn(s: TargetRegion) =>
          val ttrans = transformTargetRegion(s)
          addShape(ttrans.bbox, "black", "yellow", 0.1f)

        case Indicate(s: TargetFigure) =>
          val ftrans = transformTargetFigure(s)
          println(s"Indicate: ${ftrans}")
          addShape(ftrans.figure, "black", "red", 0.2f)

        case Message(s: TB.Box) =>

          val msg = s"<pre>${s}</pre>"
          printlog(msg)

        case a:All =>
          pushLogs(a.ts)


        case a:Link =>
          // classNum += 1
          // classStack.push(s"c${classNum}")
          pushLogs(a.ts)
          // classStack.pop()

        case a:Group =>
          pushLogs(a.ts)

        case a:GroupEnd =>

      }
    }

  }




}













// case SetViewport(b: LTBounds) =>
// put a border around the canvas
// put message area below canvas
// set total width/height
// canvasX = scaleX(b.x).toInt
// canvasY = scaleY(b.y).toInt
// canvasW = scaleX(b.width).toInt
// canvasH = scaleY(b.height).toInt

// fabricCanvas.setWidth(canvasW+canvasBorder*2+1)
// fabricCanvas.setHeight(canvasH+canvasBorder*2+1)

// addShape(LTBounds(
//   canvasX, canvasY, canvasW, canvasH
// ), "black")
// // fabricCanvas.setBackgroundColor("green", () => {})

// println("b = " + translate(b))

// def setGradient(
//   obj: fabric.FabricObject,
//   strokeOrFill: String, // stroke|fill
//   gtype: String, // radial|linear
//   start: (Int, Int),
//   end: (Int, Int),
//   colorStops: js.Object,
//   radii: Option[(Int, Int)] = None
//     // gradientTransform: js.Object = js.Dynamic.literal()
// ): fabric.FabricObject = {

//   val rd1 = radii.map(_._1).getOrElse(0)
//   val rd2 = radii.map(_._2).getOrElse(0)

//   obj.setGradient(strokeOrFill,
//     js.Dynamic.literal(
//       "type"       -> gtype,
//       "x1"         -> start._1,
//       "x2"         -> start._2,
//       "y1"         -> end._1,
//       "y2"         -> end._2,
//       "r1"         -> rd1,
//       "r2"         -> rd2,
//       "colorStops" -> colorStops,
//       "gradientTransform" -> js.Array[Int](1, 0, 0, 1, 0, 0)
//     ))
// }
