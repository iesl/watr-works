package edu.umass.cs.iesl.watr
package segment

import geometry._
import spindex._
// import utils.Colors
// import utils.Color
import play.api.libs.json, json._
import tracing._
// import utils.ExactFloats._

object DrawMethods {
  def strIdent[V](implicit n: sourcecode.Name) = n.value

  val ZipFlash = strIdent
  val Morph    = strIdent
  val Draw     = strIdent
  val Outline  = strIdent
  val Remove   = strIdent
  val Emboss   = strIdent

  val Clear   = strIdent


}

trait PageScopeTracing extends VisualTracer { self  =>
  lazy val traceLog = self

  def pageIndex: PageIndex


  lazy val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageIndex.pageGeometry.bounds
  lazy val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageIndex.pageGeometry.bounds

  def initPageTracing(): Unit = {
    if (traceLog.tracingEnabled()) {
      tracing.VisualTracer.newPage()
    }
  }
  def rescale(bboxScale1: LTBounds): LTBounds = {
    val LTBounds.Doubles(l, t, w, h) = bboxScale1

    val scaleX = pageW / svgW
    val scaleY = pageH / svgH
    val l2 = l * scaleX
    val t2 = t * scaleY
    val w2 = w * scaleX
    val h2 = h * scaleY

    LTBounds.Doubles(l2, t2, w2, h2)
  }

  private def mkShapeRecs(shapes: Seq[GeometricFigure]): Seq[JsObject] = {
    shapes.map{ _ match {
      case bbox: LTBounds =>
        val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

        Json.obj(
          ("type" -> "rect"), ("class" -> ""),
          ("x" -> JsNumber(l)),     ("y" -> JsNumber(t)),
          ("width" -> JsNumber(w)), ("height" -> JsNumber(h))
        )

      case p@ Point.Ints(x, y) =>
        Json.obj(
          ("type" -> "circle"), ("class" -> ""),
          ("cx" -> x), ("cy" -> y),
          ("r" -> 2)
        )

      case l@ Line(Point.Ints(p1x, p1y), Point.Ints(p2x, p2y)) =>

        Json.obj(
          ("type" -> "line"), ("class" -> ""),
          ("x1" -> p1x), ("y1" -> p1y),
          ("x2" -> p2x), ("y2" -> p2y)
        )

      // case b: LBBounds =>
      case _ => ???
      // case g @ GeometricGroup(bounds, figs) =>
      // case g @ Colorized(fig: GeometricFigure, fg: Color, bg: Color, fgOpacity: Float, bgOpacity: Float) =>
    } }
  }
  private def mkRegions(bboxes: Seq[LTBounds]): Seq[JsObject] = {
    bboxes.map{ bbox =>
      val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

      Json.obj(
        ("x" -> JsNumber(l)),     ("y" -> JsNumber(t)),
        ("width" -> JsNumber(w)), ("height" -> JsNumber(h))
      )
    }
  }

  private def mkComponentRecs(ccs: Seq[Component]): Seq[JsObject] = {
    ccs.map{ cc =>
      val bbox = cc.bounds()
      val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

      Json.obj(
        ("id" -> s"shape-${cc.id.unwrap}"), ("class" -> cc.roleLabel.fqn),
        ("x" -> JsNumber(l)),     ("y" -> JsNumber(t)),
        ("width" -> JsNumber(w)), ("height" -> JsNumber(h))
      )
    }
  }

  private def formatLogRec(method: String, desc: String, shapes: Seq[GeometricFigure]): JsObject = {
    Json.obj(
      ("desc" -> JsString(desc)),
      ("Method" -> method),
      ("shapes" -> mkShapeRecs(shapes))
    )
  }

  private def formatLogRecCcs(method: String, desc: String, ccs: Seq[Component]): JsObject = {
    Json.obj(
      ("desc" -> JsString(desc + s"(${ccs.length})")),
      ("Method" -> method),
      ("shapes" -> mkComponentRecs(ccs))
    )
  }

  def zipFlashThroughRegions(desc: String, bboxes: Seq[LTBounds]): JsObject = {
    formatLogRec(DrawMethods.ZipFlash, desc, bboxes)
  }

  def showShapes(desc: String, shapes: Seq[GeometricFigure]): JsObject = {
    formatLogRec(DrawMethods.Draw, desc, shapes)
  }

  def showRegions(desc: String, bboxes: Seq[LTBounds]): JsObject = {
    formatLogRec(DrawMethods.Draw, desc, bboxes)
  }

  def showMorph(desc: String, bboxes: LTBounds*)(
    implicit lg: LogSpec
  ): Unit = jsonAppend {
    formatLogRec(DrawMethods.Morph, desc, bboxes)
  }

  def drawPageGeometry()(
    implicit lg: LogSpec
  ): Unit = jsonAppend {
    val pageBounds = pageIndex.pageGeometry.bounds
    formatLogRec(DrawMethods.Outline, "Page Bounds", Seq(pageBounds))
  }

  def flashComponents(desc: String, ccs: Seq[Component])(
    implicit lg: LogSpec
  ): Unit = jsonAppend {
    formatLogRecCcs(DrawMethods.ZipFlash, desc, ccs)
  }

  def showComponentRemoval(desc: String, ccs: Seq[Component])(
    implicit lg: LogSpec
  ): Unit = jsonAppend {
    formatLogRecCcs(DrawMethods.Remove, desc, ccs)
  }

  import watrmarks.Label

  def showLabeledComponents(desc: String, l: Label)(
    implicit lg: LogSpec
  ): Unit = {
    flashComponents(desc + s" ${l.fqn}",
      pageIndex.components.componentRTree.getItems.filter(_.hasLabel(l))
    )
  }

  def drawPageShapes()(
    implicit lg: LogSpec
  ): Unit = jsonAppend {
    val pageBounds = pageIndex.pageGeometry.bounds

    val allShapes = pageIndex.shapes.shapeRIndex.getItems.map{ lshape =>
      val lls = lshape.labels.mkString(" ")
      lshape.shape match {
        case p@ Point.Ints(x, y) =>
          Json.obj(
            ("type" -> "circle"), ("class" -> lls),
            ("cx" -> x), ("cy" -> y),
            ("r" -> 2)
          ).some

        case l@ Line(Point.Ints(p1x, p1y), Point.Ints(p2x, p2y)) =>

          Json.obj(
            ("type" -> "line"), ("class" -> lls),
            ("x1" -> p1x), ("y1" -> p1y),
            ("x2" -> p2x), ("y2" -> p2y)
          ).some
        case LTBounds.Ints(left, top, width, height) =>
          Json.obj(
            ("type" -> "rect"), ("class" -> lls),
            ("x" -> left),     ("y" -> top),
            ("width" -> width), ("height" -> height)
          ).some

        case _ => None
      }

    }

    val pgBounds = mkRegions(Seq(pageBounds))

    val shapes = pgBounds ++ (allShapes.flatten)

    Json.obj(
      ("desc" -> JsString("All Page Shapes")),
      ("Method" -> DrawMethods.Outline),
      ("shapes" -> shapes)
    )

  }
}





















// class VisualLogger(
//   logName: String,
//   pageIndex: PageIndex,
//   outputRoot: Path
// ) {
//   val rTreeIndex = pageIndex.componentRTree
//   val pageNum = pageIndex.pageGeometry.id

//   val logs = mutable.ListBuffer[JsObject]()

//   val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageIndex.pageGeometry.bounds
//   val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageIndex.pageGeometry.bounds

//   def jsonLogFile(name: String) = s"${name}.pg${pageNum}.json"


//   def rescale(bboxScale1: LTBounds): LTBounds = {
//     val LTBounds.Doubles(l, t, w, h) = bboxScale1

//     val scaleX = pageW / svgW
//     val scaleY = pageH / svgH
//     val l2 = l * scaleX
//     val t2 = t * scaleY
//     val w2 = w * scaleX
//     val h2 = h * scaleY

//     LTBounds.Doubles(l2, t2, w2, h2)
//   }


//   def showRegions(name: String, bboxes: Seq[LTBounds], lineColor: Color=Colors.Blue, fillColor: Color=Colors.Yellow): Unit = {
//     val boxBlock = bboxes.map{ bbox =>
//       val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

//       Json.obj(
//         ("x" -> JsNumber(l)),     ("y" -> JsNumber(t)),
//         ("width" -> JsNumber(w)), ("height" -> JsNumber(h))
//       )
//     }

//     val obj = Json.obj(
//       ("desc" -> JsString(name)),
//       ("shapes" -> boxBlock)
//     )

//     logs += obj
//   }



//   def writeLogs(): Unit = {

//     val logJson = Json.toJson(logs.toList)
//     val jsonStr = Json.prettyPrint(logJson)

//     if (!fs.exists(outputRoot)) {
//       fs.mkdir(outputRoot)
//     }

//     val outPath = outputRoot / jsonLogFile(logName)

//     if (fs.exists(outPath)) {
//       fs.rm(outPath)
//     }

//     fs.write(outPath, jsonStr)

//   }

// }
