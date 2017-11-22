package edu.umass.cs.iesl.watr
package segment

import geometry._
import spindex._

import _root_.io.circe, circe._, circe.syntax._

import tracing._
import scala.collection.mutable

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


case class LogEntry(
  name: String,
  jsonLog: Json
)


trait PageScopeTracing extends VisualTracer { self  =>
  lazy val traceLog = self

  def pageIndex: PageIndex

  lazy val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageIndex.pageGeometry.bounds
  lazy val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageIndex.pageGeometry.bounds

  lazy val pageLogs = mutable.ArrayBuffer[LogEntry]()

  def jsonAppend(enclosingCallSite: String)(body: => Json) =
    ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
      val methodFqn = enclosingCallSite.split("\\.").toList.last
      val logName = methodFqn.replace("#", " . ")
      pageLogs.append(LogEntry(
        logName, body
      ))
    }

  def emitLogs(): Seq[Json] = {
    for {
      logEntry <- pageLogs
    } yield Json.obj(
      ("name", s"Page ${pageIndex.pageNum.unwrap+1}: ${logEntry.name}".asJson),
      ("steps", Json.arr(logEntry.jsonLog))
    )
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

  // import textgrid._
  // def logTextGrid(textGrid: TextGrid)(implicit
  //   enclosing: sourcecode.Enclosing
  // ): Unit = jsonAppend(enclosing.value) {
  //   val outputBuilder = textGrid.buildOutput()
  // }


  def pageImageShapes(): Seq[Json] = {
    val LTBounds.Ints(left, top, width, height) = pageIndex.pageGeometry.bounds
    val border = Json.obj(
      "type" := "rect",
      "hover" := false,
      "x" := left,     "y" := top,
      "width" := width, "height" := height,
      "stroke" := "black", "stroke-width" := 1, "fill" := "none"
    )
    val image = Json.obj(
      ("type" := "image"), ("class" := "page-image"), ("hover" := false),
      ("page" := (pageIndex.pageNum.unwrap+1)),
      ("x" := left),     ("y" := top),
      ("width" := width), ("height" := height)
    )
    Seq(image, border)
  }

  def drawPageShapes()(implicit
    enclosing: sourcecode.Enclosing
  ): Unit = jsonAppend(enclosing.value) {

    val pageImage = pageImageShapes()

    val allShapes = pageIndex.shapes.shapeRIndex.getItems.map{ lshape =>
      val lls = lshape.labels.mkString(" ")
      lshape.shape match {
        case p@ Point.Ints(x, y) =>

          Json.obj(
            ("type" := "circle"), ("class" := lls),
            ("cx" := x), ("cy" := y),
            ("r" := 2),
            ("hover" := false),
          ).some

        case l@ Line(Point.Ints(p1x, p1y), Point.Ints(p2x, p2y)) =>

          Json.obj(
            ("type" := "line"), ("class" := lls), ("hover" := true),
            ("x1" := p1x), ("y1" := p1y),
            ("x2" := p2x), ("y2" := p2y)
          ).some

        case LTBounds.Ints(left, top, width, height) =>
          Json.obj(
            ("type" := "rect"), ("class" := lls), ("hover" := true),
            ("x" := left),     ("y" := top),
            ("width" := width), ("height" := height)
          ).some

        case _ => None
      }

    }

    val shapes = pageImage ++ allShapes.flatten

    Json.obj(
      ("desc" := "All Page Shapes"),
      ("Method" := DrawMethods.Outline),
      ("shapes" := shapes)
    )

  }
}




  // private def mkShapeRecs(shapes: Seq[GeometricFigure]): Seq[JsonObject] = {
  //   shapes.map{ _ match {
  //     case bbox: LTBounds =>
  //       val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

  //       Json.obj(
  //         ("type" -> "rect"), ("class" -> ""),
  //         ("x" -> JsNumber(l)),     ("y" -> JsNumber(t)),
  //         ("width" -> JsNumber(w)), ("height" -> JsNumber(h))
  //       )

  //     case p@ Point.Ints(x, y) =>
  //       Json.obj(
  //         ("type" -> "circle"), ("class" -> ""),
  //         ("cx" -> x), ("cy" -> y),
  //         ("r" -> 2)
  //       )

  //     case l@ Line(Point.Ints(p1x, p1y), Point.Ints(p2x, p2y)) =>

  //       Json.obj(
  //         ("type" -> "line"), ("class" -> ""),
  //         ("x1" -> p1x), ("y1" -> p1y),
  //         ("x2" -> p2x), ("y2" -> p2y)
  //       )

  //     // case b: LBBounds =>
  //     case _ => ???
  //     // case g @ GeometricGroup(bounds, figs) =>
  //     // case g @ Colorized(fig: GeometricFigure, fg: Color, bg: Color, fgOpacity: Float, bgOpacity: Float) =>
  //   } }
  // }

  // private def mkComponentRecs(ccs: Seq[Component]): Seq[JsonObject] = {
  //   ccs.map{ cc =>
  //     val bbox = cc.bounds()
  //     val LTBounds.Doubles(l, t, w, h) = rescale(bbox)

  //     Json.obj(
  //       ("id" -> s"shape-${cc.id.unwrap}"), ("class" -> cc.roleLabel.fqn),
  //       ("x" -> JsNumber(l)),     ("y" -> JsNumber(t)),
  //       ("width" -> JsNumber(w)), ("height" -> JsNumber(h))
  //     )
  //   }
  // }

  // private def formatLogRec(method: String, desc: String, shapes: Seq[GeometricFigure]): JsonObject = {
  //   Json.obj(
  //     ("desc" -> JsString(desc)),
  //     ("Method" -> method),
  //     ("shapes" -> mkShapeRecs(shapes))
  //   )
  // }

  // private def formatLogRecCcs(method: String, desc: String, ccs: Seq[Component]): JsonObject = {
  //   Json.obj(
  //     ("desc" -> JsString(desc + s"(${ccs.length})")),
  //     ("Method" -> method),
  //     ("shapes" -> mkComponentRecs(ccs))
  //   )
  // }

  // def zipFlashThroughRegions(desc: String, bboxes: Seq[LTBounds]): JsonObject = {
  //   formatLogRec(DrawMethods.ZipFlash, desc, bboxes)
  // }

  // def showShapes(desc: String, shapes: Seq[GeometricFigure]): JsonObject = {
  //   formatLogRec(DrawMethods.Draw, desc, shapes)
  // }

  // def showRegions(desc: String, bboxes: Seq[LTBounds]): JsonObject = {
  //   formatLogRec(DrawMethods.Draw, desc, bboxes)
  // }

  // def showMorph(desc: String, bboxes: LTBounds*)(
  //   // implicit lg: LogSpec
  // ): Unit = jsonAppend {
  //   formatLogRec(DrawMethods.Morph, desc, bboxes)
  // }

  // def drawPageGeometry()(
  //   // implicit lg: LogSpec
  // ): Unit = jsonAppend {
  //   val pageBounds = pageIndex.pageGeometry.bounds
  //   formatLogRec(DrawMethods.Outline, "Page Bounds", Seq(pageBounds))
  // }

  // def flashComponents(desc: String, ccs: Seq[Component])(
  //   // implicit lg: LogSpec
  // ): Unit = jsonAppend {
  //   formatLogRecCcs(DrawMethods.ZipFlash, desc, ccs)
  // }

  // def showComponentRemoval(desc: String, ccs: Seq[Component])(
  //   // implicit lg: LogSpec
  // ): Unit = jsonAppend {
  //   formatLogRecCcs(DrawMethods.Remove, desc, ccs)
  // }


  // def showLabeledComponents(desc: String, l: Label)(
  //   // implicit lg: LogSpec
  // ): Unit = {
  //   flashComponents(desc + s" ${l.fqn}",
  //     pageIndex.components.componentRTree.getItems.filter(_.hasLabel(l))
  //   )
  // }
