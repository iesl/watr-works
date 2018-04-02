package edu.umass.cs.iesl.watr
package segment

import geometry._
import spindex._
import watrmarks._

import _root_.io.circe, circe._, circe.syntax._
import circe.generic.semiauto._

import tracing._
import scala.collection.mutable
import java.time.Instant


case class TraceLog(
  name: String = "",
  callSite: String = "",
  description: String = "",
  typeHint: String = "",
  tags: String = "",
  timestamp: Long,
  log: Json
) {
  def tagged(s: String) = copy(
    tags = tags + " " + s
  )
  def named(s: String) = copy(
    name = s
  )
}

// case class LogEntry(
//   name: String,
//   log: Json
// )


trait PageScopeTracing extends VisualTracer { self  =>
  lazy val traceLog = self

  def pageIndex: PageIndex

  lazy val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageIndex.pageGeometry.bounds
  lazy val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageIndex.pageGeometry.bounds

  // lazy val pageLogs = mutable.ArrayBuffer[LogEntry]()
  lazy val traceLogs = mutable.ArrayBuffer[TraceLog]()

  import GeometryCodecs._


  def labeledShapes(labels: Label*): TraceLog = {
    def filterf(shape: LabeledShape[_ <: GeometricFigure]): Boolean = {
      labels.exists(shape.hasLabel(_))
    }

    val f = if (labels.nonEmpty) filterf(_)
            else (_: LabeledShape[_ <: GeometricFigure]) => true

    val filtered = pageIndex.shapes.shapeRIndex.getItems.filter(f)
    shape(filtered:_*)
  }

  def figure(figures: GeometricFigure*): TraceLog = {
    TraceLog(
      typeHint = "Figures",
      timestamp = Instant.now().toEpochMilli(),
      log = figures.toList.asJson
    )
  }

  def shape[A <: GeometricFigure](lshapes: LabeledShape[A]*): TraceLog = {

    val shapes = lshapes.map{ lshape: LabeledShape[GeometricFigure] =>

      lshape.shape.asJsonObject
        .add("labels", lshape.labels.mkString(" ").asJson)
    }
    val labels = lshapes.flatMap{ lshape =>
      lshape.labels
    }.toSet.toList.mkString(" ")

    TraceLog(
      typeHint = "LabeledShapes",
      tags = labels,
      timestamp = Instant.now().toEpochMilli(),
      log = shapes.asJson
    )
  }


  def trace(log: => TraceLog)(
    implicit enclosing: sourcecode.Enclosing
  ) = ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
    val methodFqn = enclosing.value.split("\\.").toList.last
    val site = methodFqn.replace("#", " . ").replace("$anonfun", "").trim
    val l2 = log.copy(callSite = site)
    traceLogs.append(l2)
  }

  // private def jsonAppend(enclosingCallSite: String)(body: => Json) =
  //   ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
  //     val methodFqn = enclosingCallSite.split("\\.").toList.last
  //     val logName = methodFqn.replace("#", " . ")
  //     pageLogs.append(LogEntry(
  //       logName, body
  //     ))
  //   }

  implicit def Encode_TraceLog: Encoder[TraceLog] =  deriveEncoder
  def emitLogs(): Seq[Json] = {
    for { logEntry <- traceLogs } yield Json.obj(
      "page" := pageIndex.pageNum.unwrap,
      "log" := logEntry
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


  // private def appendLog(
  //   description: String,
  //   logGroup: String,
  //   typeHint: String,
  //   enclosing: sourcecode.Enclosing,
  //   log: Json,
  // ): Unit = jsonAppend(enclosing.value) {
  //   val currentTime = Instant.now().toEpochMilli()

  //   Json.obj(
  //     "desc" := description,
  //     "logGroup" := logGroup,
  //     "typeHint" := typeHint,
  //     "timestamp" := currentTime,
  //     "shapes" := log
  //   )
  // }

  // private def appendLogFigures(
  //   description: String,
  //   logGroup: String,
  //   figures: Seq[GeometricFigure]
  // )(enclosing: sourcecode.Enclosing): Unit = {
  //   appendLog(description, logGroup, "Figures", enclosing, figures.asJson)
  // }

  // private def appendLogIndexedShapes(
  //   description: String,
  //   logGroup: String,
  //   labels: Seq[Label]
  // )(enclosing: sourcecode.Enclosing): Unit = jsonAppend(enclosing.value) {
  //   val currentTime = Instant.now().toEpochMilli()

  //   def filterf(shape: LabeledShape[_]): Boolean = {
  //     labels.exists(shape.hasLabel(_))
  //   }

  //   val f = if (labels.nonEmpty) filterf(_)
  //           else (_: LabeledShape[_]) => true

  //   val shapes = pageIndex.shapes.shapeRIndex.getItems
  //     .filter(f).map{ lshape =>
  //       Json.obj(
  //         "figure" := lshape.shape,
  //         "labels" := lshape.labels.mkString(" ")
  //       )
  //     }


  //   Json.obj(
  //     "desc" := description,
  //     "logGroup" := logGroup,
  //     "typeHint" := "LabeledShapes",
  //     "timestamp" := currentTime,
  //     "shapes" := shapes
  //   )
  // }

  // def drawPageShapes(labels: Label*)(implicit
  //   enclosing: sourcecode.Enclosing
  // ): Unit = drawLabeledPageShapes("", "LabeledShapes", labels)(enclosing)

  // def describePageShapes(desc: String)(labels: Label*)(implicit
  //   enclosing: sourcecode.Enclosing
  // ): Unit = drawLabeledPageShapes(desc, "LabeledShapes", labels)(enclosing)

  // def logFigures(logGroup: String, description: String = "")(shapes: GeometricFigure*)(implicit
  //   enclosing: sourcecode.Enclosing
  // ): Unit = {

  //   // drawLabeledPageShapes(desc, "LabeledShapes", labels)(enclosing)
  // }

  // def logShapes[A](logGroup: String, description: String = "")(shapes: LabeledShape[A]*)(implicit
  //   enclosing: sourcecode.Enclosing
  // ): Unit = {
  //   appendLogIndexedShapes(description, logGroup, shapes)(enclosing)
  //   // drawLabeledPageShapes(desc, "LabeledShapes", labels)(enclosing)
  // }

}
