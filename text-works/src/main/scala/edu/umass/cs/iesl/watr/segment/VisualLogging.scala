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
import textboxing.{TextBoxing => TB}


sealed trait TraceLog {
  def withCallSite(s: String): TraceLog
}

object TraceLog {

  case class GeometryTraceLog(
    name: String = "",
    callSite: String = "",
    description: String = "",
    typeHint: String = "",
    tags: String = "",
    timestamp: Long,
    body: Json
  ) extends TraceLog {

    def withCallSite(s: String): TraceLog =
      this.copy(callSite = s)

    def tagged(s: String) = copy(
      tags = tags + " " + s
    )
    def named(s: String) = copy(
      name = s
    )
  }

  case class BoxTextLog(
    name: String = "",
    callSite: String = "",
    description: String = "",
    typeHint: String = "",
    tags: String = "",
    timestamp: Long,
    body: Seq[String]
  ) extends TraceLog {
    def withCallSite(s: String): TraceLog =
      this.copy(callSite = s)

    def tagged(s: String) = copy(
      tags = tags + " " + s
    )
    def named(s: String) = copy(
      name = s
    )
  }

}


import GeometryCodecs._
import TraceLog._

trait ScopedTracing extends VisualTracer { self  =>

  protected val traceLogs = mutable.ArrayBuffer[TraceLog]()

  implicit def Encode_TraceLog: Encoder[TraceLog] =  deriveEncoder
  implicit def Encode_GeometryTraceLog: Encoder[GeometryTraceLog] =  deriveEncoder
  implicit def Encode_BoxTextLog: Encoder[BoxTextLog] =  deriveEncoder


  def getTraceLogs(): Seq[TraceLog] = {
    traceLogs
  }


  def boxText(bs: TB.Box): BoxTextLog = {
    BoxTextLog(
      typeHint = "BoxText",
      timestamp = Instant.now().toEpochMilli(),
      body = TB.renderBox(bs)
    )
  }

  def trace(log: => TraceLog)(
    implicit enclosing: sourcecode.Enclosing
  ) = ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
    val methodFqn = enclosing.value.split("\\.").toList.last
    val rep1 = """.+[^#]#""".r
    val rep2 = """\$anonfun""".r
    val site0 = rep1.replaceFirstIn(methodFqn, "")
    val site = rep2.replaceAllIn(site0, "")
    val l2 = log.withCallSite(site.trim)
    traceLogs.append(l2)
  }

}

trait DocumentScopeTracing extends ScopedTracing { self  =>
  lazy val docTraceLogs = self

  def emitLogs(): Seq[Json] = {
    for { logEntry <- traceLogs } yield Json.obj(
      "entry" := logEntry
    )
  }
}

trait PageScopeTracing extends ScopedTracing { self  =>
  lazy val traceLog = self

  def pageIndex: PageIndex

  lazy val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageIndex.pageGeometry.bounds
  lazy val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageIndex.pageGeometry.bounds

  def emitLogs(): Seq[Json] = {
    for { logEntry <- traceLogs } yield Json.obj(
      "page" := pageIndex.pageNum.unwrap,
      "entry" := logEntry
    )
  }


  def labeledShapes(labels: Label*): GeometryTraceLog = {
    def filterf(shape: LabeledShape[_ <: GeometricFigure]): Boolean = {
      labels.exists(shape.hasLabel(_))
    }

    val f = if (labels.nonEmpty) filterf(_)
            else (_: LabeledShape[_ <: GeometricFigure]) => true

    val filtered = pageIndex.shapes.shapeRIndex.getItems.filter(f)
    shape(filtered:_*)
  }

  def figure(figures: GeometricFigure*): GeometryTraceLog = {
    GeometryTraceLog(
      typeHint = "Figures",
      timestamp = Instant.now().toEpochMilli(),
      body = figures.toList.asJson
    )
  }

  def shape[A <: GeometricFigure](lshapes: LabeledShape[A]*): GeometryTraceLog = {

    val shapes = lshapes.map{ lshape: LabeledShape[GeometricFigure] =>

      lshape.shape.asJsonObject
        .add("labels", lshape.labels.mkString(" ").asJson)
    }

    val labels = lshapes.flatMap{ lshape =>
      lshape.labels
    }.toSet.toList.mkString(" ")

    GeometryTraceLog(
      typeHint = "LabeledShapes",
      tags = labels,
      timestamp = Instant.now().toEpochMilli(),
      body = shapes.asJson
    )
  }




  private def rescale(bboxScale1: LTBounds): LTBounds = {
    val LTBounds.Doubles(l, t, w, h) = bboxScale1

    val scaleX = pageW / svgW
    val scaleY = pageH / svgH
    val l2 = l * scaleX
    val t2 = t * scaleY
    val w2 = w * scaleX
    val h2 = h * scaleY

    LTBounds.Doubles(l2, t2, w2, h2)
  }

}
