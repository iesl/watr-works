package org.watrworks
package segment

// import scala.{collection => sc}
// import sc.Seq
import geometry._
import watrmarks._

import _root_.io.circe, circe._, circe.syntax._
import circe.generic.semiauto._

import tracing._
import scala.collection.mutable
import java.time.Instant
import textboxing.{TextBoxing => TB}
import TraceLog._
import transcripts.Transcript

sealed trait TraceLog {
  def withCallSite(s: String): TraceLog
  def tagged(s: String): TraceLog
}

object TraceLog {

  case class Headers(
    name: String = "",
    callSite: String = "",
    tags: String = "",
    timestamp: Long = Instant.now().toEpochMilli()
  ) {

    def withCallSite(s: String) = copy(
      callSite = s
    )

    def tagged(s: String) = copy(
      tags = (tags.trim + " " + s.trim).trim
    )
  }

  case class LabelTraceLog(
    headers: Headers = Headers(),
    body: Seq[Transcript.Label]
  ) extends TraceLog {
    def withCallSite(s: String) = copy(headers = headers.withCallSite(s))
    def tagged(s: String) = copy(headers = headers.tagged(s))
  }

  case class BoxTextLog(
    headers: Headers = Headers(),
    body: Seq[String]
  ) extends TraceLog {
    def withCallSite(s: String) = copy(headers = headers.withCallSite(s))
    def tagged(s: String) = copy(headers = headers.tagged(s))
  }
}

trait ScopedTracing extends VisualTracer { self =>

  protected val traceLogs = mutable.ArrayBuffer[TraceLog]()

  implicit def Encode_TraceLog: Encoder[TraceLog] = deriveEncoder
  implicit def Encode_LabelTraceLog: Encoder[LabelTraceLog] = deriveEncoder
  implicit def Encode_BoxTextLog: Encoder[BoxTextLog] = deriveEncoder
  implicit def Encode_Headers: Encoder[Headers] = deriveEncoder

  def traceAll(logs: => Seq[TraceLog])(implicit
    enclosing: sourcecode.Enclosing
  ) = ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
    logs.foreach { log =>
      trace(log)(enclosing)
    }
  }

  def trace(log: => TraceLog)(implicit
    enclosing: sourcecode.Enclosing
  ) = ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
    val methodFqn = enclosing.value.split("\\.").toList.last
    val rep1 = """.+[^#]#""".r
    val rep2 = """\$anonfun""".r
    val site0 = rep1.replaceFirstIn(methodFqn, "")
    val site = rep2.replaceAllIn(site0, "")
    val l2 = log.withCallSite(site.trim)
    traceLogs.append(l2)
  }

  def boxText(bs: TB.Box): BoxTextLog = {
    BoxTextLog(
      body = TB.renderBox(bs)
    )
  }

  def emitLogJsons(): Seq[JsonObject] = {
    for { logEntry <- traceLogs.to(Seq) } yield {
      val entry = logEntry match {
        case l: LabelTraceLog =>
          l.asJson.asObject.get
            .add("kind", "Label".asJson)

        case l: BoxTextLog =>
          l.asJson.asObject.get
            .add("kind", "Text".asJson)
      }

      entry
    }
  }

}

trait DocumentScopeTracing extends ScopedTracing { self =>
  lazy val docTraceLogs = self

  def emitLogs(): Seq[Json] = {
    emitLogJsons().map(_.asJson)
  }

  def emitLoggedLabels(): Seq[Transcript.Label] = {
    self.traceLogs.to(Seq).flatMap({
      case l: LabelTraceLog => l.body
      case _                => List()
    })
  }
}

trait PageScopeTracing extends ScopedTracing { self =>
  lazy val traceLog = self

  def shapeIndex: SegmentationSystem.ShapeIndex
  def pageNum: Int @@ PageNum
  def pageGeometry: LTBounds

  lazy val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageGeometry
  lazy val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageGeometry

  def emitLogs(): Seq[Json] = {
    emitLogJsons().map { jsonObj =>
      jsonObj.add("page", pageNum.unwrap.asJson).asJson
    }
  }

  def labeledShapes(labels: Label*): LabelTraceLog = {
    def filterf(shape: AnyShape): Boolean = {
      labels.exists(shape.hasLabel(_))
    }

    val f =
      if (labels.nonEmpty) filterf(_)
      else (_: AnyShape) => true

    val filtered = shapeIndex.shapeRIndex.getItems().filter(f)
    shape(filtered: _*)
  }

  def figure(figures: GeometricFigure): LabelTraceLog = {
    figure(Seq(figures))
  }

  def figure(figures: Seq[GeometricFigure]): LabelTraceLog = {
    LabelTraceLog(
      body = figures.map(figureToTransLabel(_))
    )
  }

  def figureToTransLabel(figure: GeometricFigure): Transcript.Label = {
    val range = figureToTransRange(figure);
    Transcript.Label(
      "(anon)",
      id = None, // TypeTags.LabelID(""),
      range = List(range),
      props = None,
      children = None
    )
  }

  def figureToTransRange(figure: GeometricFigure): Transcript.GeometryRange = {
    // val unit = figure match {
    //   case shape: LTBounds => Transcript.GeometryLabelUnit.Rect
    //   case shape: Line => Transcript.GeometryLabelUnit.Line
    //   case shape: Point => Transcript.GeometryLabelUnit.Point
    //   case shape: Trapezoid => Transcript.GeometryLabelUnit.Trapezoid
    // }

    Transcript.GeometryRange(
      "shape",
      figure
    )
  }

  def shape[A <: GeometricFigure](lshapes: AnyShape*): LabelTraceLog = {
    val shapes = lshapes.map { lshape: AnyShape =>
      val id = TypeTags.LabelID(lshape.id.unwrap)
      val labelName = lshape.labels.map(_.fqn).mkString(",")
      val atShape = figureToTransRange(lshape.shape);

      Transcript.Label(
        name = labelName,
        id = Some(id),
        range = List(atShape),
        props = None,
        children = None
      )
    }

    LabelTraceLog(
      body = shapes
    )
  }

}
