package org.watrworks
package segment

import scala.{ collection => sc }
import sc.Seq
import geometry._
import rtrees._
import watrmarks._

import _root_.io.circe, circe._, circe.syntax._
import circe.generic.semiauto._

import tracing._
import scala.collection.mutable
import java.time.Instant
import textboxing.{TextBoxing => TB}
import GeometryCodecs._
import TraceLog._
import extract.FontExportRecords


sealed trait TraceLog {
  def withCallSite(s: String): TraceLog
  def tagged(s: String): TraceLog
  def named(s: String): TraceLog
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

    def named(s: String) = copy(
      name = s.trim
    )
  }

  case class RelationTraceLog(
    headers: Headers = Headers(),
    fieldTypes: List[String] = List(),
    body: List[Json]
  ) extends TraceLog {
    def withCallSite(s: String) = copy(headers = headers.withCallSite(s))
    def tagged(s: String) = copy(headers = headers.tagged(s))
    def named(s: String) = copy(headers = headers.named(s))


    def col[A: Encoder](colname: String, a: A): RelationTraceLog = {
      copy(
        body = body :+ a.asJson,
        fieldTypes = fieldTypes :+ colname
      )
    }

    def field(shape: AnyShape): RelationTraceLog = {
      val ts = shape.labels.map(_.fqn).mkString("|")
      copy(
        body = body :+ shape.id.asJson,
        fieldTypes = fieldTypes :+ ts
      )
    }

    def field(shapes: Seq[AnyShape]): RelationTraceLog = {
      val ts = shapes.map(
        _.labels.map(_.fqn).mkString("|")
      ).toSet.toList.sorted.mkString(", ")

      copy(
        body = body :+ shapes.map(_.id).asJson,
        fieldTypes = fieldTypes :+ ts
      )
    }
  }

  case class GeometryTraceLog(
    headers: Headers = Headers(),
    body: Json
  ) extends TraceLog {
    def withCallSite(s: String) = copy(headers = headers.withCallSite(s))
    def tagged(s: String) = copy(headers = headers.tagged(s))
    def named(s: String) = copy(headers = headers.named(s))
  }

  case class BoxTextLog(
    headers: Headers = Headers(),
    body: Seq[String]
  ) extends TraceLog {
    def withCallSite(s: String) = copy(headers = headers.withCallSite(s))
    def tagged(s: String) = copy(headers = headers.tagged(s))
    def named(s: String) = copy(headers = headers.named(s))
  }

  case class FontEntryLog(
    headers: Headers = Headers(),
    body: Json
  ) extends TraceLog {
    def withCallSite(s: String) = copy(headers = headers.withCallSite(s))
    def tagged(s: String) = copy(headers = headers.tagged(s))
    def named(s: String) = copy(headers = headers.named(s))
  }

}



trait ScopedTracing extends VisualTracer { self  =>

  protected val traceLogs = mutable.ArrayBuffer[TraceLog]()

  implicit def Encode_TraceLog: Encoder[TraceLog] =  deriveEncoder
  implicit def Encode_GeometryTraceLog: Encoder[GeometryTraceLog] =  deriveEncoder
  implicit def Encode_RelationTraceLog: Encoder[RelationTraceLog] =  deriveEncoder
  implicit def Encode_BoxTextLog: Encoder[BoxTextLog] =  deriveEncoder
  implicit def Encode_FontEntryLog: Encoder[FontEntryLog] =  deriveEncoder
  implicit def Encode_Headers: Encoder[Headers] =  deriveEncoder




  def traceAll(logs: => Seq[TraceLog])(
    implicit enclosing: sourcecode.Enclosing
  ) = ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
    logs.foreach { log =>
      trace(log)(enclosing)
    }
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

  def boxText(bs: TB.Box): BoxTextLog = {
    BoxTextLog(
      body = TB.renderBox(bs)
    )
  }

  import FontExportRecords._

  def fontSummaries(f: List[FontSummary]): FontEntryLog = {
    FontEntryLog(
      body = f.asJson
    )
  }

  def emitLogJsons(): Seq[JsonObject] = {
    for { logEntry <- traceLogs } yield {
      val entry = logEntry match {
        case l: GeometryTraceLog =>
          l.asJson.asObject.get
            .add("logType", "Geometry".asJson)

        case l: RelationTraceLog =>
          l.asJson.asObject.get
            .add("logType", "Relation".asJson)

        case l: BoxTextLog =>
          l.asJson.asObject.get
            .add("logType", "Text".asJson)

        case l: FontEntryLog =>
          l.asJson.asObject.get
            .add("logType", "Font".asJson)
      }

      entry
    }
  }

}

trait DocumentScopeTracing extends ScopedTracing { self  =>
  lazy val docTraceLogs = self

  def emitLogs(): Seq[Json] = {
    emitLogJsons().map(_.asJson)
  }
}

trait PageScopeTracing extends ScopedTracing { self  =>
  lazy val traceLog = self
  import SegmentationSystem._

  def shapeIndex: SegmentationSystem.ShapeIndex
  def pageNum: Int@@PageNum
  def pageGeometry: LTBounds

  lazy val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageGeometry
  lazy val LTBounds.Ints(svgL, svgT, svgW, svgH) = pageGeometry

  def emitLogs(): Seq[Json] = {
    emitLogJsons().map{ jsonObj =>
      jsonObj.add("page", pageNum.unwrap.asJson).asJson
    }
  }

  def labeledShapes(labels: Label*): GeometryTraceLog = {
    def filterf(shape: AnyShape): Boolean = {
      labels.exists(shape.hasLabel(_))
    }

    val f = if (labels.nonEmpty) filterf(_)
            else (_: AnyShape) => true

    val filtered = shapeIndex.shapeRIndex.getItems.filter(f)
    shape(filtered:_*)
  }



  def relation(
    name: String
  ): RelationTraceLog = {
    RelationTraceLog(
      body = List()
    ).named(name)
  }
  def figure(figures: GeometricFigure): GeometryTraceLog = {
    figure(Seq(figures))
  }

  def figure(figures: Seq[GeometricFigure]): GeometryTraceLog = {
    GeometryTraceLog(
      body = figures.toList.asJson
    )
  }

  def shape[A <: GeometricFigure](lshapes: AnyShape*): GeometryTraceLog = {
    ???

    // val shapes = lshapes.map{ lshape: AnyShape =>
    //   lshape.shape.asJson
    //     .add("id", lshape.id.asJson)
    //     .add("labels", lshape.labels.mkString(" ").asJson)
    // }

    // val labels = lshapes.flatMap{ lshape =>
    //   lshape.labels
    // }.toSet.toList.mkString(" ")

    // GeometryTraceLog(
    //   body = shapes.asJson
    // ).tagged(labels)
  }

}
