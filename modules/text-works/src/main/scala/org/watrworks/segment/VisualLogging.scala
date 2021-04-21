package org.watrworks
package segment

import geometry._
import watrmarks._

import _root_.io.circe, circe._, circe.syntax._
import circe.generic.semiauto._
import scala.reflect._

import tracing._
import scala.collection.mutable
import TraceLog._
import transcripts.Transcript

sealed trait TraceLog {
  def tagged(s: String): TraceLog
}

object TraceLog {

  case class Headers(
    tags: List[String] = List()
  ) {
    def tagged(s: String) = copy(
      tags = s :: tags
    )
  }

  case class LabelTraceLog(
    tags: List[String] = List(),
    body: Seq[Transcript.Label]
  ) extends TraceLog {
    def tagged(s: String) = copy(
      tags = s :: tags
    )
  }
}

trait ScopedTracing extends VisualTracer { self =>

  protected val traceLogs = mutable.ArrayBuffer[TraceLog]()

  implicit def Encode_TraceLog: Encoder[TraceLog]           = deriveEncoder
  implicit def Encode_LabelTraceLog: Encoder[LabelTraceLog] = deriveEncoder
  implicit def Encode_Headers: Encoder[Headers]             = deriveEncoder

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
    val methodFqn     = enclosing.value.split("\\.").toList.last
    val rep1          = """.+[^#]#""".r
    val rep2          = """\$anonfun""".r
    val site0         = rep1.replaceFirstIn(methodFqn, "")
    val site          = rep2.replaceAllIn(site0, "")
    val qualifiedSite = s"@${site}"
    val l2            = log.tagged(qualifiedSite)
    traceLogs.append(l2)
  }

  def getLogsAsLabels(): Seq[Transcript.Label] = {
    self.traceLogs
      .to(Seq)
      .flatMap({
        case l: LabelTraceLog =>
          l.body.map(lbl => {
            lbl.copy(props = Some(Map("tags" -> l.tags)))
          })
        case _ => List()
      })
  }

  def figureToTransRange(figure: GeometricFigure): Transcript.GeometryRange = {
    Transcript.GeometryRange(
      "shape",
      figure
    )
  }

  def shapes(lshapes: Seq[AnyShape]): LabelTraceLog = {
    shape(lshapes: _*)
  }

  def shape(lshapes: AnyShape*): LabelTraceLog = {
    val shapes = lshapes.map { lshape: AnyShape =>
      val id        = TypeTags.LabelID(lshape.id.unwrap)
      val labelName = lshape.labels.map(_.fqn).mkString(",")
      val atShape   = figureToTransRange(lshape.shape);

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

trait DocumentScopeTracing extends ScopedTracing { self =>
  lazy val docTraceLogs = self

}

trait PageScopeTracing extends ScopedTracing { self =>
  lazy val traceLog = self

  def shapeIndex: ShapeIndex
  def pageNum: Int @@ PageNum
  def pageGeometry: LTBounds

  lazy val LTBounds.Doubles(pageL, pageT, pageW, pageH) = pageGeometry
  lazy val LTBounds.Ints(svgL, svgT, svgW, svgH)        = pageGeometry

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

  def figure[T <: GeometricFigure: ClassTag](figures: T): LabelTraceLog = {
    figure(Seq(figures))
  }

  def figure[T <: GeometricFigure: ClassTag](figures: Seq[T]): LabelTraceLog = {
    LabelTraceLog(
      body = figures.map(figureToTransLabel(_))
    )
  }

  private def figureToTransLabel[T <: GeometricFigure : ClassTag](figure: T): Transcript.Label = {
    val range = figureToTransRange(figure);
    val className = implicitly[ClassTag[T]] .runtimeClass.getSimpleName

    Transcript.Label(
      className,
      id = None, // TypeTags.LabelID(""),
      range = List(range),
      props = None,
      children = None
    )
  }

}
