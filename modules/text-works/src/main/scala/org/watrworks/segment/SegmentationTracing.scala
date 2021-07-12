package org.watrworks
package segment

import geometry._
import watrmarks._

import _root_.io.circe, circe._

import circe.generic.semiauto._
import scala.reflect._

import tracing._
import scala.collection.mutable
import TraceLog._
import transcripts.Transcript

trait TraceLogEncoder[A] {
  def encode(a: A): TraceLog
}

sealed trait TraceLog {
  def tagged(s: String): TraceLog
}

object TraceLog {

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

  def figure[T <: GeometricFigure: ClassTag](figure: T): LabelTraceLog = {
    figures(Seq(figure))
  }

  def figures[T <: GeometricFigure: ClassTag](figures: Seq[T]): LabelTraceLog = {
    LabelTraceLog(
      body = figures.map(figureToTransLabel(_))
    )
  }

  private def figureToTransLabel[T <: GeometricFigure: ClassTag](figure: T): Transcript.Label = {
    val range     = figureToTransRange(figure);
    val className = implicitly[ClassTag[T]].runtimeClass.getSimpleName

    Transcript.Label(
      className,
      id = None, // TypeTags.LabelID(""),
      range = List(range),
      props = None,
      children = None
    )
  }
  implicit val LabelTraceLogEncoder = new TraceLogEncoder[Transcript.Label] {
    def encode(l: Transcript.Label): TraceLog = {
      TraceLog.LabelTraceLog(
        body = List(l)
      )
    }
  }

  implicit def FigureTraceLogEncoder[A <: GeometricFigure : ClassTag] = new TraceLogEncoder[A] {
    def encode(l: A): TraceLog = {
      figure(l)
    }
  }
  implicit def IdentTraceLogEncoder[A <: TraceLog]= new TraceLogEncoder[A] {
    def encode(l: A): TraceLog = {
      l
    }
  }

  protected val traceLogs = mutable.ArrayBuffer[TraceLog]()

  // override this to add context tags like page#, 'LineSegmentation', etc.
  protected def scopeTags: List[String]

  // Dynamically add/remove tags during processing
  protected val activeTags = mutable.Stack[String]()
  def pushTag(s: String)   = activeTags.push(s)
  def popTag()             = activeTags.pop()
  def clearTags()          = activeTags.clear()

  // Dynamically add/remove tags during processing
  protected val activeLabels         = mutable.Stack[Transcript.Label]()
  def pushLabel(l: Transcript.Label): Unit = activeLabels.push(l)
  def modLabel(f: Transcript.Label => Transcript.Label): Unit = {
    val top = activeLabels.pop()
    pushLabel(f(top))
  }
  def popLabel(): Unit = {
    val top = activeLabels.pop()
    if (activeLabels.isEmpty) {
      trace(top)
    } else {
      val nextLabel = activeLabels.pop()
      pushLabel(nextLabel.withChildren(top))
    }
  }

  implicit def Encode_TraceLog: Encoder[TraceLog]           = deriveEncoder
  implicit def Encode_LabelTraceLog: Encoder[LabelTraceLog] = deriveEncoder

  def traceAll[A](avals: => Seq[A])(implicit
    enclosing: sourcecode.Enclosing,
    Encoder: TraceLogEncoder[A]
  ) = ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
    avals.foreach { aval =>
      trace(aval)(enclosing, Encoder)
    }
  }

  def trace[A](a: => A)(implicit
    enclosing: sourcecode.Enclosing,
    Encoder: TraceLogEncoder[A]
  ) = ifTrace(tracemacros.VisualTraceLevel.JsonLogs) {
    val log = Encoder.encode(a)

    val methodFqn     = enclosing.value.split("\\.").toList.last
    val rep1          = """.+[^#]#""".r
    val rep2          = """\$anonfun""".r
    val site0         = rep1.replaceFirstIn(methodFqn, "")
    val site          = rep2.replaceAllIn(site0, "")
    val qualifiedSite = s"@${site}()"
    val allTags       = qualifiedSite +: (activeTags ++ scopeTags)
    val taggedLog     = allTags.foldLeft(log) { case (acc, e) => acc.tagged(e) }

    traceLogs.append(taggedLog)
  }

  def getLogsAsLabels(): Seq[Transcript.Label] = {
    self.traceLogs
      .to(Seq)
      .flatMap({
        case l: LabelTraceLog =>
          l.body.map(_.withProps(("tags" -> l.tags)))
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

  def shapesToLabels(lshapes: AnyShape*): Seq[Transcript.Label] = {
    lshapes.map { lshape: AnyShape =>
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
  }

  def shape(lshapes: AnyShape*): LabelTraceLog = {
    LabelTraceLog(
      body = shapesToLabels(lshapes: _*)
    )
  }
}

trait DocumentScopeTracing extends ScopedTracing { self =>
  lazy val docTraceLogs = self

}

trait PageScopeTracing extends ScopedTracing { self: BasePageSegmenter =>
  lazy val traceLog: PageScopeTracing = this



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


}
