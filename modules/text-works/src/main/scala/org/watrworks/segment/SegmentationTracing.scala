package org.watrworks
package segment

import geometry._
import watrmarks._

import _root_.io.circe, circe._

import circe.generic.semiauto._
import scala.reflect._

import tracing._
import scala.collection.mutable
import transcripts.Transcript

trait TraceLogEncoder[A] {
  def encode(a: A): TraceLog
}

case class TraceLog(
  tags: List[String] = List(),
  outline: List[String] = List(),
  body: Seq[Transcript.Label]
) {
  def tagged(s: String) = copy(
    tags = s :: tags
  )
}

trait TracelogCodecs {
  this: ScopedTracing =>

  implicit val LabelTraceLogEncoder = new TraceLogEncoder[Transcript.Label] {
    def encode(l: Transcript.Label): TraceLog = {
      TraceLog(
        body = List(l)
      )
    }
  }

  implicit def FigureTraceLogEncoder[A <: GeometricFigure: ClassTag] = new TraceLogEncoder[A] {
    def encode(l: A): TraceLog = {
      figure(l)
    }
  }

  implicit def IdentTraceLogEncoder[A <: TraceLog] = new TraceLogEncoder[A] {
    def encode(l: A): TraceLog = {
      l
    }
  }

  implicit def Encode_TraceLog: Encoder[TraceLog]      = deriveEncoder
  implicit def Encode_LabelTraceLog: Encoder[TraceLog] = deriveEncoder

}

trait ScopedTracing extends VisualTracer with TracelogCodecs { self =>

  protected val traceLogs = mutable.ArrayBuffer[TraceLog]()

  def traceAll[A](avals: => Seq[A])(implicit
    Encoder: TraceLogEncoder[A]
  ) = ifTrace {
    avals.foreach { aval =>
      trace(aval)(Encoder)
    }
  }

  def trace[A](a: => A)(implicit
    Encoder: TraceLogEncoder[A]
  ) = ifTrace {
    val log = Encoder.encode(a)
    val taggedLog =
      log.copy(
        tags = log.tags ++ activeTags ++ scopeTags,
        outline = taskOutline.to(List).reverse
      )

    traceLogs.append(taggedLog)
  }

  def figure[T <: GeometricFigure: ClassTag](figure: T): TraceLog = {
    figures(Seq(figure))
  }

  def figures[T <: GeometricFigure: ClassTag](figures: Seq[T]): TraceLog = {
    TraceLog(
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

  // override this to add context tags like page#, 'LineSegmentation', etc.
  protected def scopeTags: List[String]

  // Dynamically add/remove tags during processing
  protected val activeTags = mutable.Stack[String]()
  def pushTag(s: String)   = activeTags.push(s)
  def popTag()             = activeTags.pop()
  def clearTags()          = activeTags.clear()

  // Keep track of task names to help with visual tracing
  protected val taskOutline = mutable.Stack[String]()
  def startTask(s: String)  = taskOutline.push(s)
  def endTask()             = taskOutline.pop()

  // Dynamically add/remove tags during processing
  protected val activeLabels               = mutable.Stack[Transcript.Label]()
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

  def getLogsAsLabels(): Seq[Transcript.Label] = {
    self.traceLogs
      .to(Seq)
      .flatMap({
        case l: TraceLog =>
          l.body.map(
            _.withProps(
              ("tags"    -> l.tags),
              ("outline" -> l.outline)
            )
          )
        case _ => List()
      })
  }

  def figureToTransRange(figure: GeometricFigure): Transcript.GeometryRange = {
    Transcript.GeometryRange(
      "shape",
      figure
    )
  }

  def shapes(lshapes: Seq[AnyShape]): TraceLog = {
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

  def shape(lshapes: AnyShape*): TraceLog = {
    TraceLog(
      body = shapesToLabels(lshapes: _*)
    )
  }
}

trait DocumentScopeTracing extends ScopedTracing { self =>
  lazy val docTraceLogs = self

}

trait PageScopeTracing extends ScopedTracing { self: BasePageSegmenter =>
  lazy val traceLog: PageScopeTracing = this

  def labeledShapes(labels: Label*): TraceLog = {
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
