package edu.umass.cs.iesl.watr
package tracing

import tracemacros._
import scala.language.experimental.macros
import geometry._
import scala.collection.mutable
import VisualTraceLevel._

object VisualTracer {

  { // Set trace levels here to force recompile and macro re-eval
    // VisualTraceGlobals.addTraceLevel(VisualTraceLevel.JsonLogs)
  }

  def tracingEnabled(): Boolean = {
    VisualTraceGlobals.tracingEnabled()
  }

}

protected [tracing] object VisualTraceGlobals {

  val activeTraces = mutable.HashSet[VisualTraceLevel]()

  def tracingEnabled(): Boolean = {
    activeTraces.nonEmpty
  }

  def clearTraceLevels(): Unit = {
    activeTraces.clear()
  }

  def addTraceLevel(v: VisualTraceLevel): Unit = v match {
    case  EnterExit =>
      activeTraces += EnterExit
    case  Checkpoint =>
      activeTraces ++= Seq(EnterExit, Checkpoint)
    case  JsonLogs =>
      activeTraces ++= Seq(EnterExit, JsonLogs)
    case  PrintLogs =>
      activeTraces ++= Seq(EnterExit, PrintLogs)
  }


}

trait VisualTracer extends EnableTrace { self =>
  import VisualTraceGlobals._

  lazy val tracer = self

  def isEnabled(v: VisualTraceLevel): Boolean = {
    activeTraces.contains(v)
  }

  def traceLevels(): Seq[VisualTraceLevel] = activeTraces.toSeq

  def tracingEnabled(): Boolean = {
    activeTraces.nonEmpty
  }

  def ifTrace(vtl: VisualTraceLevel)(body: => Unit): Unit = macro VisualTraceMacros.runOnTraceLevel[TraceLog]

}
