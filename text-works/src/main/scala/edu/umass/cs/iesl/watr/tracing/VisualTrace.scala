package edu.umass.cs.iesl.watr
package tracing

import tracemacros._
import scala.language.experimental.macros
import geometry._

// import textboxing.{TextBoxing => TB}

object VisualTracer {
  import scala.collection.mutable
  import VisualTraceLevel._


  val activeTraces = mutable.HashSet[VisualTraceLevel]()

  def clearTraceLevels(): Unit = {
    activeTraces.clear()
  }

  def addTraceLevel(v: VisualTraceLevel): Unit = v match {
    case  EnterExit =>
      activeTraces += EnterExit
    case  Checkpoint =>
      activeTraces ++= Seq(EnterExit, Checkpoint)
    case  AccumLogs =>
      activeTraces ++= Seq(EnterExit, AccumLogs)
    case  PrintLogs =>
      activeTraces ++= Seq(EnterExit, PrintLogs)
  }


}


trait VisualTracer { self =>
  import VisualTracer._

  lazy val tracer = self

  def traceLevels(): Seq[VisualTraceLevel] = activeTraces.toSeq

  def tracingEnabled(): Boolean = {
    activeTraces.nonEmpty
  }

  def apply(body: => Unit): Unit = ifTrace(VisualTraceLevel.PrintLogs)(body)


  def ifTrace(vtl: VisualTraceLevel)(body: => Unit): Unit = macro VisualTraceMacros.runOnTraceLevel[TraceLog]

  def enter()(implicit enclosing: sourcecode.Name): Unit = ifTrace(VisualTraceLevel.EnterExit){
    println(s"entered: ${enclosing.value}")
  }

  def exit()(implicit enclosing: sourcecode.Name): Unit = ifTrace(VisualTraceLevel.EnterExit) {
    println(s"exit: ${enclosing.value}")
  }


  def checkpoint(msg: String, args: Any*)(implicit
    enclosing: sourcecode.Name,
    loc: sourcecode.Enclosing
  ): Unit = ifTrace(VisualTraceLevel.Checkpoint) {
    println(s"checkpoint@${enclosing.value}/${loc.value}:  '($msg)'")
  }

}
