package edu.umass.cs.iesl.watr
package tracing

import tracemacros._
import scala.language.experimental.macros

import geometry._
import textboxing.{TextBoxing => TB}, TB._

object VisualTracer {
  import TraceLog._

  implicit class RicherString(val s: String) extends AnyVal {
  }

  def message(s: TB.Box): TraceLog                      = {Message(s)}

  var visualTraceLevel: VisualTraceLevel = VisualTraceLevel.Off

}


class VisualTracer() {
  import VisualTracer._

  def traceLevel(): VisualTraceLevel = visualTraceLevel

  def tracingEnabled(): Boolean = {
    traceLevel() != VisualTraceLevel.Off //  && traceIsUnfiltered()
  }


  def apply(body: Unit): Unit = macro VisualTraceMacros.sideEffectIfEnabled[TraceLog]
  // def ifTrace(body: Unit): Unit = macro VisualTraceMacros.sideEffectIfEnabled[TraceLog]
  // def ifEnabled(body: Unit): Unit = macro VisualTraceMacros.sideEffectIfEnabled[TraceLog]

  def printTrace(str: String): Unit = macro VisualTraceMacros.printTrace[TraceLog]
  def printTrace(str: TB.Box): Unit = printTrace(str.toString())

  def visualTrace(body: Unit): Unit = macro VisualTraceMacros.sideEffectIfEnabled[TraceLog]

  def enter()(implicit enclosing: sourcecode.Name): Unit = apply {
    println(s"entered: ${enclosing.value}")
  }

  def exit()(implicit enclosing: sourcecode.Name): Unit = apply {
    println(s"exit: ${enclosing.value}")
  }

  def checkpoint(msg: String)(implicit enclosing: sourcecode.Name): Unit = apply {
    println(s"checkpoint@${enclosing.value}($msg)")
  }

}
