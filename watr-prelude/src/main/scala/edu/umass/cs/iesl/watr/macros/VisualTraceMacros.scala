package edu.umass.cs.iesl.watr
package utils

import scala.reflect.macros.blackbox.Context

sealed trait VisualTraceLevel
object VisualTraceLevel {
  case object Off extends VisualTraceLevel
  case object Append extends VisualTraceLevel
  case object Print extends VisualTraceLevel
}



trait EnableTrace[T] {
  def traceLevel(): VisualTraceLevel

  def tracingEnabled(): Boolean = traceLevel() != VisualTraceLevel.Off
  def runTrace(level: VisualTraceLevel, ts: T*): Unit
}

private object VisualTraceMacros {
  type VTraceContext[S] = Context { type PrefixType = EnableTrace[S] }

  def runIfEnabled[T](c: VTraceContext[T])(exprs: c.Expr[T]*) = {
    import c.universe._
    // q"if (${c.prefix}.tracingEnabled) { ${c.prefix}.runTrace(..$exprs) }"
    q"""
    if (${c.prefix}.tracingEnabled()) {
       import _root_.edu.umass.cs.iesl.watr.utils.{VisualTraceLevel => L}
       ${c.prefix}.traceLevel() match {
         case L.Off          => // noop
         case L.Append|L.Print => ${c.prefix}.runTrace(${c.prefix}.traceLevel(), ..$exprs)
       }
    }
    """
  }

}
