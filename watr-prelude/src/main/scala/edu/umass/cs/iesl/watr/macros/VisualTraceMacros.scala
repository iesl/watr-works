package edu.umass.cs.iesl.watr
package tracing

import scala.reflect.macros.blackbox.Context

sealed trait VisualTraceLevel
object VisualTraceLevel {
  case object Off extends VisualTraceLevel
  case object Append extends VisualTraceLevel
  case object Print extends VisualTraceLevel
}



trait EnableTrace[T] {
  def traceLevel(): VisualTraceLevel

  // def tracingEnabled(): Boolean = traceLevel() != VisualTraceLevel.Off
  def tracingEnabled(): Boolean
  def runTrace(level: VisualTraceLevel, ts: T*): Unit
}

object VisualTraceMacros {
  type VTraceContext[S] = Context { type PrefixType = EnableTrace[S] }

  def runIfEnabledWithCondition[T](c: VTraceContext[T])(cond: c.Expr[Boolean])(exprs: c.Expr[T]*) = {
    import c.universe._
    q"""
    if (${c.prefix}.tracingEnabled() && $cond) {
       import _root_.edu.umass.cs.iesl.watr.tracing.{VisualTraceLevel => L}
       ${c.prefix}.traceLevel() match {
         case L.Off          => // noop
         case L.Append|L.Print => ${c.prefix}.runTrace(${c.prefix}.traceLevel(), ..$exprs)
       }
    }
    """
  }

  def sideEffectIfEnabled[T](c: VTraceContext[T])(body: c.Expr[Unit]) = {
    import c.universe._
    q"""
    if (${c.prefix}.tracingEnabled()) {
       import _root_.edu.umass.cs.iesl.watr.tracing.{VisualTraceLevel => L}
       ${c.prefix}.traceLevel() match {
         case L.Off          => // noop
         case _              => ..$body
       }
    }
    """
  }
  def runIfEnabled[T](c: VTraceContext[T])(exprs: c.Expr[T]*) = {
    import c.universe._
    // q"if (${c.prefix}.tracingEnabled) { ${c.prefix}.runTrace(..$exprs) }"
    q"""
    if (${c.prefix}.tracingEnabled()) {
       import _root_.edu.umass.cs.iesl.watr.tracing.{VisualTraceLevel => L}
       ${c.prefix}.traceLevel() match {
         case L.Off          => // noop
         case L.Append|L.Print => ${c.prefix}.runTrace(${c.prefix}.traceLevel(), ..$exprs)
       }
    }
    """
  }

}

object Deshugar {

  import scala.reflect.macros.Context
  import scala.reflect.runtime.universe._
  import scala.language.experimental.macros

  def desugarImpl(c : Context)(expr : c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    println(show(expr.tree))
    reify {}
  }

  def desugar(expr : Any) = macro desugarImpl

}
