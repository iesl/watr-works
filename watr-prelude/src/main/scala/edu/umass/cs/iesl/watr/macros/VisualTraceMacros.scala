package edu.umass.cs.iesl.watr
package tracemacros

import scala.reflect.macros.blackbox.Context

sealed trait VisualTraceLevel

object VisualTraceLevel {
  case object Off extends VisualTraceLevel
  case object TraceLevel1 extends VisualTraceLevel
  case object TraceLevel2 extends VisualTraceLevel
  case object TraceLevel3 extends VisualTraceLevel
  case object Append extends VisualTraceLevel
  case object Print extends VisualTraceLevel
}


trait EnableTrace[T] {
  def traceLevel(): VisualTraceLevel

  def tracingEnabled(): Boolean
  def runTrace(level: VisualTraceLevel, ts: T*): Unit
}

object VisualTraceMacros {
  type VTraceContext[S] = Context { type PrefixType = EnableTrace[S] }

  def runIfEnabledWithCondition[T](c: VTraceContext[T])(cond: c.Expr[Boolean])(exprs: c.Expr[T]*) = {
    import c.universe._
    q"""
    if (${c.prefix}.tracingEnabled() && $cond) {
       import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
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
       import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
       ${c.prefix}.traceLevel() match {
         case L.Off          => // noop
         case _              => ..$body
       }
    }
    """
  }


  // def runLevel[T](c: VTraceContext[T])(level: c.Expr[VisualTraceLevel])(body: c.Expr[Unit]) = {
  //   import c.universe._
  //   q"""
  //   if (${c.prefix}.tracingEnabled()) {
  //      import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
  //      ${c.prefix}.traceLevel() match {
  //        case L.Off          => // noop
  //        case L.Tracelevel1  => // noop
  //        case _              => ..$body
  //      }
  //   }
  //   """
  // }
  def runIfEnabled[T](c: VTraceContext[T])(exprs: c.Expr[T]*) = {
    import c.universe._
    // q"if (${c.prefix}.tracingEnabled) { ${c.prefix}.runTrace(..$exprs) }"
    q"""
    if (${c.prefix}.tracingEnabled()) {
       import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
       ${c.prefix}.traceLevel() match {
         case L.Off          => // noop
         case L.Append|L.Print => ${c.prefix}.runTrace(${c.prefix}.traceLevel(), ..$exprs)
       }
    }
    """
  }

}
