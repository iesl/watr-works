package edu.umass.cs.iesl.watr
package utils

import scala.reflect.macros.blackbox.Context

trait EnableTrace[T] {
  def tracingEnabled(): Boolean
  def runTrace(ts: T*): Unit
}

private object VisualTraceMacros {
  type VTraceContext[S] = Context { type PrefixType = EnableTrace[S] }

  // def ifEnabled[T](c: VTraceContext)(func: c.Expr[() => Unit]) = {
  //   import c.universe._

  //   q"if (${c.prefix}.tracingEnabled) { ${func}() }"
  // }

  // def applyIfEnabled[T](c: VTraceContext)(f: c.Expr[Seq[T] => Unit])(exprs: c.Expr[T]*) = {
  //   import c.universe._

  //   q"if (${c.prefix}.tracingEnabled) { ${f} (..$exprs) }"
  // }

  def runIfEnabled[T](c: VTraceContext[T])(exprs: c.Expr[T]*) = {
    import c.universe._

    q"if (${c.prefix}.tracingEnabled) { ${c.prefix}.runTrace(..$exprs) }"
  }

}

