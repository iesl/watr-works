package edu.umass.cs.iesl.watr
package tracemacros

import scala.reflect.macros.blackbox.Context

sealed trait VisualTraceLevel

object VisualTraceLevel {
  case object EnterExit extends VisualTraceLevel
  case object Checkpoint extends VisualTraceLevel
  case object PrintLogs extends VisualTraceLevel
  case object AccumLogs extends VisualTraceLevel
  // case object Callback extends VisualTraceLevel

  // val all = List(Off, EnterExit, Checkpoint, Debug, Callback)

  // def cmp(a: VisualTraceLevel, b:VisualTraceLevel) = all.indexOf(b) - all.indexOf(a)
}


trait EnableTrace[T] {
  def traceLevel(): VisualTraceLevel
  def tracingEnabled(): Boolean
  def isEnabled(v: VisualTraceLevel): Boolean
  def traceLevels(): Seq[VisualTraceLevel]
  // def runTrace(level: VisualTraceLevel, ts: T*): Unit
  // def traceCallbacks: TraceCallbacksT

}

object VisualTraceMacros {
  type VTraceContext[S] = Context { type PrefixType = EnableTrace[S] }


  def printTrace[T](c: VTraceContext[T])(str: c.Expr[String]) = {
    import c.universe._
    q"""
    if (${c.prefix}.tracingEnabled()) {
       import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
       if( ${c.prefix}.isEnabled() )
       ${c.prefix}.traceLevel() match {
         case L.Print  => println($str)
         case _  =>
       }
    }
    """
  }

  def runOnTraceLevel[T](c: VTraceContext[T])(vtl: c.Expr[VisualTraceLevel])(body: c.Tree): c.Tree = {
    import c.universe._
    q"""
    if (${c.prefix}.tracingEnabled()) {

       val doTrace: Boolean = ${c.prefix}.traceLevels().contains(..$vtl)

       if (doTrace) {
          val _ = $body
       }
    }
    """
  }

  // def sideEffectIfEnabled[T](c: VTraceContext[T])(vtl: c.Expr[VisualTraceLevel])(body: c.Tree): c.Tree = {
  //   import c.universe._
  //   q"""
  //   if (${c.prefix}.tracingEnabled()) {
  //      import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}

  //      val doTrace: Boolean = true //  L.cmp(..$vtl, ${c.prefix}.traceLevel()) >= 0

  //      if (doTrace) {
  //         val _ = $body
  //      }
  //   }
  //   """
  // }

  // def materializeCallback[T](c: VTraceContext[T])(name: c.Expr[String])(value: c.Expr[Any]) = {
  // def materializeCallback[T](c: VTraceContext[T])(name: c.Expr[String])(value: c.Expr[Any]): c.Expr[Unit] = {

  // def checkpointImpl[T](c: VTraceContext[T])
  //   (loc: c.Expr[String], msg: c.Expr[String], args: c.Expr[Seq[Any]]) = {

  //   import c.universe._

  //   q"""
  //     val callbacks = ${c.prefix}.traceCallbacks()
  //     callbacks

  //   ; ()
  //   """

  // }

}



