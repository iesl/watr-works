package edu.umass.cs.iesl.watr
package tracemacros

import scala.reflect.macros.blackbox.Context

sealed trait VisualTraceLevel

object VisualTraceLevel {
  case object EnterExit extends VisualTraceLevel
  case object Checkpoint extends VisualTraceLevel
  case object PrintLogs extends VisualTraceLevel
  case object JsonLogs extends VisualTraceLevel
  // case object Callback extends VisualTraceLevel

  // val all = List(Off, EnterExit, Checkpoint, Debug, Callback)

  // def cmp(a: VisualTraceLevel, b:VisualTraceLevel) = all.indexOf(b) - all.indexOf(a)
}

trait EnableTrace {
  def isEnabled(v: VisualTraceLevel): Boolean
  def tracingEnabled(): Boolean
  def traceLevels(): Seq[VisualTraceLevel]
}

object VisualTraceMacros {
  type VTraceContext = Context { type PrefixType = EnableTrace }


  // def printTrace[T](c: VTraceContext)(str: c.Expr[String]) = {
  //   import c.universe._
  //   q"""
  //      import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
  //      val doPrint: Boolean = ${c.prefix}.traceLevels().contains(L.PrintLogs)

  //      if( doPrint ) {

  //      }
  //   """
  // }

  // def tracedImpl[T](c: VTraceContext)(body: c.Tree): c.Tree = {
  //   import c.universe._
  //   q"""
  //      {
  //        import _root_.edu.umass.cs.iesl.watr.tracemacros.{VisualTraceLevel => L}
  //        if( ${c.prefix}.isEnabled(L.EnterExit) ) {
  //          enter();
  //          val a = ${body};
  //          exit();
  //          a;
  //        } else { ${body} }
  //     }
  //   """
  // }

  def runOnTraceLevel[T](c: VTraceContext)(vtl: c.Expr[VisualTraceLevel])(body: c.Tree): c.Tree = {
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
