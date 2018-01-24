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

}
