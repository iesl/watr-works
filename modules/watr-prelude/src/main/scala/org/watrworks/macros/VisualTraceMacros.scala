package org.watrworks
package tracemacros

import scala.reflect.macros.blackbox.Context

sealed trait VisualTraceLevel

object VisualTraceLevel {
  case object EnterExit extends VisualTraceLevel
  case object Checkpoint extends VisualTraceLevel
  case object PrintLogs extends VisualTraceLevel
  case object JsonLogs extends VisualTraceLevel
}

trait EnableTrace {
  def isEnabled(v: VisualTraceLevel): Boolean
  def tracingEnabled(): Boolean
  def traceLevels(): Seq[VisualTraceLevel]
}

object VisualTraceMacros {
  type VTraceContext = Context { type PrefixType = EnableTrace }


  def runOnTraceLevel(c: VTraceContext)(vtl: c.Expr[VisualTraceLevel])(body: c.Tree): c.Tree = {
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
