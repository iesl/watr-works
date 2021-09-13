package org.watrworks
package tracemacros

import scala.reflect.macros.blackbox.Context

trait EnableTrace {
  def vtraceEnabled(): Boolean
}

object VisualTraceMacros {
  type VTraceContext = Context { type PrefixType = EnableTrace }

  def ifTraceEnabled(c: VTraceContext)(body: c.Tree): c.Tree = {
    import c.universe._
    q"""
    if (${c.prefix}.vtraceEnabled()) {
      val _ = $body
    }
    """
  }

}
