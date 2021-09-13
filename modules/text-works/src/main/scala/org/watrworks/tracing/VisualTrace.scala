package org.watrworks
package tracing

import tracemacros._

trait VisualTracer extends EnableTrace { self =>

  final def vtraceEnabled(): Boolean = true

  def ifTrace(body: => Unit): Unit = macro VisualTraceMacros.ifTraceEnabled

}
