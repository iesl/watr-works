package edu.umass.cs.iesl

import com.softwaremill.debug.DebugConsole
import scalaz.syntax.ToIdOps
import scalaz.syntax.std.ToBooleanOps
  // import scalaz.syntax.std.ToOptionOps
  // import scalaz.syntax.std.ToOptionIdOps
  // import scalaz.syntax.std.ToListOps
  // import scalaz.syntax.FoldableSyntax
  // import scalaz.syntax.ToValidationOps


trait ScalaZCommonOps
    extends ToIdOps
    with ToBooleanOps
// with ToOptionIdOps
// with ToOptionOps


package object watr
    extends DebugConsole
    with ScalaZCommonOps {

  def time[R](prefix: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"${prefix}: " + (t1 - t0)/1000000.0d + "ms")
    result
  }


}
