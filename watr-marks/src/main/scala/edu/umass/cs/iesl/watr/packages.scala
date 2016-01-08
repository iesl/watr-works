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

}
