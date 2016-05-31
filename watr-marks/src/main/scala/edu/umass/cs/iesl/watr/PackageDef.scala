package edu.umass.cs.iesl
package watr


// import scalaz.syntax.FoldableSyntax
import scalaz.syntax.ToIdOps
// import scalaz.syntax.ToValidationOps
import scalaz.syntax.std.ToBooleanOps
// import scalaz.syntax.std.ToListOps
import scalaz.syntax.std.ToOptionIdOps
import scalaz.syntax.std.ToOptionOps

import com.softwaremill.debug.DebugConsole
// import com.softwaremill.debug.DebugMacros

trait ScalaZCommonOps
    extends ToIdOps
    with ToBooleanOps
    with ToOptionIdOps
    with ToOptionOps
    // with ToValidationOps

trait PackageDefs
    extends ScalaZCommonOps
    with DebugConsole
