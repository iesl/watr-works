package edu.umass.cs.iesl.watr


import scalaz.syntax.ToIdOps
import scalaz.syntax.std.ToBooleanOps
import scalaz.syntax.std.ToOptionIdOps
import scalaz.syntax.std.ToOptionOps

trait ScalaZCommonOps
    extends ToIdOps
    with ToBooleanOps
    with ToOptionIdOps
    with ToOptionOps

trait PackageDefs
    extends ScalaZCommonOps {
}
