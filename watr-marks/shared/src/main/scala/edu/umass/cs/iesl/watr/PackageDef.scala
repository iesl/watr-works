package edu.umass.cs.iesl.watr


import scalaz.syntax
import scalaz.syntax.std

trait ScalaZCommonOps
    extends syntax.ToIdOps
    with std.ToBooleanOps
    with std.ToOptionIdOps
    with std.ToOptionOps

trait PackageDefs
    extends ScalaZCommonOps {
}

