package edu.umass.cs.iesl.watr


import scalaz.syntax
import scalaz.syntax.std
import scalaz.Tag

trait ScalaZCommonOps
    extends syntax.ToIdOps
    with std.ToBooleanOps
    with std.ToOptionIdOps
    with std.ToOptionOps

    // with syntax.ToEqualOps
    // with std.ToListOps
    // with syntax.ToTraverseOps


trait PackageDefs extends ScalaZCommonOps {

  // val LB = watrmarks.StandardLabels
  // val TB = textboxing.TextBoxing

  type @@[A, B] = scalaz.@@[A, B]


  implicit class TagOps[A, T](val value: A@@T) {
    def unwrap: A = Tag.of[T].unwrap(value)
  }

}

