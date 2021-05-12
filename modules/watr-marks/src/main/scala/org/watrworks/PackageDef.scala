package org.watrworks

import scalaz.syntax.std
import scalaz.Tag

trait ScalaZCommonOps extends std.ToBooleanOps with std.ToOptionIdOps with std.ToOptionOps

trait PackageDefs extends ScalaZCommonOps {

  type @@[A, B] = scalaz.@@[A, B]

  implicit class TagOps[A, T](val value: A @@ T) {
    def unwrap: A = Tag.of[T].unwrap(value)
  }

}
