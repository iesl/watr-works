package edu.umass.cs.iesl
package watr

import scalaz.@@
import scalaz.Tag

object TagUtils {
  implicit class TagOps[A, T](val value: A@@T) extends AnyVal {
    def unwrap: A = Tag.of[T].unwrap(value)
  }

}
