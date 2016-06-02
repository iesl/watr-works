package edu.umass.cs.iesl
package watr

import scalaz.Tag
import scalaz.@@

object `package` extends PackageDefs {

  val SHA1String = Tag.of[SHA1String]

  val LB = watrmarks.StandardLabels

  implicit class TagOps[A, T](val value: A@@T) extends AnyVal {
    def unwrap: A = Tag.of[T].unwrap(value)
  }

}
