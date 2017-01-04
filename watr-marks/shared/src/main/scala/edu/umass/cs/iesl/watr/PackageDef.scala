package edu.umass.cs.iesl.watr


import scalaz.syntax
import scalaz.syntax.std
import scalaz.Tag
import scalaz.@@

trait ScalaZCommonOps
    extends syntax.ToIdOps
    with std.ToBooleanOps
    with std.ToOptionIdOps
    with std.ToOptionOps

trait PackageDefs
    extends ScalaZCommonOps with TypeTags {

  val LB = watrmarks.StandardLabels
  val TB = textboxing.TextBoxing


  implicit class TagOps[A, T](val value: A@@T) {
    def unwrap: A = Tag.of[T].unwrap(value)
  }

}

