package edu.umass.cs.iesl
package watr

import scalaz.Tag

object `package` extends PackageDefs {

  val SHA1String = Tag.of[SHA1String]

  val LB = watrmarks.StandardLabels

}
