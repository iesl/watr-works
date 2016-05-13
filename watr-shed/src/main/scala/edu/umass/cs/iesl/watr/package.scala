package edu.umass.cs.iesl

import scalaz.Tag

package object watr {

  val SHA1String = Tag.of[SHA1String]

  val LB = watrmarks.StandardLabels

}
