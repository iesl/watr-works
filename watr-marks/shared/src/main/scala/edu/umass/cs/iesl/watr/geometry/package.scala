package edu.umass.cs.iesl.watr
package geometry

object `package` {

  val syntax = GeometryImplicits
  object zones {
    val data = ZoneTrees
    val syntax = ZoneTreeSyntax
  }
  type Zone = ZoneTrees.ZoneTree

}
