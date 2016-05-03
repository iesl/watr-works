package edu.umass.cs.iesl.watr
package watrmarks

import scalaz.{Tag, @@}

object IdGenerator {
  def apply[T]: IdGenerator[T] = new IdGenerator[T]{}
}

trait IdGenerator[T] {
  val tagOf = Tag.of[T]

  def startingId = 0

  var _nextId = startingId
  def nextId: Int @@ T = {
    val id = _nextId
    _nextId += 1
    tagOf(id)
  }
}
