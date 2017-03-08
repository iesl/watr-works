package edu.umass.cs.iesl.watr.utils

import scalaz.@@
import scalaz.Tag

object IdGenerator {
  def apply[T](start: Int=1): IdGenerator[T] = new IdGenerator[T]{
    override def startingId:Int = start
  }
}

trait IdGenerator[T]  {
  val tagOf = Tag.of[T]

  def startingId:Int = 1

  def reset(): Unit = { _nextId = startingId }

  var _nextId = startingId
  def nextId: Int @@ T = {
    val id = _nextId
    _nextId += 1
    tagOf(id)
  }
}
