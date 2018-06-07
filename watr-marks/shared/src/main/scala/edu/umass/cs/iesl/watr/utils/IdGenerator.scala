package edu.umass.cs.iesl.watr
package utils

import scalaz.Tag

object IdGenerator {
  def apply[T](start: Int=1): IdGenerator[T] = new IdGenerator[T]{
    override def startingId: Int = start
  }
}

trait IdGenerator[T]  {
  val tagOf = Tag.of[T]

  def startingId: Int = 1

  var _nextId = startingId

  def reset(): Unit = { _nextId = startingId }

  def setNextId(i: Int): Unit = { _nextId = i }


  def nextId: Int @@ T = {
    val id = _nextId
    _nextId += 1
    tagOf(id)
  }

  def peekId: Int @@ T = {
    tagOf(_nextId)
  }
}
