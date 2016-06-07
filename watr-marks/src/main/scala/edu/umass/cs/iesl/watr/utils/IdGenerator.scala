package edu.umass.cs.iesl.watr
package utils 

import scalaz.{Tag, @@}

object IdGenerator {
  // def apply[T]: IdGenerator[T] = new IdGenerator[T]{}
  def apply[T](start: Int=0): IdGenerator[T] = new IdGenerator[T]{ override val startingId = start }


  // def GenInts[T](start: Int=0): IntIdGenerator[T] = new IntIdGenerator[T]{ override val startingId = start }
  // def GenLongs[T](start: Int=0): LongIdGenerator[T] = new LongIdGenerator[T]{ override val startingId = start }
}

trait IdGenerator[T]  {
  val tagOf = Tag.of[T]

  def startingId = 0

  var _nextId = startingId
  def nextId: Int @@ T = {
    val id = _nextId
    _nextId += 1
    tagOf(id)
  }
}



// trait IdGenerator[T <: @@[_, _]] {
// }

// trait IdGeneratorX[N <: Numeric[N], T] extends IdGenerator[N@@T] {
//   val tagOf = Tag.of[T]

//   def startingId: Long = 0

//   var _nextId = startingId
//   def nextId: Long @@ T = {
//     val id = _nextId
//     _nextId += 1
//     tagOf(id)
//   }
// }

// trait IdGenerator[N <: Numeric[N], T] {
//   val tagOf = Tag.of[T]

//   def startingId: Long = 0

//   var _nextId = startingId
//   def nextId: Long @@ T = {
//     val id = _nextId
//     _nextId += 1
//     tagOf(id)
//   }
// }


// trait LongIdGenerator[T] extends IdGenerator[T] {
//   val tagOf = Tag.of[T]

//   def startingId: Long = 0

//   var _nextId = startingId
//   def nextId: Long @@ T = {
//     val id = _nextId
//     _nextId += 1
//     tagOf(id)
//   }
// }


