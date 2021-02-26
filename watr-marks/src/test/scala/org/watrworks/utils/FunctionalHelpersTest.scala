package org.watrworks
package utils

import FunctionalHelpers._

class FunctionalHelpersTest extends WatrSpec {
  behavior of "Function helper routines"


  it should "collect contiguous spans" in {
    val examples = List(
      List(1, 2, 3, 4, 5) -> List(Left(List(1, 2, 3)), Right(List(4, 5))),
      List(10, 20, 1, 2, 30) -> List(Right(List(10, 20)), Left(List(1, 2)), Right(List(30))),
    )
    examples.foreach(ex => {
      val in = ex._1
      val exp = ex._2

      val res = collectSpanEither(in, (n: Int) => n > 3)

      assert( res == exp )
    })

  }
}
