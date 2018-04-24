package edu.umass.cs.iesl.watr
package utils

import org.scalatest._


class WindowedCursorTest extends FlatSpec with Matchers {
  behavior of "Cursors and Windows"


  it should "construct cursors" in {
    val ints = List(1, 2, 3, 4)
    val maybeCursor = Cursor.init(ints)
    assert(maybeCursor.isDefined)
    val cursor = maybeCursor.get

    assert(cursor.atStart)

    // assertResult(List(List(1, 2), List(3))){
    // }
  }

  behavior of "Grouping by Windows"

  it should "return windowed groups" in {
    val ints: List[Int] = List(1, 1, 1, 2, 2, 3)
    val groups = Cursors.groupByWindow[Int]({case (prevs, a) =>
      (a +: prevs).toSet.size == 1
    }, ints)
    println(s"groups = ${groups}")

    // assertResult(List(List(1, 1, 1), List(2, 2), List(3))){
    //   groups
    // }

  }

}
