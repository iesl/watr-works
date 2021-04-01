package org.watrworks
package utils

class WindowedCursorTest extends WatrSpec {
  behavior of "Cursors and Windows"


  it should "construct cursors" in {
    val ints = List(1, 2, 3, 4)
    val maybeCursor = Cursor.init(ints)
    assert(maybeCursor.isDefined)
    val cursor = maybeCursor.get

    assert(cursor.atStart)

  }

  behavior of "Grouping by Windows"

  it should "return windowed groups" in {
    val ints: List[Int] = List(1, 1, 1, 2, 2, 3)

    val groups = Cursors.groupByWindow[Int]({case (prevs, a) =>
      (a +: prevs).toSet.size == 1
    }, ints)

    println(s"groups = ${groups}")

    assertResult(List(List(1, 1, 1), List(2, 2), List(3))){
      groups
    }

  }

  it should "not call function for singleton groups" in {
    val ints = List(1)

    var calls = 0
    val groups = Cursors.groupByWindow[Int]({case (_, _) =>
      calls = calls + 1
      false
    }, ints)

    println(s"calls = ${calls}")

    assertResult(List(List(1))){
      groups
    }

    assert( calls == 0 )
  }

}
