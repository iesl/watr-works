package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._


class BrickCursorSpec extends FlatSpec {

  import StandardLabels._

  behavior of "brick cursors over chars"

  val runBrick =
    """|| |t~$T| {ns:pos, type: {token: t}, unit: char}
       |  >Run.<
       |""".stripMargin

  it should "navigate chars" in {

    val lspan = biolu.parseBioBrick(runBrick, bioDict, None, None, None)
    val charCursor = lspan.initBrickCursor(CharLabel)

    val cc1 = charCursor.get
    val cc2 = cc1.next.get
    val cc3 = cc2.next.get
    val cc4 = cc3.next.get
    val cc5 = cc4.prev.get
    val cc6 = cc5.prev.get
    val cc7 = cc6.prev.get

    assert(cc1 === cc7)
    assert(cc1.prev === None)
    assert(cc4.next === None)

    // println(s"${cc1}")
    // println(s"${cc2}")
    // println(s"${cc3}")
    // println(s"${cc4}")
    // println(s"${cc5}")
    // println(s"${cc6}")
    // println(s"${cc7}")

  }

  behavior of "brick cursors over labels"

  it should "navigate labels" in {
    val lspan = biolu.parseBioBrick(runBrick, bioDict, None, None, None)
    val charCursor = lspan.initBrickCursor(Token)

    val cc1 = charCursor.get
    val cc2 = cc1.next.get
    val cc3 = cc2.prev.get

    assert(cc1 === cc3)
    assert(cc3.prev === None)
    assert(cc2.next === None)


    // println(s"${cc1}")
    // println(s"${cc2}")
    // println(s"${cc3}")
    // println(s"${cc4}")

  }

  behavior of "partial bricks cursors over labels"

  val fullBrick =
    """|| |t~~~~~$ | {ns:pos, type: {token: t}, unit: char}
       |  >Running.<
       |""".stripMargin

  val partialBrickBegin =
    """|| |t~~~| {ns:pos, type: {token: t}, unit: char}
       |  >Runn<
       |""".stripMargin

  val partialBrickEnd =
    """||t|~~$ | {ns:pos, type: {token: t}, unit: char}
       |  >ing.<
       |""".stripMargin


  it should "parse half labels correctly" in {
    val full = biolu.parseBioBrick(fullBrick, bioDict, None, None, None)
    val begin = biolu.parseBioBrick(partialBrickBegin, bioDict, None, None, None)
    val end = biolu.parseBioBrick(partialBrickEnd, bioDict, None, None, None)

    val fullCur = full.initBrickCursor(Token).get
    val beginCur = begin.initBrickCursor(Token).get
    val endCur = end.initBrickCursor(Token).get

    assert(fullCur.coversCompleteLabel)
    assert(fullCur.coversStartOfLabel)
    assert(fullCur.coversEndOfLabel)

    assert(!beginCur.coversCompleteLabel)
    assert(beginCur.coversStartOfLabel)
    assert(!beginCur.coversEndOfLabel)

    assert(!endCur.coversCompleteLabel)
    assert(!endCur.coversStartOfLabel)
    assert(endCur.coversEndOfLabel)

    assert(fullCur.current == beginCur.current ++ endCur.current)
    assert(fullCur.prevs == beginCur.prevs ++ endCur.prevs)
    assert(fullCur.nexts == beginCur.nexts ++ endCur.nexts)

  }


}
