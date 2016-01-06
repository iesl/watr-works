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



    for {
      cc1 <- charCursor
      asdf  = cc1.showBox
      // _ = debug(cc1.showBox.padTop1)
      cc2 <- cc1.next
      // _ = debug(cc2.showBox.padTop1)
      cc3 <- cc2.next
      cc4 <- cc3.next
      // _ = debug(cc4.showBox |> boxlf)
      cc5 <- cc4.prev
      cc6 <- cc5.prev
      cc7 <- cc6.prev
    } {
      assert(cc1 === cc7)
      assert(cc1.prev === None)
      assert(cc4.next === None)
    }


  }

  behavior of "brick cursors over labels"

  it should "navigate labels" in {
    val lspan = biolu.parseBioBrick(runBrick, bioDict, None, None, None)
    val charCursor = lspan.initBrickCursor(Token)

    for {
      cc1 <- charCursor
      // _ = debug(cc1.showBox.padTop1)
      cc2 <- cc1.next
      // _ = debug(cc2.showBox.padTop1)
      cc3 <- cc2.prev
      // _ = debug(cc3.showBox.padTop1)
    } {
      assert(cc1 === cc3)
      assert(cc3.prev === None)
      assert(cc2.next === None)
    }
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

    // debug(fullCur.showBox.padTop1)
    // debug(beginCur.showBox.padTop1)
    // debug(endCur.showBox.padTop1)

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
