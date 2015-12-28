package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._

class LatticeCursorSpec extends FlatSpec {

  import StandardLabels._
  behavior of "lattice cursors over chars"


  val runBrick =
    """|| |t~$T| {ns:pos, type: {token: t}, unit: char}
       |  >Run.<
       |""".stripMargin

  it should "navigate chars" in {

    val lspan = biolu.parseBioBrick(runBrick, bioDict, None, None, None)
    val lattice = BioLattice.initFromBrickColumns(lspan)
    val charCursor = lattice.initLatticeCursor(CharLabel)

    for {
      cc1 <- charCursor
      asdf  = cc1.showBox
      // _ = debug(cc1.showBox.padTop1)
      cc2 <- cc1.next
      // _ = debug(cc2.showBox.padTop1)
      cc3 <- cc2.next
      cc4 <- cc3.next
      // _ = debug(cc4.showBox.padTop1)
      cc5 <- cc4.prev
      cc6 <- cc5.prev
      cc7 <- cc6.prev
    } {
      assert(cc1 === cc7)
      assert(cc1.prev === None)
      assert(cc4.next === None)
    }
  }


  val svgStr = """|<svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
                  |    <g>
                  |      <text>
                  |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                  |bio="| |T t| {ns:tok, type: {token: t}, unit: char}"
                  |       >abc</tspan>
                  |      </text>
                  |      <text>
                  |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                  |bio="|t|~$t| {ns:tok, type: {token: t}, unit: char}"
                  |       >def</tspan>
                  |      </text>
                  |    </g>
                  |    <text>
                  |      <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                  |bio="|t|$T | {ns:tok, type: {token: t}, unit: char}"
                  |       >123</tspan>
                  |    </text>
                  |    <g>
                  |      <text>
                  |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                  |bio="| |TTT| {ns:tok, type: {token: t}, unit: char}"
                  |       >ghi</tspan>
                  |      </text>
                  |      <text>
                  |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                  |bio="| | t$| {ns:tok, type: {token: t}, unit: char}"
                  |       >jkl</tspan>
                  |      </text>
                  |    </g>
                  |  </svg>
                  |""".stripMargin



  it should "init from dom" in {
    val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)

    val lattice = BioLattice.initFromDom(doc)

    for {
      cc1 <-  lattice.initLatticeCursor(CharLabel)
      // _ = debug(cc1.showBox.padTop1)
      cc2 <- cc1.next
      // _ = debug(cc2.showBox.padTop1)
      cc3 <- cc2.next
      // _ = debug(cc3.showBox.padTop1)
      cc4 <- cc3.next
      // _ = debug(cc4.showBox.padTop1)
      cc5 <- cc4.prev
      cc6 <- cc5.prev
      cc7 <- cc6.prev
    } {
      assert(cc1.prev === None)
      assert(cc1 === cc7)
    }


  }

  behavior of "brick cursors over labels"

  it should "navigate labels" in {
    val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)

    val lattice = BioLattice.initFromDom(doc)

    for {
      cc1 <-  lattice.initLatticeCursor(Token)
      // _ = debug(cc1.showBox.padTop1)
      cc2 <- cc1.next
      // _ = debug(cc2.showBox.padTop1)
      cc3 <- cc2.next
      // _ = debug(cc3.showBox.padTop1)
      cc4 <- cc3.next
      // _ = debug(cc4.showBox.padTop1)
      cc5 <- cc4.next
      // _ = debug(cc5.showBox.padTop1)
      cc6 <- cc5.next
      // _ = debug(cc6.showBox.padTop1)
      cc7 <- cc6.next
      // _ = debug(cc7.showBox.padTop1)
      cc8 <- cc7.next
      // _ = debug(cc8.showBox.padTop1)
    } {
      assert(cc1.prev === None)
      assert(cc8.next === None)
    }
  }

  it should "add new label(s) to focus, change label focus" in {
    val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)
    val FooLabel          = BioLabel("foo", "foo")
    val BarLabel          = BioLabel("foo", "bar", 'b', FooLabel)

    val lattice = BioLattice.initFromDom(doc)

    for {
      cc1 <-  lattice.initLatticeCursor(Token)
      cc2 <- cc1.next
      _ = debug(cc2.showBox.padTop1)
      cc2l = cc2.addLabel(FooLabel)
      _ = debug("cc2 w/label", cc2l.showBox.padTop1)
      foo1 <- cc2l.initCursor(FooLabel)
      _ = debug("foo cursor from bar cursor", foo1.showBox.padTop1)
      foo1bar = foo1.addLabel(BarLabel)
      _ = debug("foo+bar", foo1bar.showBox.padTop1)
      cc3 <- cc2l.next
      cc4 <- cc3.next
      cc5 <- cc4.next
      cc6 <- cc5.next
      cc7 <- cc6.next
      cc8 <- cc7.next
      _ = debug(cc8.showBox.padTop1)
    } {
      assert(cc1.prev === None)
      assert(cc8.next === None)
    }
  }


  it should "widen/narrow labels" in {
    val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)
    val FooLabel          = BioLabel("foo", "foo")
    val BarLabel          = BioLabel("foo", "bar", 'b', FooLabel)

    val lattice = BioLattice.initFromDom(doc)

    for {
      cc1 <-  lattice.initLatticeCursor(Token)
      cc2 <- cc1.next
      _ = debug(cc2.showBox.padTop1)
    } {
    }

  }

}
