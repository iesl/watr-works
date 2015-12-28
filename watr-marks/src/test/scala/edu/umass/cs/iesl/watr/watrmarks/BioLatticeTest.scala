package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._


class BioLatticeSpec extends FlatSpec {

  import StandardLabels._

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



  behavior of "bio lattice"

  it should "unserialize from dom" in {
    val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)

    val lattice = BioLattice.initFromDom(doc)

    debugReport(lattice.showBox.padTop1)

  }


}
