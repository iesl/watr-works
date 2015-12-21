package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._


class BioCursorSpec extends FlatSpec {
  import StandardLabels._

  behavior of "bio cursors (brick+dom cursor)"

  val svgStr = """|<svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
                   |    <g>
                   |      <text>
                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                   |bio="| |TTT| {ns:tok, type: {token: t}, unit: char}"
                   |       >abc</tspan>
                   |      </text>
                   |      <text>
                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                   |bio="| |TTT| {ns:tok, type: {token: t}, unit: char}"
                   |       >def</tspan>
                   |      </text>
                   |    </g>
                   |    <text>
                   |      <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                   |bio="| |TTT| {ns:tok, type: {token: t}, unit: char}"
                   |       >ghi</tspan>
                   |    </text>
                   |    <g>
                   |      <text>
                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                   |bio="| |TTT| {ns:tok, type: {token: t}, unit: char}"
                   |       >ghi</tspan>
                   |      </text>
                   |      <text>
                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
                   |bio="| |TTT| {ns:tok, type: {token: t}, unit: char}"
                   |       >jkl</tspan>
                   |      </text>
                   |    </g>
                   |  </svg>
                   |""".stripMargin


  it should "navigate chars across dom nodes" in {
      val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)
      val charCursor = doc.toCursor(CharLabel)


  }


}

// val bioSamples = List(
//   "    "
// )

// val bioline = """| |ZZZ| {ns:pos, type: {token: t}, unit: char}"""

// val text = "abcde".toList.zipWithIndex

// bioSamples  foreach { bio =>

//   val bl = "ZZZ".r.replaceAllIn(bioline, bio)
//   val xs = bio.zipWithIndex.map(_._2).mkString(" ")

//   val svg =
//     brickTemplate
//       .replaceAll("BIO", bl)
//       .replaceAll("XXX", xs)
//       .replaceAll("TEXT", text.take(bio.length).map(_._1).mkString)
//       .replaceAll("!", "\\$")

//   // println(svg)

//   val doc = dom.readWatrDom(new StringReader(svg), bioDict)
//   val charCursor = doc.toCursor(CharLabel)
//   charCursor.foreach{ cur =>
//     println("cc: " + cur.getText)
//   }
// }
