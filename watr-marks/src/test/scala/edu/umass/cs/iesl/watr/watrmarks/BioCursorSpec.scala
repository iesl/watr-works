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



  it should "properly combine lists of brick+dom cursors" in {
    val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)
    val FooLabel = BioLabel("foo", "foo")

    val tspanCs = doc.toDomCursor
      .unfoldTSpansCursors
      .map({ tcur =>
          val bcur = tcur.getLabelAsTSpan
          .bioBrick
          .initBrickCursor(CharLabel).get

        // debug(bcur.showBox.padTop1)

        val bcurLabeled = bcur.addLabel(FooLabel)

        // debug(bcurLabeled.showBox.padTop1)

        tcur -> bcurLabeled
      })


    val head = tspanCs.head
    val tail = tspanCs.tail


    val combined = BioCursor.combineCursors(head, tail)

    combined.root
      .unfoldTSpansCursors
      .foreach ({ dcur =>
        val combinedBioBrick = dcur.getLabelAsTSpan.bioBrick

        debug(combinedBioBrick.showBox.padTop1)
      })

  }

  // it should "navigate chars across dom nodes" in {
  //   val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)

  //   debugReport(this.testNames.head)

  //   val _ = for {
  //     t1 <- doc.toCursor(Token)
  //     _ = debug(t1.showBox.padTop1)
  //     t2 <- t1.next
  //     _ = debug(t2.showBox.padTop1)
  //     t3 <- t2.next
  //     _ = debug(t3.showBox.padTop1)
  //   } yield {
  //     println("done")
  //   }

  // }


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
