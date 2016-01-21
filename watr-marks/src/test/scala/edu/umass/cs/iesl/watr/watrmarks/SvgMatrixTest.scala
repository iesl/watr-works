package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._

class SvgMatrixSpec extends FlatSpec {

  // import SvgMatrix._
  import dom._

  import StandardLabels._

  val svgstr =
    """|<?xml version="1.0"?>
       |<svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
       |  <g transform="matrix(1 0 0 -1 0 792)">
       |    <text transform="matrix(0 1 -1 0 32 256) scale(1, -1)">
       |      <tspan x="0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1" endX="100.2" y="0" font-size="20px">abcdefghijkl</tspan>
       |    </text>
       |    <text transform="translate(136.8 669.12) scale(1, -1)">
       |      <tspan x="0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83" endX="112.43" y="0" font-size="17.2154px">mnopqrstuvwxyz1</tspan>
       |      <tspan x="137.16 146.75 154.43 164.03 171.71 186.11" endX="191.22" y="19.91" font-size="17.2154px">234567</tspan>
       |    </text>
       |  </g>
       |</svg>
       |""".stripMargin

  behavior of "matrix ops"

  // it should "transform tspan leaf coords into absolute page coors" in {
  //   val doc = readWatrDom(new StringReader(svgstr), bioDict)
  //   val _ = for {
  //     tspan1 <- doc.toDomCursor.nextTSpan
  //     t1trans = getTransformedCoords(tspan1)
  //     _ = debug(t1trans)
  //     //     PositionGroup(
  //     //       List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
  //     //       32.0,
  //     //       List(536.0, 527.12, 520.46, 506.02, 500.46, 490.46, 484.9, 474.9, 464.9, 454.9, 444.9, 439.9)
  //     tspan2 <- tspan1.nextTSpan
  //     t2trans = getTransformedCoords(tspan2)
  //     _ = debug(t2trans)
  //     //       List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
  //     //       32.0,
  //     //       List(256.0, 264.88, 271.54, 285.98, 291.54, 301.54, 307.1, 317.1, 327.1, 337.1, 347.1, 352.1)
  //     tspan3 <- tspan2.nextTSpan
  //     t3trans = getTransformedCoords(tspan3)
  //     _ = debug(t3trans)
  //   } yield {}
  // }

  // "getTransformedCoords" should "raise an exception if the first argument is missing attributes y, endX, or x" in {}

  // it should "raise an exception if second argument is not an ancestor of the first" in {
  //   intercept[IllegalArgumentException] {
  //     getTransformedCoords(e_1_2_1, e_1_1)
  //   }
  // }

  // it should """return a PositionGroup if provided source element has attributes y, endX and x
  //              and the second argument is an ancestor of the first""" in {

  //   assertResult(
  //     PositionGroup(
  //       List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
  //       32.0,
  //       List(536.0, 527.12, 520.46, 506.02, 500.46, 490.46, 484.9, 474.9, 464.9, 454.9, 444.9, 439.9)
  //     )
  //   ) {
  //     getTransformedCoords(e_1_1_1, e)
  //   }

  //   assertResult(
  //     PositionGroup(
  //       List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
  //       32.0,
  //       List(256.0, 264.88, 271.54, 285.98, 291.54, 301.54, 307.1, 317.1, 327.1, 337.1, 347.1, 352.1)
  //     )
  //   ) {
  //     getTransformedCoords(e_1_1_1, e_1_1)
  //   }

  //   assertResult(
  //     PositionGroup(
 //       List(137.16, 146.75, 154.43, 164.03, 171.71, 186.11),
  //       191.22,
  //       List(19.91, 19.91, 19.91, 19.91, 19.91, 19.91)
  //     )
  //   ) {
  //     getTransformedCoords(e_1_2_2, e_1_2_2)
  //   }

  //   assertResult(
  //     PositionGroup(
  //       List(273.96000000000004, 283.55, 291.23, 300.83000000000004, 308.51, 322.91),
  //       328.02,
  //       List(142.79, 142.79, 142.79, 142.79, 142.79, 142.79)
  //     )
  //   ) {
  //     getTransformedCoords(e_1_2_2, e)
  //   }
  // }

  // it should "not make transformations for ancestral elements lacking transform attributes" in {
  //   assertResult(
  //     getTransformedCoords(e_1_2_2, e)
  //   ) {
  //     getTransformedCoords(e_1_2_2, e_1)
  //   }
  // }
  it should "rotate 90" in {
    // val m    = Matrix(1, 2,
    //                   3, 4)
    // val r90  = Matrix(2, 4,
    //                   1, 3)
    // val r180 = Matrix(4, 3,
    //                   2, 1)
    // assert(rot90(m) === r90)
    // assert(rot90(m, 2) === r180)
    // assert(rot90(m, 4) === m)
    // assert(rot90(m, -3) === r90)
    // assert(rot90(m, -2) === r180)
    }

}
