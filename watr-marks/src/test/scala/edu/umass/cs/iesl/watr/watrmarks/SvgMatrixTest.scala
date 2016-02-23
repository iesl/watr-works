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

  val svgstr2 =
    """|<?xml version="1.0"?>
       |<svg:svg xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:svg="http://www.w3.org/2000/svg" version="1.1" width="595.276px" height="3968.505px" viewBox="0 0 595.276 3968.505">
       |  <svg:g transform="matrix(1 0 0 -1 0 793.701)">
       |    <svg:defs>
       |    </svg:defs>
       |    <svg:g transform="">
       |      <svg:text transform="matrix(6.376 0 0 6.376 241.6818 750.8975) scale(1, -1)" xml:space="preserve">
       |        <svg:tspan font-size="1px" y="0" endX="16.06149999999999" x="0 0.52 1.082 1.373 1.664 2.257 2.5875 3.1075 3.4715 3.9924 4.3564 4.8764 5.2106 5.5126 6.0746 6.6786 6.9696 7.4376 7.8746 8.2072 8.7792 9.3512 9.9232 10.2612 10.7082 11.2802 11.8522 12.4242 12.9962 13.4332 13.7735 14.3455 14.9175 15.4895">Solid State Ionics 181 (2010) 1415</svg:tspan>
       |        <svg:tspan font-size="1px" y="0" endX="16.6392" x="16.0672" spelling="endash">&#x2013;</svg:tspan>
       |        <svg:tspan font-size="1px" y="0" endX="18.9243" x="16.6363 17.2083 17.7803 18.3523">1419</svg:tspan>
       |      </svg:text>
       |    </svg:g>
       |  </svg:g>
       |</svg:svg>
       |""".stripMargin
  behavior of "matrix ops"

  it should "transform tspan leaf coords into absolute page coors" in {
    val doc = readWatrDom(new StringReader(svgstr2), bioDict)
      doc.toDomCursor.unfoldTSpansCursors.take(1).foreach({ domCursor =>
        domCursor.loc.path
        // get all transforms leading to this tspan
        val transforms = domCursor.loc
          .path.reverse.toList
          .flatMap{ _ match {
            case t: Transformable => t.transforms
            case _  => List()
          }}
        val tspan = domCursor.getLabel.asInstanceOf[dom.TSpan]

        println(s"""tspan: ${tspan.text}, trans = ${transforms.mkString("\n", "\n", "\n~")}""")

        val mFinal = transforms.foldLeft(Matrix.idMatrix)({
          case (acc, e) =>
            acc.multiply(e.toMatrix)
        })


        tspan.textXYOffsets.foreach { offsets =>
          val y = offsets.ys(0)
          val ff = tspan.fontFamily
          offsets.xs.foreach { x =>
            val tvec = mFinal.transform(watrmarks.Vector(x, y))
            println(s"processing x,y=$x, $y ==> ${tvec.x}, ${tvec.y}")
          }
        }
      })
  }

  "getTransformedCoords" should "raise an exception if the first argument is missing attributes y, endX, or x" in {}


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
