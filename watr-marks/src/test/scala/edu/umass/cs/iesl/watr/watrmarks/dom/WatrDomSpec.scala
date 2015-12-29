package edu.umass.cs.iesl.watr
package watrmarks
package dom

import java.io.StringReader
import org.scalatest._

class WatrDomSpec extends FlatSpec {


  import StandardLabels._

  val svgStrNS = """|<?xml version="1.0"?>
                    |<svg:svg
                    | xmlns:svg="http://www.w3.org/2000/svg"
                    | xmlns:xlink="http://www.w3.org/1999/xlink"
                    | version="1.1"
                    | width="612px" height="13248px" viewBox="0 0 612 13248">
                    |  <svg:g transform="matrix(1 0 0 -1 0 828)">
                    |    <svg:defs/>
                    |    <svg:g transform="">
                    |      <svg:text transform="matrix(1.0308 0 0 1 103.44 762.96) scale(1, -1)" xml:space="preserve">
                    |        <svg:tspan font-family="TimesNewRomanMTExtra" font-size="8.64px" y="0" endX="40.94946" x="0 5.86898 10.0186 13.72758 18.33512 22.58842 29.35596 33.9635 38.6488" font-weight="bold" fill="rgb(0,0,0)">Pergamon </svg:tspan>
                    |      </svg:text>
                    |    </svg:g>
                    |  </svg:g>
                    |</svg:svg>
                    |""".stripMargin

  val svgStr1 = """| <svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
                   |   <g transform="matrix(1 0 0 -1 0 792)">
                   |     <text transform="matrix(0 1 -1 0 32 256) scale(1, -1)">
                   |       <tspan x="0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1" endX="100.2" y="0" font-size="20px">abcdefghijkl</tspan>
                   |     </text>
                   |     <text transform="translate(136.8 669.12) scale(1, -1)">
                   |       <tspan x="0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83" endX="112.43" y="0" font-size="17.2154px">mnopqrstuvwxyz1</tspan>
                   |       <tspan x="137.16 146.75 154.43 164.03 171.71 186.11" endX="191.22" y="19.91" font-size="17.2154px">234567</tspan>
                   |     </text>
                   |   </g>
                   | </svg>
                   |""".stripMargin



  behavior of "svg unserialization"

  it should  "understand xml with and without namespace prefixes" in {

    val doc = readWatrDom(new StringReader(svgStrNS), bioDict)
    // val doc2 = watrdom.read(new StringReader(svgStrNS.replaceAll("svg:", "")))
    // assert(svg2Dom === doc)
    // assert(svg2Dom === doc2)
  }

  import scalaz.Tree

  it should "parse an svg" in {
    val examples = List(
      ("""<svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168"> </svg>""",
        Svg(612, 3168, ViewBox(0, 0, 612, 3168))
      ),
      ("""<g transform="matrix(0 0 0 0 0 0)" ></g>""",
        Grp(List(Matrix(0, 0, 0, 0, 0, 0)))
      )
    )

    examples foreach { case(instr, expected)  =>
      val actual = readWatrDom(new StringReader(instr), bioDict)

      val elem = actual.toDomCursor.firstChild.get.getLabel

      assert(expected === elem)
    }

  }


  it should "unserialize any embedded annotations" in {
    val svgstr = """|<tspan
                    |  x="0 8.0"
                    |  endX="10.0"
                    |  y="0"
                    |  font-size="20px"
                    |  font-family="Times"
                    |  bio="
                    |    | |w$| {type: {word: w}, unit: char}"
                    |      >ab</tspan>
                    |""".stripMargin
    val doc = readWatrDom(new StringReader(svgstr), bioDict)
    // println(doc.prettyPrint)

    val otspan = doc.toDomCursor.firstChild
    assert(otspan.isDefined)

    val bioBrick = otspan.get.getLabel.asInstanceOf[TSpan].bioBrick

    assert(bioBrick.columns.length===2)

    val ls = List(Set(Word.B), Set(Word.L))
    bioBrick.columns.map(_.pins).zip(ls).foreach { case (l, lexpect) =>
      assert(l === lexpect)
    }

  }
  it should "create default annotations on tspans if none exist" in {
    val svgstr = """|<tspan
                    |  x="0 8.0"
                    |  endX="10.0"
                    |  y="0"
                    |  font-size="20px"
                    |  font-family="Times"
                    |  >ab</tspan>
                    |""".stripMargin
    val doc = readWatrDom(new StringReader(svgstr), bioDict)

    val otspan = doc.toDomCursor.firstChild
    assert(otspan.isDefined)

    val bioBrick = otspan.get.getLabel.asInstanceOf[TSpan].bioBrick

    assert(bioBrick.columns.length===2)

    val allLabels = bioBrick.columns.map(_.pins).reduce{_ ++ _}
    assert(allLabels.isEmpty)

  }

  it should "serialize to well-formed svg" in {
    val doc = readWatrDom(new StringReader(svgStrNS), bioDict)

    val s = doc.toSvg()
    println(s)



  }



}
