package edu.umass.cs.iesl.watr.shell

import org.scalatest._

import java.io.StringReader
import scala.collection.mutable

class CursorSpec extends FlatSpec {
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




  val expected = WDocument(List(
    WSvg(List(
      WGrp(List(
        WText(List(
          WTSpan(WChars("abcdefghijkl")))),
        WText(List(
          WTSpan(WChars("mnopqrstuvwxyz1")),
          WTSpan(WChars("234567"))))))))))

  val expected2 = WDocument(List(
    WSvg(List(
      WGrp(List(WDefs(List()),
        WGrp(List(WText(List(WTSpan(WChars("Pergamon "))))))))))))

  behavior of "svg unserialization"

  it should  "understand xml with and without namespace prefixes" in {

    val doc = wdocuments.readWDocument(
      new StringReader(svgStrNS)
    )
    val doc2 = wdocuments.readWDocument(
      new StringReader(svgStrNS.replaceAll("svg:", ""))
    )

    assert(expected2 === doc)
    assert(expected2 === doc2)
  }

  behavior of "svg dom cursor"

  it should "convert dom to/from cursor" in {
    // val wcursor = wdoc.cursor ===...
  }
  it should "traverse elements via next" in {
    // val wcursor =...
    // wcursor.next
    // wcursor.next[WGrp]
    // wcursor.stream[WTspan]
  }

  // it should "" in {}

}
