package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._


class BioCursorSpec extends FlatSpec {
  import StandardLabels._

  behavior of "bio brick cursors"
  val svgBrick1 =
    """| <svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
       |   <g transform="matrix(1 0 0 -1 0 792)">
       |     <text transform="translate(136.8 669.12) scale(1, -1)">
       |       <tspan
       |         x="0 11.51 20.15 25.91 33.59 43.19"
       |         endX="112.43"
       |         y="0"
       |         font-size="17.2154px"
       |         font-family="Times"
       |         bio="| |V   | {ns:pos, type: {verb: v}, unit: word} % | |w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}"
       |       >Run.</tspan>
       |     </text>
       |   </g>
       | </svg>
       |""".stripMargin

  // TextSpan functionality includes
  //  - chars (the actual text)
  //  - x,y position for each
  //  - font info: name/type/weight/height/etc
  //  - labels
  //    - serialize to/from brick format



  val brickTemplate =
    """| <svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
       |   <g transform="matrix(1 0 0 -1 0 792)">
       |     <text transform="translate(136.8 669.12) scale(1, -1)">
       |       <tspan endX="112.43" font-size="17.2154px" font-family="Times" y="0"
       |         x="XXX"
       |bio="BIO"
       |       >TEXT</tspan>
       |     </text>
       |   </g>
       | </svg>
       |""".stripMargin

  it should "navigate chars" in {

    val bioSamples = List(
      "    "
    )

    val bioline = """| |ZZZ| {ns:pos, type: {token: t}, unit: char}"""

    val text = "abcde".toList.zipWithIndex

    bioSamples  foreach { bio =>

      val bl = "ZZZ".r.replaceAllIn(bioline, bio)
      val xs = bio.zipWithIndex.map(_._2).mkString(" ")

      val svg =
        brickTemplate
          .replaceAll("BIO", bl)
          .replaceAll("XXX", xs)
          .replaceAll("TEXT", text.take(bio.length).map(_._1).mkString)
          .replaceAll("!", "\\$")

      // println(svg)

      val doc = dom.readWatrDom(new StringReader(svg), bioDict)
      val charCursor = doc.toCursor(CharLabel)
      charCursor.foreach{ cur =>
        println("cc: " + cur.getText)
      }
    }

  }

  // it should "navigate labels" in {

  //   val bioSamples = List(
  //     "t!  "
  //   )

  //   val bioline = """| |ZZZ| {ns:pos, type: {token: t}, unit: char}"""

  //   val text = "abcde".toList.zipWithIndex

  //   bioSamples  foreach { bio =>

  //     val bl = "ZZZ".r.replaceAllIn(bioline, bio)
  //     val xs = bio.zipWithIndex.map(_._2).mkString(" ")

  //     val svg =
  //       brickTemplate
  //         .replaceAll("BIO", bl)
  //         .replaceAll("XXX", xs)
  //         .replaceAll("TEXT", text.take(bio.length).map(_._1).mkString)
  //         .replaceAll("!", "\\$")

  //     // println(svg)

  //     val doc = dom.readWatrDom(new StringReader(svg), bioDict)
  //     val charCursor = doc.toCursor(CharLabel)
  //     println("cc1: " + charCursor.getText)
  //   }

  // }



}
