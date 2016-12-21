package edu.umass.cs.iesl.watr
package textreflow

import org.scalatest._
// import geometry.EnrichGeometricFigures._
import utils.ScalazTreeImplicits._

import scalaz._
import Scalaz._

class TextReflowSpec extends PlainTextReflow with FlatSpec with Matchers {

  def annotateAndPrint(tr: TextReflow): Unit = {
    val ranges = tr.annotateCharRanges()
    val rbox = prettyPrintTree(tr)
    println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)
  }

  def checkSlices(t: TextReflow): Unit = {
    val text = t.toText()

    for (i <- 0 to text.length; j <- i to text.length) {
      t.slice(i, j).foreach{ tr =>
        val sliceText = tr.toText()
        val expected = text.slice(i, j)
        // println(s"slice($i, $j)")
        // println(s"got: $sliceText")
        // println(s"exp: $expected")
        sliceText shouldBe expected
      }
    }
  }


  behavior of "modifying chars"

  it should "mod single char" in {
    val pageText = (
      """|a q1
         |e ^{ﬂ}
         |""".stripMargin)
    val pageLines = lines(pageText)
    val reflowLines = lines(pageText).map(stringToTextReflow(_))
    val reflowPage = stringToTextReflow(pageText)
    val reflowPageText = reflowPage.toText

    annotateAndPrint(reflowPage)


    for (i <- 0 until reflowPage.length) {
      println(s"@ $i")
      val modified = reflowPage
        .modifyCharAt(i)({(ch, index) =>
          println(s"mod char ${ch} ($index) ")

          Some("")
        })
      println(s"=> ${modified.toText()}")
    }

  }

  // behavior of "text reflowing"


  // val Eu1_x = stringToTextReflow("Eu_{1 - x}")

  // it should "count atoms correctly" in {
  //   Eu1_x.charCount shouldBe 7
  // }


  // it should "annotate reflow with (begin, len) ranges over chars" in {
  //   // annotateAndPrint(stringToTextReflow("bc\naﬂﬆﬂ_{b}ﬂc"))

  //   val ranges = Eu1_x.annotateCharRanges()

  //   cofreeAttrToTree(ranges).flatten.toList
  //     .map(coff => (coff.begin, coff.len))
  //     .shouldBe({
  //       List((0,7), (0,1), (1,1), (2,5), (2,5), (2,1), (3,1), (4,1), (5,1), (6,1))
  //     })
  // }

  // behavior of "unicode char rewriting"

  // it should "handle replaced unicode -> ascii chars" in {
  //   stringToTextReflow("ﬂavor").charCount shouldBe {
  //     6
  //   }
  // }

  // import spindex.{ComponentOperations => CO}

  // it should "join lines into single virtual line" in {
  //   val dict = utils.EnglishDictionary.fromWords("scanning")

  //   val textReflow1 = stringToTextReflow("PO_{4} scan-")
  //   val textReflow2 = stringToTextReflow("ning")

  //   val joined = CO.joinTextLines(textReflow1, textReflow2, force=true)(dict)
  //   val formatted = joined.applyLineFormatting()

  //   val rbox = prettyPrintTree(formatted)
  //   val ranges = formatted.annotateCharRanges()
  //   println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)

  //   val joinedText = formatted.toText()
  //   println(joinedText)
  //   checkSlices(formatted)
  // }

  // // it should "slice reflows" in {
  // //   val reflow = stringToTextReflow("lime _{^{ﬂ}a}vor")
  // // }
  // // val pageText = (
  // //   """|abcdef
  // //      |lime _{^{ﬂ}a}vored scan-
  // //      |ning electron
  // //      |""".stripMargin)
  // // val pageText = (
  // //   """|abc
  // //      |def
  // //      |""".stripMargin)


  // it should "clip to target regions" in {
  //   val pageText = (
  //     """|a q1
  //        |e ^{ﬂ}
  //        |""".stripMargin)
  //   val pageLines = lines(pageText)
  //   val reflow = stringToTextReflow(pageText)
  //   // annotateAndPrint(reflow)

  //   for {
  //     (line, y)       <- pageLines.zipWithIndex
  //     height          <- 1 to pageLines.length
  //     maxlen           = lines(pageText).map(_.length).max
  //     x               <- 0 until maxlen
  //     width           <- 1 until maxlen

  //   } {
  //     val bounds = s"x:$x, y:$y, w:$width, h:$height"
  //     val tr = targetRegionForXY(x, y, width, height)

  //     val res = reflow.clipToTargetRegion(tr)
  //     println(s"[$bounds]  ${tr.bbox.prettyPrint}")
  //     res.foreach { case (resReflow, range) =>
  //       val resText = resReflow.toText()
  //       // println(s"    ${resText}   ${range}")
  //       // annotateAndPrint(resReflow)
  //     }
  //   }
  // }

  //   it should "join/break paragraph" in {}
  //   it should "grep-search virtual lines" in {}
  //   it should "define a repr for MIT-annots with context" in {}


}
