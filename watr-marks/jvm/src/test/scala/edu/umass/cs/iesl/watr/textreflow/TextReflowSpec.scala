package edu.umass.cs.iesl.watr
package textreflow

import org.scalatest._
import utils.ScalazTreeImplicits._
import scalaz._
import Scalaz._
import TextReflowF._
import TypeTags._
import corpora._
import data._
// import geometry.zones.syntax._

class TextReflowSpec extends FlatSpec with Matchers with CorpusTestingUtil {
  def createEmptyDocumentZoningApi(): DocumentZoningApi = new MemDocZoningApi

  initEmpty()

  val docs = List(
    List(
      "abc\ndef\nghi",
      "012\n345\n678"
    )
  )

  for { (doc, i) <- docs.zipWithIndex } {
    addDocument(DocumentID(s"doc#${i}"), doc)
  }

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

  it should "clip to target regions" in {
    val pageLines = docStore.getPageVisualLines(PageID(1))

    for {
      (line, i)       <- pageLines.zipWithIndex
      lineReflow       = docStore.getTextReflowForZone(line.id).get
      lineText         = lineReflow.toText
      lineTR           = lineReflow.targetRegion
      x               <- 0 until 3
      y               <- 0 until 3
      height          <- 1 to 2
      width           <- 1 to 2
    } {
      annotateAndPrint(lineReflow)
      // println(s">${lineText} in ${lineTR.bbox}")
      val bounds = getRegionBounds(x, y, width, height)

      val res = lineReflow.clipToBoundingRegion(bounds)
      // println(s"  clip to $bounds")
      res.foreach { case (resReflow, range) =>
        val resText = resReflow.toText()
        println(s"   => ${resText}  @${range} query:${bounds}")
      }
    }
  }
  behavior of "modifying chars"

  it should "mod single char" in {
    // addDocument(DocumentID("d#23"), List(
    //   """|a q1
    //      |e ^{ﬂ}
    //      |""".stripMargin
    // ))

    // val pageLines = lines(pageText)
    // val reflowLines = lines(pageText).map(stringToReflow(_))
    // val reflowPage = stringToReflow(pageText)

    // annotateAndPrint(reflowPage)


    // for (i <- 0 until reflowPage.length) {
    //   println(s"@ $i")
    //   val modified = reflowPage
    //     .modifyCharAt(i)({(ch, index) =>
    //       println(s"mod char ${ch} ($index) ")

    //       Some("")
    //     })
    //   println(s"=> ${modified.toText()}")
    // }

  }

  // behavior of "text reflowing"

  // val Eu1_x = stringToTextReflow("Eu_{1 - x}")
  val _ = """
scanning{ }of the...

    ﬂ
    fl
"""

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

  // it should "slice reflows" in {
  //   val reflow = stringToTextReflow("lime _{^{ﬂ}a}vor")
  // }
  // val pageText = (
  //   """|abcdef
  //      |lime _{^{ﬂ}a}vored scan-
  //      |ning electron
  //      |""".stripMargin)
  // val pageText = (
  //   """|abc
  //      |def
  //      |""".stripMargin)



  //   it should "join/break paragraph" in {}
  //   it should "grep-search virtual lines" in {}
  //   it should "define a repr for MIT-annots with context" in {}


}
