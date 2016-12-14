package edu.umass.cs.iesl.watr
package textreflow


// import watrmarks.{StandardLabels => LB}
// import matryoshka._
// import matryoshka.data._
// import matryoshka.implicits._
import spindex.EnrichGeometricFigures._

import utils.ScalazTreeImplicits._
import scalaz._
import Scalaz._

class TextReflowSpec extends StringReflowTestUtil {


  // behavior of "text reflowing"


  // val Eu1_x = stringToTextReflow("Eu_{1 - x}")

  // it should "count atoms correctly" in {
  //   Eu1_x.charCount shouldBe 7
  // }

  def annotateAndPrint(tr: TextReflow): Unit = {
    val ranges = tr.annotateCharRanges()
    val rbox = prettyPrintTree(tr)
    println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)
  }

  // it should "annotate reflow with (begin, len) ranges over chars" in {
  //   // annotateAndPrint(stringToTextReflow("bc\naﬂﬆﬂ_{b}ﬂc"))

  //   val ranges = Eu1_x.annotateCharRanges()

  //   // val rbox = prettyPrintTree(Eu1_x)
  //   // println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)

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

  // // import spindex.{ComponentOperations => CO}
  // // import textreflow.TextReflowRendering._

  // it should "join lines into single virtual line" in {
  //   val dict = utils.EnglishDictionary.fromWords("scanning")

  //   val ls = stringToTextReflow(
  //     """|of LiFePO4 scan-
  //        |ning electron
  //        |""".stripMargin
  //   )
  //   // val rbox = prettyPrintTree(ls)
  //   // println(rbox)

  //   // val textFlows = ls.map(l =>
  //   //   labeled(LB.VisualLine, flows(toAtoms(l)))
  //   // )

  //   // val joined = CO.joinTextLines(textFlows(0), textFlows(1))(dict)

  //   // joined.toText() shouldBe {
  //   //   "of LiFePO4 scanning electron"
  //   // }
  // }


  // it should "slice reflows" in {
  //   val reflow = stringToTextReflow("lime _{^{ﬂ}a}vor")
  //   // annotateAndPrint(reflow)
  //   // reflow.slice(6, 8).foreach{ tr =>
  //   //   val text = tr.toText()
  //   //   println(s"Slice:  $text")
  //   //   // annotateAndPrint(tr)
  //   // }

  //   for (i <- 0 to 11; j <- 0 to 11) {
  //     reflow.slice(i, j).foreach{ tr =>
  //       val text = tr.toText()
  //       println(s"($i, $j):  $text")
  //       // annotateAndPrint(tr)
  //     }
  //   }

  //   // reflow.slice(3, 6).foreach{ tr =>
  //   //   val text = tr.toText()
  //   //   println(s"Sliced: $text")
  //   //   annotateAndPrint(tr)
  //   // }

  //   // reflow.slice(0, 3).foreach{ tr =>
  //   //   val text = tr.toText()
  //   //   println(s"Sliced: $text")
  //   //   annotateAndPrint(reflow)
  //   // }


  //   // stringToTextReflow("ﬂavor").slice(0, 3).toText() shouldBe {
  //   //   "fla"
  //   // }

  // }

  it should "clip to target regions" in {
    val pageText = (
      """|a
         |e ^{ﬂ}
         |""".stripMargin)
    // val pageText = (
    //   """|abcdef
    //      |lime _{^{ﬂ}a}vored scan-
    //      |ning electron
    //      |""".stripMargin)
    // val pageText = (
    //   """|abc
    //      |def
    //      |""".stripMargin)

    val pageLines = lines(pageText)
    val reflow = stringToTextReflow(pageText)
    annotateAndPrint(reflow)

    for {
      (line, linenum) <- lines(pageText).zipWithIndex
      lineLen = line.length()
      maxlen = lines(pageText).map(_.length).max
      x1 <- 0 until maxlen
      height <- 1 to pageLines.length
      width <- x1+1 to (lineLen-x1)

    } {
      val bounds = s"$x1, $linenum, $width, $height"
      val tr = targetRegionForXY(x1, linenum, width, height)
      // println(s"clipping to ${tr.bbox.prettyPrint}")

      val res = reflow.clipToTargetRegion(tr)
      res.foreach { case (resReflow, range) =>
        val resText = resReflow.toText()
        // println(s"range ${range}: ${resText}  in bbox =${tr.bbox.prettyPrint} ")
        println(s"range ${range}: ${resText}   in ${bounds}")
        // annotateAndPrint(resReflow)
      }
    }

    // val reflow = stringToTextReflow("abcdef")

    // val y = 0

    // for (x <- 0 to 7; x2 <- 0 to 7 if x <= x2) {
    //   val tr = targetRegionForXY(x, y, x2-x, 1)
    //   println(s"clipping to ${tr.bbox.prettyPrint}")
    //   val res = reflow.clipToTargetRegion(tr)
    //   res.foreach { case (resReflow, range) =>
    //     println(s"in range ${range}: ")
    //     annotateAndPrint(resReflow)
    //   }
    // }

  }
  //   it should "join/break paragraph" in {}
  //   it should "grep-search virtual lines" in {}
  //   it should "define a repr for MIT-annots with context" in {}


}
