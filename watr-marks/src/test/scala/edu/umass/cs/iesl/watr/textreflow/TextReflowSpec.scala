package edu.umass.cs.iesl.watr
package textreflow

// import watrmarks.{StandardLabels => LB}
// import matryoshka._
// import matryoshka.data._
// import matryoshka.implicits._

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


  it should "slice reflows" in {
    val reflow = stringToTextReflow("_{^{ﬂ}a}vor")
    annotateAndPrint(reflow)

    reflow.slice(0, 3).map(_.toText()).foreach{t =>
      println(s"resulted in text $t")
    }


    // stringToTextReflow("ﬂavor").slice(0, 3).toText() shouldBe {
    //   "fla"
    // }

  }
  //   it should "join/break paragraph" in {}
  //   it should "grep-search virtual lines" in {}
  //   it should "define a repr for MIT-annots with context" in {}


}
