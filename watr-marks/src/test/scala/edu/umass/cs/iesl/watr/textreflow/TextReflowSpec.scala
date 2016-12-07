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


  behavior of "text reflowing"


  it should "count atoms correctly" in {
    val Eu1_x = stringToTextReflow("Eu_{1 - x}")
    Eu1_x.charCount shouldBe 7
  }

  def annotateAndPrint(tr: TextReflow): Unit = {
    val ranges = tr.annotateCharRanges()
    val rbox = prettyPrintTree(tr)
    println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)
  }

  it should "annotate reflow with (begin, len) ranges over chars" in {
    // annotateAndPrint(stringToTextReflow("abc"))
    // annotateAndPrint(stringToTextReflow("a_{b}"))
    annotateAndPrint(stringToTextReflow("bc\naﬂﬆﬂ_{b}ﬂc"))


    // annotateAndPrint(stringToTextReflow("aﬂc"))
    // annotateAndPrint(stringToTextReflow("aﬂc"))
    // val abc = stringToTextReflow("""abc""")
    // val ranges = Eu1_x.annotateCharRanges()

    // val rbox = prettyPrintTree(Eu1_x)
    // println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)

    // cofreeAttrToTree(ranges).flatten.toList
    //   .map(coff => (coff.begin, coff.len))
    //   .shouldBe({
    //     List((0,7), (0,7), (0,7), (0,1), (1,1), (2,5), (2,5), (2,1), (3,1), (4,1), (5,1), (6,1))
    //   })
  }

  // behavior of "unicode char rewriting"

  // it should "handle replaced unicode -> ascii chars" in {
  //   val flavor = stringToTextReflow("""ﬂavor""")

  //   val rbox = prettyPrintTree(flavor)
  //   val ranges = flavor.annotateCharRanges()
  //   println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)

  //   flavor.charCount shouldBe 6
  // }

  // import spindex.{ComponentOperations => CO}
  // import textreflow.TextReflowRendering._

  // it should "join lines into single virtual line" in {
  //   val dict = utils.EnglishDictionary.fromWords("scanning")
  //   val ls = lines(
  //     """|of LiFePO4 scan-
  //        |ning electron
  //        |""".stripMargin
  //   )

  //   val textFlows = ls.map(l =>
  //     labeled(LB.VisualLine, flows(toAtoms(l)))
  //   )

  //   val joined = CO.joinTextLines(textFlows(0), textFlows(1))(dict)

  //   joined.toText() shouldBe {
  //     "of LiFePO4 scanning electron"
  //   }
  // }

  // it should "represent sup/sub" in {
  //   val f0 = flow(
  //     flows(toAtoms("Eu")),
  //     labeled(LB.Sub, flows(toAtoms("1 - x")))
  //   )
  //   f0.toText() shouldBe { "Eu1 - x" }
  //   f0.toFormattedText() shouldBe { "Eu_{1 - x}" }
  // }

  // it should "insert space into text" in {
  //   val textFlow = flows(
  //     lines("""1-x""")
  //       .map({l =>
  //         labeled(LB.VisualLine,
  //           flows(toAtoms(l))
  //         )}))

  //   val withSpace = everySequence(textFlow)({ seq =>
  //     seq.intersperse(space())
  //   })

  //   withSpace.toText() shouldBe { "1 - x" }

  // }





  //     val ffi = 0xFB03.toChar
  //     s"""| Eu1-xBixVO4 bar
  //         |      bbb  b  b
  //         | Eu_{1¿23;x}Bi_{x}VO_{4} bar
  //         | Fe^{+}_{x+1.0}O_{x}
  //         | ¿23;ﬂ bar
  //         | ﬂavor ﬆars e${ffi}cient bar
  //         | æon Æon bar
  //         |""".stripMargin

  //   it should "correctly compute string-match offsets" in {
  //   it should "join/break paragraph" in {}
  //   it should "grep-search virtual lines" in {}
  //   it should "define a repr for MIT-annots with context" in {}


}
