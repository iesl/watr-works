package edu.umass.cs.iesl.watr
package textflow


import watrmarks.{StandardLabels => LB}

class GeneralizedReflowTest extends StringReflowTestUtil {
  import TextReflow._
  // import TextReflowF._
  import matryoshka._
  import matryoshka.data._


  import scalaz._, Scalaz._
  import TextReflowOps._


  behavior of "component reflowing"



  // it should "count atoms correctly" in {
  //   val f0 = flow(
  //     flows(toAtoms("Eu")),
  //     labeled(LB.Sub, flows(toAtoms("1 - x")))
  //   )
  //   val s = f0.charCount
  //   f0.charCount shouldBe 7
  // }

  it should "annotate reflow with (begin, len) ranges over chars" in {
    // Eu1 - x
    // 0123456

    val f0 = flow(
      flows(toAtoms("Eu")),
      labeled(LB.Sub, flows(toAtoms("1 - x")))
    )

    // val resE = f0.annotateOffsets()
    val resStart = f0.annotateCharRanges()

    println(prettyPrintTree(f0))
    // println("offs")
    // println(printCofree(resE))
    println("starts")
    println(printCofree(resStart))

    val hidden = f0.modifyCharAtom(6)(hideChar)
    println(prettyPrintTree(hidden))

  }

  // import spindex.{ComponentOperations => CO}

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
  //     s"""foo Eu1-xBixVO4 bar
  //         |      bbb  b  b
  //         |foo Eu_{1¿23;x}Bi_{x}VO_{4} bar
  //         |foo Fe^{+}_{x+1.0}O_{x}
  //         |foo ¿23;ﬂ bar
  //         |foo ﬂavor ﬆars e${ffi}cient bar
  //         |foo æon Æon bar
  //         |""".stripMargin

  //   it should "correctly compute string-match offsets" in {
  //   it should "join/break paragraph" in {}
  //   it should "grep-search virtual lines" in {}
  //   it should "define a repr for MIT-annots with context" in {}


}
