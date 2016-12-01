package edu.umass.cs.iesl.watr
package textreflow

import watrmarks.{StandardLabels => LB}

class TextReflowTest extends StringReflowTestUtil {
  // import TextReflow._
  // import matryoshka._
  // import matryoshka.data._
  // import matryoshka.implicits._
  // import matryoshka.patterns.EnvT
  // import utils.ScalazTreeImplicits._
  // import scalaz._, Scalaz._

  import TextReflowOps._


  behavior of "text reflowing"

  def `Eu1-x` = flow(
    flows(toAtoms("Eu")),
    labeled(LB.Sub, flows(toAtoms("1 - x")))
  )


  it should "count atoms correctly" in {
    val f0 = flow(
      flows(toAtoms("Eu")),
      labeled(LB.Sub, flows(toAtoms("1 - x")))
    )
    val s = f0.charCount
    f0.charCount shouldBe 7
  }

  it should "annotate reflow with (begin, len) ranges over chars" in {
    val ranges = `Eu1-x`.annotateCharRanges()

    cofreeToTree(ranges).flatten.toList.map(_.cbegin) shouldBe {
      List(0, 0, 0, 1, 2, 2, 2, 3, 4, 5, 6)
    }
    // ranges.cata(deattribute[TextReflowF, CharOffsetState, TextReflow](f => fixf(f)))
  }

  it should "rewrite chars" in {

    // val RC = implicitly[Recursive.Aux[Cofree[TextReflowF, CharOffsetState], EnvT[CharOffsetState, TextReflowF, ?]]]
    // val rbox = RC.cata(ranges)(toTree).drawBox
    // println(rbox)


    val rewritten = `Eu1-x`.modifyCharAt(6)({case (c, i) =>
      Some("")
    })

    println(prettyPrintTree(rewritten))

  }

  import spindex.{ComponentOperations => CO}
  import textreflow.TextReflowRendering._

  it should "join lines into single virtual line" in {
    val dict = utils.EnglishDictionary.fromWords("scanning")
    val ls = lines(
      """|of LiFePO4 scan-
         |ning electron
         |""".stripMargin
    )

    val textFlows = ls.map(l =>
      labeled(LB.VisualLine, flows(toAtoms(l)))
    )

    val joined = CO.joinTextLines(textFlows(0), textFlows(1))(dict)

    joined.toText() shouldBe {
      "of LiFePO4 scanning electron"
    }
  }

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
