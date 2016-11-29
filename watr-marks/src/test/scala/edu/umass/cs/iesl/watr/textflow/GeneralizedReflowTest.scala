package edu.umass.cs.iesl.watr
package textflow

import watrmarks.{StandardLabels => LB}

class GeneralizedReflowTest extends StringReflowTestUtil {
  import TextReflow._
  import matryoshka._
  import matryoshka.data._
  // import matryoshka.implicits._
  import matryoshka.patterns.EnvT
  import utils.ScalazTreeImplicits._


  import scalaz._, Scalaz._
  import TextReflowOps._

  // "annotate and fold" >> {
  //   val RC = implicitly[Recursive.Aux[Cofree[Exp, Int], EnvT[Int, Exp, ?]]]

  //   val res = 100.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]])
  //   val rbox = boxTF[Fix[Exp], Exp](res)
  //   println(s"${rbox}")
  //   println(res.shows)

  //   val res2 = res.attributeTopDownM[State[Int, ?], Int](0)(sequential).eval(0)
  //   // val rbox2 = boxTF[Fix[Exp], Exp](res2)
  //   val rbox2 = RC.cata(res2)(toTree).drawBox
  //   println(s"${rbox2}")
  //   println(res2.shows)

  //   // val res3 = res2.cataM(liftTM(attributeElgotM[(Int, ?), Option](weightedEval)))
  //   val res3 = res2.cataM(liftTM(attributeElgotM[(Int, ?), Option](weightedEval)))

  //   res3.foreach { r =>
  //     // envT[Cofree](r)
  //     val cbox = RC.cata(r)(toTree).drawBox
  //     // val box = boxTF[Cofree[Exp, Int], Exp](r)
  //     println(s"${cbox}")
  //     println(r.shows)
  //   }
  // }

  behavior of "component reflowing"

  def `Eu1-x` = flow(
    flows(toAtoms("Eu")),
    labeled(LB.Sub, flows(toAtoms("1 - x")))
  )


  // it should "count atoms correctly" in {
  //   val f0 = flow(
  //     flows(toAtoms("Eu")),
  //     labeled(LB.Sub, flows(toAtoms("1 - x")))
  //   )
  //   val s = f0.charCount
  //   f0.charCount shouldBe 7
  // }

  it should "annotate reflow with (begin, len) ranges over chars" in {
    val ranges = `Eu1-x`.annotateCharRanges()

    val RC = implicitly[Recursive.Aux[Cofree[TextReflowF, CharOffsetState], EnvT[CharOffsetState, TextReflowF, ?]]]
    val rbox = RC.cata(ranges)(toTree).drawBox
    println(rbox)

    // boxTF[Cofree[TextReflowF, CharOffsetState], TextReflowF](ranges)
    println(ranges.shows)

    // val hidden = `Eu1-x`.modifyCharAt(6)(hideChar)

    // val rewritten = `Eu1-x`.modifyCharAt(6)(
    //   (c: Char, index: Int)  => None: Option[Char]
    // )

  }

  // import spindex.{ComponentOperations => CO}
  // import ComponentReflow._

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
