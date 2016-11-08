package edu.umass.cs.iesl.watr
package textflow


import watrmarks.{StandardLabels => LB, Labeler}

class GeneralizedReflowTest extends StringReflowTestUtil {
  import TextReflow._
  import ComponentReflow._

  behavior of "component reflowing"
  // import matryoshka._
  // import Recursive.ops._
  // import TraverseT.ops._

  import scalaz.std.list.listSyntax._

  it should "represent sup/sub" in {
    // e.g., "Eu_{1-x}Bi_{x}VO_{4}"

    val labeler = new Labeler()
    val sub = labeler.getLabel(LB.Sub)

    val f0 = flow(
      flows(toAtoms("Eu")),
      labeled(LB.Sub, flows(toAtoms("1 - x")))
    )
    // println("Tree-like")
    // println(prettyPrintTree(f0))
    println(s"toText: ${f0.toText()}")
    println(s"toFormattedText: ${f0.toFormattedText()}")
  }

  it should "insert space into text" in {
    val textFlow = flows(
      lines("""1-x""")
        .map({l =>
          labeled(LB.VisualLine,
            flows(toAtoms(l))
          )}))

    val withSpace = everySequence(textFlow)({ seq =>
      seq.intersperse(space())
    })

    // println("post")
    // println(prettyPrintTree(withSpace))
    println(s"toText: ${withSpace.toText()}")
    println(s"toFormattedText: ${withSpace.toFormattedText()}")
  }


  it should "join lines into single virtual line" in {
    val ls = lines(
      """|of LiFePO4 scan-
         |ning electron
         |""".stripMargin
    )

    val textFlow = flows(
      ls.map(l =>
        labeled(LB.VisualLine, flows(toAtoms(l)))
      ))

    // println(prettyPrintTree(textFlow))
    println(s"toText: ${textFlow.toText()}")
    println(s"toFormattedText: ${textFlow.toFormattedText()}")

    //

  }
  //   val tr0 = transLabeled(textFlow, LB.VisualLine, reflow =>{
  //     groupByPairs(reflow)({
  //       case (Atom(a), Atom(b), i) => true
  //       case qq => false
  //     }, onGrouped = {groups =>

  //       val tokenGroups = groups.map {_ match {
  //         case f @ Flow(labels, as) if as.length > 1 =>
  //           addLabel(LB.Token)(f)
  //         case x                => x
  //       }}

  //       tokenGroups.toZipper.map (
  //         _.start.modify { addLabel(LB.First) }
  //           .end.modify { addLabel(LB.Last) }
  //           .toList)
  //         .getOrElse(tokenGroups)

  //     })
  //   })

  //   println(prettyPrintTree(tr0))

  //   val tr1 = unFlow(unFlow(unFlow(tr0)))
  //   println(prettyPrintTree(tr1))

  //   // val linear = linearize(tr0.unFix)
  //   // println("Line-like")
  //   // println(prettyPrintTree(fixf(linear)))


  //   // flatten the structure, and consider pairs w/ labels First, Last


  // }


}
