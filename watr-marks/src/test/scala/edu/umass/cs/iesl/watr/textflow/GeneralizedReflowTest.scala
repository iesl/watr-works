package edu.umass.cs.iesl.watr
package textflow

import spindex._

import scalaz._, Scalaz._
import matryoshka._,  Recursive.ops._ , TraverseT.ops._

import GeneralizedReflow._, Reflow._
import watrmarks.{StandardLabels => LB, _}


class GeneralizedReflowTest extends ReflowTestUtil {
  behavior of "component reflowing"

  import Reflow._



  it should "represent sup/sub" in {
    // e.g., "Eu_{1-x}Bi_{x}VO_{4}"

    val labeler = new Labeler()
    val sub = labeler.getLabel(LB.Sub)

    val f0 = flow(
      flows(toAtoms("Eu")),
      anchor(sub.B),
      flows(toAtoms("1 - x")),
      anchor(sub.L)
    )

    // println("Tree-like")
    // println(prettyPrintTree(f0))

    // val linear = linearize(f0)
    // linear

    // println("Line-like")
    // println(prettyPrintTree(linear))

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

    val tr0 = transLabeled(textFlow, LB.VisualLine, reflow =>{
      groupByPairs(reflow)({
        case (Atom(a), Atom(b), i) => true
        case qq => false
      }, onGrouped = {groups =>

        val tokenGroups = groups.map {_ match {
          case f @ Flow(labels, as) if as.length > 1 =>
            addLabel(LB.Token)(f)
          case x                => x
        }}

        tokenGroups.toZipper.map (
          _.start.modify { addLabel(LB.First) }
            .end.modify { addLabel(LB.Last) }
            .toList)
          .getOrElse(tokenGroups)

      })
    })

    println(prettyPrintTree(tr0))

    val tr1 = unFlow(unFlow(unFlow(tr0)))
    println(prettyPrintTree(tr1))

    // val linear = linearize(tr0.unFix)
    // println("Line-like")
    // println(prettyPrintTree(fixf(linear)))


    // flatten the structure, and consider pairs w/ labels First, Last


  }


}
