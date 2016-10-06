package edu.umass.cs.iesl.watr
package textflow

import spindex._

import scalaz._, Scalaz._
import matryoshka._,  Recursive.ops._ , TraverseT.ops._

import GeneralizedReflow._
import watrmarks.{StandardLabels => LB, _}

class GeneralizedReflowTest extends ConnectedComponentTestUtil {
  behavior of "component reflowing"

  import Reflow._


  def toAtoms(s: String): List[FixReflow] = {
    s.toList.map(_ match {
      case ' ' => space()
      case c   => atom(c)
    })
  }

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

    val linear = linearize(f0)

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
        val ggs = groups.map {_ match {
          case f @ Flow(_) => labeled(LB.Token, fixf(f)).unFix
          case x           => x
        }}

        ggs.toZipper.map { z =>
          z.start.modify { r: Reflow[Fix[Reflow]] =>
            labeled(LB.First, fixf(r)).unFix
          }.end.modify { r: Reflow[Fix[Reflow]] =>
            labeled(LB.Last, fixf(r)).unFix
          }.toList
        }.getOrElse{
          ggs
        }
      })
    })

    println(prettyPrintTree(tr0))


    // flatten the structure, and consider pairs w/ labels First, Last


  }


}
