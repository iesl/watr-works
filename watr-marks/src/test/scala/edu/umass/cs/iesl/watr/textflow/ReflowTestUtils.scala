package edu.umass.cs.iesl.watr
package textflow

import spindex._

import scalaz._, Scalaz._
// import matryoshka._

import watrmarks.{StandardLabels => LB}

class StringReflowTestUtil(
  val genReflow: GeneralizedReflow[Char]
) extends ConnectedComponentTestUtil {

  import genReflow._
  import Reflow._

  def toAtoms(s: String): List[Reflow] = {
    s.toList.map(_ match {
      case ' ' => space()
      case c   => atom(c)
    })
  }

  def makeTestSample(): Reflow = {

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

    val tr0 = everyLabel(LB.VisualLine, textFlow)(reflow =>{
      groupByPairs(reflow.unFix)({
        case (Atom(a), Atom(b), i) => true
        case qq => false
      }, onGrouped = {groups =>

        val tokenGroups = groups.map {_ match {
          case f @ Flow(ls, as) => addLabel(LB.Token)(f)
          case x                => x
        }}

        tokenGroups.toZipper.map (
          _.start.modify { addLabel(LB.First) }
            .end.modify { addLabel(LB.Last) }
            .toList)
          .getOrElse(tokenGroups)

      }).embed
    })
    tr0
  }

}
