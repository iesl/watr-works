package edu.umass.cs.iesl.watr
package textreflow

import spindex._

import scalaz._, Scalaz._

import watrmarks.{StandardLabels => LB}

class StringReflowTestUtil extends ConnectedComponentTestUtil {

  import TextReflowF._
  import matryoshka.implicits._

  def toAtoms(s: String): List[TextReflow] = {
    s.toList.map(_ match {
      case ' ' => space()
      case c   => atom(c, new TextReflowAtomOps(Seq(c)))
    })
  }

  def makeTestSample(): TextReflow = {

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
        case (Atom(a, aops), Atom(b, bops), i) => true
        case qq => false
      }, onGrouped = {groups =>

        val tokenGroups = groups.map {_ match {
          case f @ Flow(as) => addLabel(LB.Token)(fixf(f)).unFix
          case x            => x
        }}

        tokenGroups.toZipper.map (
          _.start.modify { v => addLabel(LB.First)(fixf(v)).unFix }
            .end.modify { v => addLabel(LB.Last)(fixf(v)).unFix }
            .toList)
          .getOrElse(tokenGroups)

      }).embed
    })
    tr0
  }

}
