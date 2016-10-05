package edu.umass.cs.iesl.watr
package textflow

import spindex._

import scalaz._, Scalaz._
import matryoshka._,  Recursive.ops._ , TraverseT.ops._
import utils.ScalazTreeImplicits._

import GeneralizedReflow._

class GeneralizedReflowTest extends ConnectedComponentTestUtil {
  behavior of "component reflowing"

  import Reflow._

  s"""|foo Eu_{1-x}Bi_{x}VO_{4} bar
      |foo Eu_{1¿23;x}Bi_{x}VO_{4} bar
      |foo Fe^{+}_{x+1.0}O_{x}
      |foo ¿23;ﬂ bar-
      |food ﬂavor ﬆars e${ffi}cient bar
      |foo æon Æon bar
      |""".stripMargin



  it should "construct reflows" in {

    val a1 = atom('3')
    val a2 = atom('5')
    val a3 = atom('8')
    val f1 = flow(a1, a2)

    val f2 = flow(
      flow(a1, a2), f1, a1
    )

    val q1 = f2
    // q1.map { _0: Reflow[Fix[Reflow]] => G[Fix[G]] }

    val qTree = q1.cata(toTree).draw
    println(qTree)

  }

  it should "join lines into single virtual line" in {
    val ls = lines(
      """|of LiFePO4 scan-
         |ning electron
         |""".stripMargin
    )

    val textFlow = flows(ls.map({ l =>
      flows(l.map(atom(_)))
    }))

    // fold pairwise to decide how to join lines together

    val qTree = textFlow.cata(toTree).draw
    println(qTree)

  }


  // it should "represent a 'window' within a text flow" in {
  //   val atoms = """samples was set to 0.5 mol%. The nanopowder""".map(atom(_))
  //   val flow0 = flow(atoms:_*)
  //   val (pr, h, nx) =(
  //     "samples was set to".map(atom(_)),
  //     " 0.5 mol%.".map(atom(_)),
  //     "The nanopowder".map(atom(_))
  //   )
  // }



  it should "define a repr for MIT-annots with context" in {}
  it should "grep-search virtual lines" in {}

}
