package edu.umass.cs.iesl.watr
package spindex


// import watrmarks.{StandardLabels => LB}
// import textboxing.{TextBoxing => TB}, TB._
// import org.typelevel.discipline.specs2.mutable._


import scalaz._, Scalaz._

import GeneralizedReflow._
// import matryoshka._

class GeneralizedReflowTest extends ConnectedComponentTestUtil {
  behavior of "component reflowing"

  val rTraverse = implicitly[Traverse[Reflow]]

  s"""|foo Eu_{1-x}Bi_{x}VO_{4} bar
      |foo Eu_{1¿23;x}Bi_{x}VO_{4} bar
      |foo Fe^{+}_{x+1.0}O_{x}
      |foo ¿23;ﬂ bar-
      |food ﬂavor ﬆars e${ffi}cient bar
      |foo æon Æon bar
      |""".stripMargin

  """|of LiFePO4 scan-
     |ning electron
     |""".stripMargin

  it should "construct reflows" in {

    val atoms = "great blue heron".map(atom(_))
    val gbhFlow = flow(atoms:_*)

    // rTraverse

    val qwer = rTraverse.map(gbhFlow)({
      case Reflow.Atom(a) => atom(a.toString()+"!")
      case x => x
    })

    println(s"qwer: ${qwer}")


  }

  it should "grep-search virtual lines" in {
    val atoms = """samples was set to 0.5 mol%. The nanopowder""".map(atom(_))
    val flow0 = flow(atoms:_*)
    val context = ???

  }
  it should "define a repr for MIT-annots with context" in {}

}
