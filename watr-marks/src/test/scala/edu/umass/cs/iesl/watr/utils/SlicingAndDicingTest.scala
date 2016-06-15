package edu.umass.cs.iesl.watr
package utils

import org.scalatest._


class SlicingAndDicingTest extends FlatSpec with Matchers {
  behavior of "list clustering/partitioning utils"

  import SlicingAndDicing._


  it should "split lists on pairwise functions" in {
    assertResult(List(List(1), List(2), List(3))){
      List(1, 2, 3).splitOnPairs((e1, e2) => e1 < e2)
    }
    assertResult(List(List(3))){
      List(3).splitOnPairs((e1, e2) => true)
    }

    assertResult(Seq(Seq())){
      Seq.empty[Int].splitOnPairs((e1, e2) => true)
    }

    assertResult(List(List(1, 2), List(3))){
      List(1, 2, 3).splitOnPairs((e1, e2) => e2 > 2)
    }
  }


  it should "do greedy clustering on lists" in {

    val hasSharedLetter: ((String, String) => Boolean) = (s1, s2) => {
      !(s1.toSet intersect s2.toSet).isEmpty
    }

    assertResult(List[List[Int]]()){
      List.empty[Int].clusterBy((_:Int, _:Int) => false)
    }

    val examples = List(
      (hasSharedLetter, "abc abd def ghi", List("abc abd", "def", "ghi")),
      (hasSharedLetter, "abc qwer abd ef ri", List("abc abd", "qwer ef ri")),
      (hasSharedLetter, "qwe abd ef ri", List("qwe ef", "abd", "ri"))
    )

    examples.foreach { case (fn, inp, out) =>
      val in = inp.split(" ").toList
      val expect = out.map(_.split(" ").toList).toList

      assertResult(expect){
        in.clusterBy(fn)
      }

    }

  }

}
