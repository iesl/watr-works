package edu.umass.cs.iesl.watr
package utils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class DisjointSetsTest extends AnyFlatSpec with Matchers {

  behavior of  "DisjointSet"

  it should "add/union" in {
    val ds = DisjointSet(1 to 6: _*)

    ds union (2,3)
    ds union (4,5)
    ds union (5,6)

    // println(s" (5,6) \n${ds.toString()} ")

    // No longer throws assertion when duplicate member is added, just skips insertion
    // an [AssertionError] should be thrownBy (ds union (0, 6))

    ds(1) shouldBe 1
    ds(2) shouldBe 2
    ds(3) shouldBe 2
    ds(4) shouldBe 4
    ds(5) shouldBe 4
    ds(6) shouldBe 4
    ds.sets should contain allOf (Set(1), Set(2,3), Set(4,5,6))
  }

  behavior of  "OrderedDisjointSet"

  it should "add/union" in {
    val ds = OrderedDisjointSet(1 to 6: _*)
    an [AssertionError] should be thrownBy (ds add 1)

    ds union (2,3)
    ds union (4,5)
    ds union (5,6)

    an [AssertionError] should be thrownBy (ds union (0, 6))

    ds(1) shouldBe 1
    ds(2) shouldBe 2
    ds(3) shouldBe 2
    ds(4) shouldBe 4
    ds(5) shouldBe 4
    ds(6) shouldBe 4

    ds(6) shouldEqual 4

    an [AssertionError] shouldBe thrownBy (ds(7))


  }

  it should "preserve order and allow reordering" in {
    val ds = OrderedDisjointSet(1 to 6: _*)

    ds union (2,3)
    ds union (4,5)
    ds union (5,6)

    ds.sets should contain allOf (List(1), List(2,3), List(4,5,6))

    ds.at(2).reorderBy(i => -i)
    ds.at(5).reorderWith(_ > _)

    ds.sets should contain allOf (List(1), List(3, 2), List(6, 5, 4))
  }
  it should "preserve ordering with incremental additions" in {
    val ds = OrderedDisjointSet[Int]()



    ds.add(0)
    (1 to 6).foreach {n =>
      ds.add(n)
      ds union (0, n)

    }


    ds.sets should contain (List(0, 1, 2, 3, 4, 5, 6))

  }

}
