package edu.umass.cs.iesl.watr
package utils

import org.scalatest._

class TreeShaperTests extends FlatSpec with Matchers {
  import scalaz._
  import Scalaz._


  behavior of "tree shaper"

  behavior of "rose tree construction"

  val ts = TreeShaper[Int]

  it should "create a tree from a list of parent-child pairs" in  {

    val graphPairs = List[(ts.NodeType, ts.NodeType)](
      (0, 1),
      (0, 2),
      (1, 3),
      (1, 4),
      (2, 5)
    )

    val tree = ts.makeTreeFromPairs(graphPairs).head

    tree.drawTree shouldEqual (
      0.node(
        1.node(
          3.leaf,
          4.leaf
        ),
        2.node(
          5.leaf
        )
      ).drawTree
    )
  }


  it should "create distinct, unconnected trees" in {

    val graphPairs = List[(ts.NodeType, ts.NodeType)](
      (0, 1),
      (0, 2),
      (1, 3),
      (1, 4),
      (2, 5),
      (10, 11),
      (20, 21)
    )

    val trees = ts.makeTreeFromPairs(graphPairs)

    trees zip List(
      0.node(
        1.node(
          3.leaf,
          4.leaf
        ),
        2.node(
          5.leaf
        )
      )
    ) map {case (actual, expected) =>
        actual.drawTree shouldEqual expected.drawTree
    }


  }

  it should "create a right-slanting list" in {
    val graphPairs = List[(ts.NodeType, ts.NodeType)](
      (0, 1),
      (1, 2),
      (2, 3)
    )

    val tree = ts.makeTreeFromPairs(graphPairs).head

    tree.drawTree shouldEqual (
      0.node(
        1.node(
          2.node(
            3.leaf
          )
        )
      ).drawTree
    )

  }

  it should "not care about input order" in {
    val graphPairs = List[(ts.NodeType, ts.NodeType)](
      (1, 2),
      (0, 1),
      (2, 3)
    )

    val tree = ts.makeTreeFromPairs(graphPairs).head

    tree.drawTree shouldEqual (
      0.node(
        1.node(
          2.node(
            3.leaf
          )
        )
      ).drawTree
    )
  }

  it should "pass this bug-derived test" in {
    val input = List[(ts.NodeType, ts.NodeType)](
      (41, 42),
      (42, 43),
      (43, 44),
      (44, 45),
      (42, 47),
      (43, 48),
      (42, 49)
    )

    val trees = ts.makeTreeFromPairs(input)

    trees.head.flatten.toList shouldEqual 41.node(
      42.node(
        43.node(
          44.node(
            45.leaf
          ),
          48.leaf
        ),
        47.leaf,
        49.leaf
      )
    ).flatten.toList

  }

}
