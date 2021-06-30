package org.watrworks
package graphs

import scalax.{collection => xc}
import xc.edge._
import xc.edge.Implicits._

import utils._

case class EdgeLabel(
  name: String
)
object EdgeLabel {
  // def apply(s: String):
}

object IntLGraph {
  type G = LabeledGraph[Int, LkDiEdge, EdgeLabel]
}
class IntLGraph extends LabeledGraph[Int, LkDiEdge, EdgeLabel]() {

  def createOuterEdge(label: EdgeLabel, n1: OuterNodeT, n2: OuterNodeT): OuterEdgeT = {
    (n1 ~+#> n2)(label)
  }

  def weaklyConnectedComponents() = {
    val sdf = graph.componentTraverser().topologicalSortByComponent()
    sdf.foreach({ cycleOrOrder: graph.CycleNodeOrTopologicalOrder =>
      cycleOrOrder match {
        case Left(cycleNode) =>
          println(s"cycle=${cycleNode}")
        case Right(ordering) =>
          println(s"""Ordering = ${ordering} """)
          val asLayers: graph.LayeredTopologicalOrder[graph.NodeT] = ordering.toLayered
          for {
            (layerNum, nodes) <- asLayers
          } {
            println(s"""   l ${layerNum}= ${nodes.mkString(", ")}""")
          }
      }

    })
  }

}

class GraphOpTests extends SegmentationTestUtils {
  behavior of "graph creation/traversal operations"

  it should "create a graph and find connected components" in {

    val graph = new IntLGraph()

    val edges = List(
      (1, 10, "Down"),
      (10, 20, "Down"),
      (20, 30, "Down"),
      (25, 35, "Down"),
      (30, 35, "Right"),
    )

    for {
      (n1, n2, l) <- edges
    } graph.addEdge(EdgeLabel(l), n1, n2)

    println(
      graph.toString()
    )

    graph.weaklyConnectedComponents()

  }

}
