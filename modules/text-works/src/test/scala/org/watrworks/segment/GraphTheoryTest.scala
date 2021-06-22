package org.watrworks
package segment

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
    } graph.addEdge(n1, n2, EdgeLabel(l))

    println(
      graph.toString()
    )

    graph.weaklyConnectedComponents()

  }

}
