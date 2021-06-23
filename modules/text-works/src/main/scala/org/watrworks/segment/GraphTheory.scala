package org.watrworks
package segment

import scalax.{collection => xc}
import xc.{mutable => xcm}
import xc.edge._
import xc.edge.Implicits._
import xc.GraphPredef._
import scala.reflect._
import cats.instances.seq

case class EdgeLabel(
  name: String
)

abstract class LabeledGraph[NodeType, EdgeType[+X] <: EdgeLikeIn[X], LabelType](implicit
  CT: ClassTag[EdgeType[NodeType]]
) {

  // import xc.edge.LBase.LEdgeImplicits
  // object MyImplicit extends LEdgeImplicits[EdgeLabel]
  // import MyImplicit._

  def emptyGraph(): xcm.Graph[NodeType, EdgeType] = xcm.Graph[NodeType, EdgeType]()
  val graph                                       = emptyGraph()

  type InnerEdgeT = graph.EdgeT
  type InnerNodeT = graph.NodeT
  type OuterEdgeT = EdgeType[NodeType]
  type OuterNodeT = NodeType

  protected def createOuterEdge(n1: OuterNodeT, n2: OuterNodeT, label: LabelType): OuterEdgeT

  def addEdge(
    n1: OuterNodeT,
    n2: OuterNodeT,
    label: LabelType
  ): Boolean = {
    val e = createOuterEdge(n1, n2, label)
    graph.add(e)
  }

  def node(n: OuterNodeT): InnerNodeT = graph get n

  override def toString(): String = {
    graph.mkString("; ")
  }
}

class IntLGraph extends LabeledGraph[Int, LkDiEdge, EdgeLabel]() {

  def createOuterEdge(n1: OuterNodeT, n2: OuterNodeT, label: EdgeLabel): OuterEdgeT = {
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

case class LabeledEdgeShapes(
  name: String,
  shape1: AnyShape,
  shape2: AnyShape
)

class ShapeIDGraph extends LabeledGraph[Int @@ ShapeID, LkDiEdge, LabeledEdgeShapes]() {

  def createOuterEdge(n1: OuterNodeT, n2: OuterNodeT, label: LabeledEdgeShapes): OuterEdgeT = {
    (n1 ~+#> n2)(label)
  }

  def edge(n1: AnyShape, n2: AnyShape, label: String): Boolean =
    addEdge(n1.id, n2.id, LabeledEdgeShapes(label, n1, n2))

  type LayeredOrdering = graph.LayeredTopologicalOrder[graph.NodeT]
  type TopoOrdering = graph.TopologicalOrder[graph.NodeT]

  def weaklyConnectedComponents(): Seq[TopoOrdering] = {
    val sorted = graph.componentTraverser().topologicalSortByComponent()
    val iter = sorted.flatMap({ cycleOrOrder: graph.CycleNodeOrTopologicalOrder =>
      cycleOrOrder match {
        case Left(cycleNode @ _) =>
          List()
        case Right(ordering) =>
          List(ordering)
      }
    })
    iter.to(List)
  }

}
