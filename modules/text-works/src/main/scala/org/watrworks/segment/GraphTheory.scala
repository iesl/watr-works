package org.watrworks
package segment

import scalax.{collection => xc}
import xc.{mutable => xcm}
import xc.edge._
import xc.edge.Implicits._
import xc.GraphPredef._
// import xc.GraphEdge._

case class EdgeLabel(
  name: String
)

import scala.reflect._

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

  def createEdge(n1: OuterNodeT, n2: OuterNodeT, label: LabelType): OuterEdgeT

  def addEdge(
    n1: OuterNodeT,
    n2: OuterNodeT,
    label: LabelType
  ): Boolean = {
    val e = createEdge(n1, n2, label)
    graph.add(e)
  }

  def node(n: OuterNodeT): InnerNodeT = graph get n

  override def toString(): String = {
    graph.mkString("; ")
  }
}

class IntLGraph extends LabeledGraph[Int, LkDiEdge, EdgeLabel]() {

  def createEdge(n1: OuterNodeT, n2: OuterNodeT, label: EdgeLabel): OuterEdgeT = {
    (n1 ~+#> n2)(label)
  }

  def weaklyConnectedComponents() = {
    val sdf = graph.componentTraverser().topologicalSortByComponent()
    sdf.foreach({ cycleOrOrder: graph.CycleNodeOrTopologicalOrder =>

      cycleOrOrder match {
	    case Left(cycleNode) =>
          println(s"cycle=${cycleNode}")
        case Right(ordering) =>
          val asLayers: graph.LayeredTopologicalOrder[graph.NodeT] = ordering.toLayered
          for {
          (layerNum, nodes) <- asLayers
          } {
            println(s"""layer ${layerNum}=${nodes.mkString(", ")}""")

          }
      }

    })

    val wer = sdf.to(List)
    println(wer.mkString(" & "))

  }

}
