package org.watrworks
package graphs

import scalax.{collection => xc}
import xc.{mutable => xcm}
import xc.edge._
import xc.edge.Implicits._
import xc.GraphPredef._

import scala.reflect._

abstract class CustomGraph[NodeType, EdgeType[+X] <: EdgeLikeIn[X]](implicit
  CT: ClassTag[EdgeType[NodeType]]
) {
  def EdgeTypeClassTag: ClassTag[EdgeType[NodeType]] = CT

  def emptyGraph(): xcm.Graph[NodeType, EdgeType] =
    xcm.Graph.empty[NodeType, EdgeType]

  val graph = emptyGraph()

  type InnerEdgeT = graph.EdgeT
  type InnerNodeT = graph.NodeT
  type OuterEdgeT = EdgeType[NodeType]
  type OuterNodeT = NodeType
  type EdgeFilter = InnerEdgeT => Boolean
  type NodeFilter = InnerNodeT => Boolean

  def addEdge(e: OuterEdgeT): Boolean = {
    graph.add(e)
  }

  def findEdge(n1: OuterNodeT, n2: OuterNodeT): Option[OuterEdgeT] = {
    val matching = for {
      inner1 <- graph.find(n1).to(List)
      edge   <- inner1.edges
      if edge._2.toOuter == n2
    } yield edge.toOuter

    // println(s"    findEdge(${n1}, ${n2}) = ${matching}")
    matching.headOption

  }

  def upsertEdge(
    f: Option[OuterEdgeT] => OuterEdgeT,
    n1: OuterNodeT,
    n2: OuterNodeT
  ): Boolean = {
    // println(s"upsertEdge(${n1}, ${n2})")
    val eold    = findEdge(n1, n2)
    val newEdge = f(eold)
    val didInsert = graph.upsert(newEdge)
    // findEdge(n1, n2)
    // val dbgEdge = graph.find(newEdge)
    // println(s"   findByEdge: ${dbgEdge}")

    didInsert
  }

  def node(n: OuterNodeT): InnerNodeT = graph get n

  override def toString(): String = {
    graph.mkString("; ")
  }
}

abstract class LabeledGraph[NodeType, EdgeType[+X] <: EdgeLikeIn[X], LabelType](implicit
  CT: ClassTag[EdgeType[NodeType]]
) {
  def EdgeTypeClassTag: ClassTag[EdgeType[NodeType]] = CT

  def emptyGraph(): xcm.Graph[NodeType, EdgeType] =
    xcm.Graph.empty[NodeType, EdgeType]

  val graph = emptyGraph()

  type InnerEdgeT = graph.EdgeT
  type InnerNodeT = graph.NodeT
  type OuterEdgeT = EdgeType[NodeType]
  type OuterNodeT = NodeType

  protected def createOuterEdge(
    label: LabelType,
    n1: OuterNodeT,
    n2: OuterNodeT
  ): OuterEdgeT

  def addEdge(
    label: LabelType,
    n1: OuterNodeT,
    n2: OuterNodeT
  ): Boolean = {
    val e = createOuterEdge(label, n1, n2)
    graph.add(e)
  }

  // TODO: FIXME I think this is an error (edge._2 || edge._1 might contain node id)
  def findEdge(n1: OuterNodeT, n2: OuterNodeT): Option[OuterEdgeT] = {
    val matching = for {
      inner1 <- graph.find(n1).to(List)
      edge   <- inner1.edges
      if edge._2.toOuter == n2
    } yield edge.toOuter

    matching.headOption

  }

  def upsertEdge(
    f: Option[LabelType] => LabelType,
    n1: OuterNodeT,
    n2: OuterNodeT
  ): Boolean = {
    val eold     = findEdge(n1, n2)
    val olabel   = eold.map(_.label.asInstanceOf[LabelType])
    val newLabel = f(olabel)
    val e        = createOuterEdge(newLabel, n1, n2)
    graph.upsert(e)
  }

  def node(n: OuterNodeT): InnerNodeT = graph get n

  override def toString(): String = {
    graph.mkString("; ")
  }
}

case class EdgeLabel(
  name: String
)

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

