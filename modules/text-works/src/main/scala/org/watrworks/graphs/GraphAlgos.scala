package org.watrworks
package graphs

import scalax.{collection => xc}
import xc.{mutable => xcm}
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

    matching.headOption
  }

  def upsertEdge(
    f: Option[OuterEdgeT] => OuterEdgeT,
    n1: OuterNodeT,
    n2: OuterNodeT
  ): Boolean = {
    val eold    = findEdge(n1, n2)
    val newEdge = f(eold)
    val didInsert = graph.upsert(newEdge)
    didInsert
  }

  def node(n: OuterNodeT): InnerNodeT = graph get n

  override def toString(): String = {
    graph.mkString("; ")
  }
}

abstract class LabeledGraph[NodeType, EdgeType[+X] <: EdgeLikeIn[X], LabelType](implicit
  CT: ClassTag[EdgeType[NodeType]]
) extends CustomGraph[NodeType, EdgeType] {

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

}
