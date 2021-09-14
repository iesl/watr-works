package org.watrworks
package segment
package prelude

import scalax.{collection => xc}
import xc.GraphPredef._
import xc.GraphEdge._

import utils.ExactFloats
import ExactFloats._
import utils.{Direction => Dir}
import graphs._

object LabeledShapeGraph extends AttributeTags {
  abstract class JumpNode(val shapeId: Int @@ ShapeID) {
    override def toString: String = {
      val str = shapeIndex.getAttr(shapeId, ExtractedChars).get.map(_.char).mkString
      s"'${str}'@${shapeId}"
    }
    def nodeShape: AnyShape
    def shapeIndex: ShapeIndex
    def primaryFont(): String @@ ScaledFontID = shapeIndex.getAttr(shapeId, PrimaryFont).get
    lazy val fontId: String @@ ScaledFontID   = primaryFont()

    override def equals(other: Any) = other match {
      case that: JumpNode => that.shapeId == this.shapeId
      case _              => false
    }

    override def hashCode = shapeId.unwrap.##
  }

  object JumpNode {
    def apply(shape: AnyShape, si: ShapeIndex): JumpNode = {
      new JumpNode(shape.id) {
        val nodeShape              = shape
        val shapeIndex: ShapeIndex = si
      }
    }
  }

  class JumpEdge[+N](
    nodes: Product,
    val jumpDir: Dir,
    val vJumpEvidence: List[Int @@ FloatRep]
  ) extends DiEdge[N](nodes)
    with EdgeCopy[JumpEdge]
    with OuterEdge[N, JumpEdge] {

    override def copy[NN](newNodes: Product) =
      new JumpEdge[NN](newNodes, jumpDir, vJumpEvidence)
  }

  object JumpEdge {
    def apply(from: JumpNode, to: JumpNode, jumpDir: Dir, jumpDist: Int @@ FloatRep) =
      new JumpEdge[JumpNode](NodeProduct(from, to), jumpDir, List(jumpDist))

    def withEvidence(jumpEdge: JumpEdge[JumpNode], jumpDist: Int @@ FloatRep) =
      new JumpEdge[JumpNode](jumpEdge.nodes, jumpEdge.jumpDir, jumpDist :: jumpEdge.vJumpEvidence)

    def unapply(e: JumpEdge[JumpNode]): Option[(JumpNode, JumpNode, Dir, List[Int @@ FloatRep])] =
      if (e eq null) None else Some((e.from, e.to, e.jumpDir, e.vJumpEvidence))
  }
}

import LabeledShapeGraph._


class LabeledShapeGraph extends CustomGraph[JumpNode, JumpEdge]() with AttributeTags {

  type LayeredOrdering = graph.LayeredTopologicalOrder[graph.NodeT]
  type TopoOrdering    = graph.TopologicalOrder[graph.NodeT]

  def findConnectedComponents(
    edgeFilter: EdgeFilter,
    nodeFilter: NodeFilter,
    shapeIndex: ShapeIndex
  ): List[List[graph.NodeT]] = {
    graph
      .componentTraverser(
        subgraphEdges = edgeFilter,
        subgraphNodes = nodeFilter
      )
      .map(_.nodes.to(List))
      .to(List)
  }

}
