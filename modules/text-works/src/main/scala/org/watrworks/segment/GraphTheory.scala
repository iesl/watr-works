package org.watrworks
package segment

import scalax.{collection => xc}
import xc.{mutable => xcm}
import xc.edge._
import xc.edge.Implicits._
import xc.GraphPredef._
import scala.reflect._
import utils.ExactFloats
import ExactFloats._
import utils.{Direction => Dir}

case class EdgeLabel(
  name: String
)

abstract class LabeledGraph[NodeType, EdgeType[+X] <: EdgeLikeIn[X], LabelType](implicit
  CT: ClassTag[EdgeType[NodeType]]
) {

  def emptyGraph(): xcm.Graph[NodeType, EdgeType] = xcm.Graph[NodeType, EdgeType]()
  val graph                                       = emptyGraph()

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

sealed trait JumpLabel
object JumpLabel {

  case class RightJump(
    fontId: String @@ ScaledFontID,
    downJumpDist: Int @@ FloatRep,
    upJumpDist: Int @@ FloatRep
  ) extends JumpLabel

  case class DownJump(
    fontId: String @@ ScaledFontID,
    downJumpDist: Int @@ FloatRep
  ) extends JumpLabel
}

class ShapeJumpGraph extends LabeledGraph[Int @@ ShapeID, LDiEdge, JumpLabel]() {

  def createOuterEdge(label: JumpLabel, n1: OuterNodeT, n2: OuterNodeT): OuterEdgeT = {
    (n1 ~+> n2)(label)
  }

  def edgeDown(
    fontId: String @@ ScaledFontID,
    downJumpDist: Int @@ FloatRep
  ): JumpLabel =
    JumpLabel.DownJump(
      fontId,
      downJumpDist
    )

  def edgeRight(
    fontId: String @@ ScaledFontID,
    jumpEvidenceDir: Dir,
    jumpDist: Int @@ FloatRep
  ): JumpLabel = {
    val (upDist, downDist) = jumpEvidenceDir match {
      case Dir.Up   => (jumpDist, FloatExact.zero)
      case Dir.Down => (FloatExact.zero, jumpDist)
      case d =>
        println(s"Error: JumpRight edge specified with directional evidence ${d}, requires Up or Down")
        (FloatExact.zero, FloatExact.zero)
    }
    JumpLabel.RightJump(
      fontId,
      downJumpDist = downDist,
      upJumpDist = upDist
    )
  }

  def edge(jl: JumpLabel, n1: AnyShape, n2: AnyShape): Boolean = {
    import JumpLabel._
    upsertEdge(
      (maybeOld: Option[JumpLabel]) => {
        maybeOld
          .flatMap(ol => {
            (ol, jl) match {
              case (oldJump: RightJump, newJump: RightJump) =>
                val newDownJump = ExactFloats.max(newJump.downJumpDist, oldJump.downJumpDist)
                val newUpJump   = ExactFloats.max(newJump.upJumpDist, oldJump.upJumpDist)
                Some(
                  oldJump.copy(
                    downJumpDist = newDownJump,
                    upJumpDist = newUpJump
                  )
                )
              case (o, n) =>
                println(s"Error: Trying to upsert incompatible edges: old=${o}, new=${n}")
                None
            }
          })
          .getOrElse(jl)
      },
      n1.id,
      n2.id
    )
  }

  type LayeredOrdering = graph.LayeredTopologicalOrder[graph.NodeT]
  type TopoOrdering    = graph.TopologicalOrder[graph.NodeT]

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

case class LabeledEdgeShapes(
  name: String
)

class ShapeIDGraph extends LabeledGraph[Int @@ ShapeID, LkDiEdge, LabeledEdgeShapes]() {

  // def createOuterEdge(n1: OuterNodeT, n2: OuterNodeT, label: LabeledEdgeShapes): OuterEdgeT = {
  def createOuterEdge(label: LabeledEdgeShapes, n1: OuterNodeT, n2: OuterNodeT): OuterEdgeT = {
    (n1 ~+#> n2)(label)
  }

  def edge(n1: AnyShape, n2: AnyShape, label: String): Boolean =
    addEdge(LabeledEdgeShapes(label), n1.id, n2.id)

  type LayeredOrdering = graph.LayeredTopologicalOrder[graph.NodeT]
  type TopoOrdering    = graph.TopologicalOrder[graph.NodeT]

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
