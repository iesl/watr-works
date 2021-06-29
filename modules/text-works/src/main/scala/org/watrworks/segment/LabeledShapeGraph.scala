//
package org.watrworks
package segment

import scalax.{collection => xc}
// import xc.edge._
// import xc.edge.Implicits._
import xc.GraphPredef._
import xc.GraphEdge._

import utils.ExactFloats
import ExactFloats._
import utils.{Direction => Dir}
import org.watrworks.utils.Interval

// sealed trait JumpLabel {
//   def fontId: String @@ ScaledFontID
// }
// object JumpLabel {

//   case class RightJump(
//     fontId: String @@ ScaledFontID,
//     downJumpDist: Int @@ FloatRep,
//     upJumpDist: Int @@ FloatRep
//   ) extends JumpLabel

//   case class DownJump(
//     fontId: String @@ ScaledFontID,
//     downJumpDist: Int @@ FloatRep
//   ) extends JumpLabel
// }
//

object LabeledShapeGraph extends AttributeTags {
  abstract class JumpNode(val shapeId: Int @@ ShapeID) {
    override def toString: String = s"${nodeShape.shapeType}#${shapeId}" // without JumpNode-prefix
    def nodeShape: AnyShape
    def shapeIndex: ShapeIndex
    def primaryFont(): String @@ ScaledFontID = shapeIndex.getAttr(shapeId, PrimaryFont).get
    lazy val fontId: String @@ ScaledFontID = primaryFont()
  }
  object JumpNode {
    def apply(shape: AnyShape, si: ShapeIndex): JumpNode = {
      new JumpNode(shape.id) {
        val nodeShape              = shape
        val shapeIndex: ShapeIndex = si
        // val fontId: String @@ ScaledFontID = shape
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

    // For Keyed Edges based on internal attrs
    // with ExtendedKey[N]
    // def keyAttributes = Seq(jumpDir)

    override def copy[NN](newNodes: Product) =
      new JumpEdge[NN](newNodes, jumpDir, vJumpEvidence)

    // override def withEvidence[NN](newNodes: Product) =
    //   new JumpEdge[NN](newNodes, jumpDir, vJumpEvidence)
  }
  object JumpEdge {
    def apply(from: JumpNode, to: JumpNode, jumpDir: Dir, jumpDist: Int @@ FloatRep) =
      new JumpEdge[JumpNode](NodeProduct(from, to), jumpDir, List(jumpDist))

    def withEvidence(jumpEdge: JumpEdge[JumpNode], jumpDist: Int @@ FloatRep) =
      new JumpEdge[JumpNode](jumpEdge.nodes, jumpEdge.jumpDir, jumpDist :: jumpEdge.vJumpEvidence)

    def unapply(e: JumpEdge[JumpNode]): Option[(JumpNode, JumpNode, Dir, List[Int @@ FloatRep])] =
      if (e eq null) None else Some((e.from, e.to, e.jumpDir, e.vJumpEvidence))
  }
  // implicit class JumpEdgeAssoc[A <: JumpNode](val e: DiEdge[A]) {
  //   @inline def ##(flightNo: String) = new JumpEdge[A](e.nodes, flightNo) with OuterEdge[A, JumpEdge]
  // }
  // implicit class ExtGraph[N, E[+X] <: EdgeLikeIn[X]](protected val g: Graph[N, E]) {
  //   def foo: String = "foo"
  // }

}

import LabeledShapeGraph._

class LabeledShapeGraph extends CustomGraph[JumpNode, JumpEdge]() with AttributeTags {

  // import scalax.collection.edge.LBase.LEdgeImplicits

  // object MyImplicit extends LEdgeImplicits[JumpLabel]
  // import MyImplicit._

  // def edgeDown(
  //   fontId: String @@ ScaledFontID,
  //   downJumpDist: Int @@ FloatRep
  // ): JumpLabel =
  //   JumpLabel.DownJump(
  //     fontId,
  //     downJumpDist
  //   )

  // def edgeRight(
  //   fontId: String @@ ScaledFontID,
  //   jumpEvidenceDir: Dir,
  //   jumpDist: Int @@ FloatRep
  // ): JumpLabel = {
  //   val (upDist, downDist) = jumpEvidenceDir match {
  //     case Dir.Up   => (jumpDist, FloatExact.zero)
  //     case Dir.Down => (FloatExact.zero, jumpDist)
  //     case d =>
  //       println(
  //         s"Error: JumpRight edge specified with directional evidence ${d}, requires Up or Down"
  //       )
  //       (FloatExact.zero, FloatExact.zero)
  //   }
  //   JumpLabel.RightJump(
  //     fontId,
  //     downJumpDist = downDist,
  //     upJumpDist = upDist
  //   )
  // }

  // def edge(jl: JumpLabel, n1: AnyShape, n2: AnyShape): Boolean = {
  //   // import JumpLabel._
  //   // upsertEdge(
  //   //   (maybeOld: Option[JumpLabel]) => {
  //   //     maybeOld
  //   //       .flatMap(ol => {
  //   //         (ol, jl) match {
  //   //           case (oldJump: RightJump, newJump: RightJump) =>
  //   //             val newDownJump = ExactFloats.max(newJump.downJumpDist, oldJump.downJumpDist)
  //   //             val newUpJump   = ExactFloats.max(newJump.upJumpDist, oldJump.upJumpDist)
  //   //             Some(
  //   //               oldJump.copy(
  //   //                 downJumpDist = newDownJump,
  //   //                 upJumpDist = newUpJump
  //   //               )
  //   //             )
  //   //           case (o, n) =>
  //   //             println(s"Error: Trying to upsert incompatible edges: old=${o}, new=${n}")
  //   //             None
  //   //         }
  //   //       })
  //   //       .getOrElse(jl)
  //   //   },
  //   //   n1,
  //   //   n2
  //   // )
  //   ???
  // }

  type LayeredOrdering = graph.LayeredTopologicalOrder[graph.NodeT]
  type TopoOrdering    = graph.TopologicalOrder[graph.NodeT]

  import Interval._
  def getConnectedComponents(
    fontId: String @@ ScaledFontID,
    vjumpRange: Interval.FloatExacts,
    shapeIndex: ShapeIndex
  ): Seq[TopoOrdering] = {
    val sorted = graph
      .componentTraverser(
        subgraphEdges = (edge) => {
          val jumpMatch = edge.vJumpEvidence.exists(j => vjumpRange.containsLCRC(j))

          // val fontsMatch = label.fontId == fontId
          // val jumpMatch = label match {
          //   case JumpLabel.RightJump(_, downJumpDist, upJumpDist) =>
          //     vjumpRange.containsLCRC(downJumpDist) ||
          //       vjumpRange.containsLCRC(upJumpDist)
          //   case JumpLabel.DownJump(_, downJumpDist) =>
          //     vjumpRange.containsLCRC(downJumpDist)
          // }

          jumpMatch
        },
        subgraphNodes = (node) => {
          node.fontId == fontId
        }
      )
      .topologicalSortByComponent()

    val iter = sorted.flatMap({ cycleOrOrder: graph.CycleNodeOrTopologicalOrder =>
      cycleOrOrder match {
        case Left(cycleNode) =>
          val cycleShape = cycleNode.nodeShape
          // val cycleShape = shapeIndex.getById(cycleNode)
          val pageNum    = cycleShape.pageNum
          val labels     = cycleShape.labels.mkString(", ")

          val chars = shapeIndex.getAttr(cycleShape.id, ExtractedChars).getOrElse(List())
          val chstr = chars.map(_.char).mkString

          println(s"Error: cycle node page:${pageNum} ${labels} str: ${chstr}")

          def _go(node: graph.NodeT, depth: Int, max: Int, parents: List[graph.NodeT]): Unit = {

            if (depth < max) {
              val isLoop = parents.exists(_.toOuter == node.toOuter)

              // val succShape  = shapeIndex.getById(node.toOuter)
              val succShape = node.nodeShape
              val succlabels = succShape.labels.mkString(", ")
              val succchars  = shapeIndex.getAttr(succShape.id, ExtractedChars).getOrElse(List())
              val chstr0     = succchars.map(_.char).mkString
              val lpad       = "   " * depth
              if (isLoop) {
                println(s"${lpad}${node.toOuter}: ${succlabels} str: ${chstr0}")
                println(s"""${lpad}!!!cycle: ${parents.reverse.mkString(", ")}""")

              } else {
                node.diSuccessors.foreach(succ => _go(succ, depth + 1, max, node :: parents))
              }

            }
          }
          _go(cycleNode, 0, 20, List())
          println(s"finished looking for cycle")

          List()
        case Right(ordering) =>
          List(ordering)
      }
    })
    iter.to(List)

  }

}
