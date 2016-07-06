package edu.umass.cs.iesl.watr
package utils


object TreeShaper {
  def apply[N : Numeric](
    implicit o: Ordering[N]
  ) = new TreeShaper {
    override type NodeType = N
  }
}

abstract class TreeShaper[N](
  implicit ord: Ordering[N], num: Numeric[N]

) {
  import scalaz.Tree
  import scalaz.TreeLoc
  import scalaz.syntax.tree._

  type NodeType = N
  type Path = Seq[NodeType]
  type Paths = Seq[Path]
  type Edges = Seq[(NodeType, NodeType)]


  import scala.collection.immutable.SortedSet

  def singleton(t: N): SortedSet[N] = SortedSet[N](t)

  def empty[T: Ordering]: SortedSet[T] = SortedSet[T]()

  def makeTreeFromPairs(paths: Edges): Stream[Tree[NodeType]] = {

    val parentChildMap: Map[NodeType, SortedSet[NodeType]] = {
      val base = paths.map { case (parent, child)  =>
        List(
          parent -> singleton(child),
          child -> empty
        )
      }.flatten.toMap

      paths.foldLeft(base) {
        case (mapAcc, (parent, child)) =>
          val children = mapAcc(parent)
          mapAcc + (parent -> (children + child))
      }
    }

    def mkTree(id: NodeType): Tree[NodeType] = id.node(
      parentChildMap(id).toList.map(mkTree(_)):_*
    )

    val inner = parentChildMap.values
      .foldLeft(empty) ((acc, e) => acc ++ e)


    val parents = parentChildMap.keys.toSet
    val roots = parents diff inner

    val str = roots.map(mkTree(_)).toStream

    str

  }


  def getAllPathsInTreeLoc[A](treeLoc: TreeLoc[A]): List[List[A]] = {
    val allPathsToLeaves: List[List[A]] =
      treeLoc.cojoin
        .tree
        .flatten
        .filter(_.isLeaf)
        .map(_.path.toList.reverse)
        .toList

    allPathsToLeaves
  }

}
