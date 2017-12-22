package edu.umass.cs.iesl.watr
package utils

import textboxing.{TextBoxing => TB}

object TreeShaper {
  def apply[N:Numeric:Ordering] = new TreeShaper {
    override type NodeType = N
  }
}

abstract class TreeShaper[N:Numeric:Ordering] {
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

object ScalazTreeImplicits {
  import scalaz._


  implicit class RicherTree[A](val thisTree: scalaz.Tree[A]) extends AnyVal {

    def drawBox(implicit sh: Show[A]): TB.Box = {
      TB.linesToBox(thisTree.draw0)
    }

    def draw0(implicit sh: Show[A]): Stream[String] = {
      def drawSubTrees(s: List[Tree[A]]): Stream[String] = s match {
        case Nil      => Stream.Empty
        case t :: Nil => shift("╰─ ", "   ", t.draw0)
        case t :: ts  => shift("├─ ", "│  ", t.draw0) append drawSubTrees(ts)
      }
      def shift(first: String, other: String, s: Stream[String]): Stream[String] =
        (first #:: Stream.continually(other)).zip(s).map {
          case (a, b) => a + b
        }
      def mapParts[X, Y](as: Stream[X])(f: (X, Boolean, Boolean) => Y): Stream[Y] = {
        def loop(as: Stream[X], first: Boolean): Stream[Y] =
          if (as.isEmpty)           Stream.empty
          else if (as.tail.isEmpty) f(as.head, first, true) #:: Stream.empty
          else                      f(as.head, first, false) #:: loop(as.tail, false)
        loop(as, true)
      }

      val body = sh.shows(thisTree.rootLabel)
      val lines = body.split("\n").toStream
      mapParts(lines) { (a, first, last) =>
        a
      } ++ drawSubTrees(thisTree.subForest.toList)
    }

  }

}
