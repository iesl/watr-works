package edu.umass.cs.iesl.watr
package utils

import scala.collection.mutable

/**
  * Copied from   https://github.com/pathikrit/scalgos
  *
  *
  * A disjoint-set data structure (also called union-find data structure)
  * Has efficient union and find operations in amortised O(a(n)) time (where a is the inverse-Ackermann function)
  * @tparam A types of things in set
  *
  */
class DisjointSet[A] {
  import DisjointSet.Node
  private[this] val parent = mutable.Map.empty[A, Node[A]]

  private[this] implicit def toNode(x: A) = {
    assume(contains(x))
    parent(x)
  }

  def contains(x: A) = parent contains x

  /**
    * Add a new singleton set with only x in it (assuming x is not already known)
    */
  def add(x: A) = {
    assume(!contains(x))
    parent(x) = new Node(x)
  }

  /**
    * Union the sets containing x and y
    */
  def union(x: A, y: A) = {
    val (xRoot, yRoot) = (x.root, y.root)
    if (xRoot != yRoot) {
      if (xRoot.rank < yRoot.rank) {        // change the root of the shorter/less-depth one
        xRoot.parent = yRoot
      } else if (xRoot.rank > yRoot.rank) {
        yRoot.parent = xRoot
      } else {
        yRoot.parent = xRoot
        xRoot.rank += 1   // else if there is tie, increment
      }
    }
  }

  /**
    * @return the root (or the canonical element that contains x)
    */
  def getCanonical(x: A) = x.root.entry
  def apply(x: A) = x.root.entry

  /**
    * @return Iterator over groups of items in same set
    */
  def sets = parent.keys
    .groupBy {_.root.entry}
    .values
}

object DisjointSet {

  /**
    * Each internal node in DisjointSet
    */
  private[DisjointSet] class Node[A](val entry: A) {
    /**
      * parent - the pointer to root node (by default itself)
      * rank - depth if we did not do path compression in find - else its upper bound on the distance from node to parent
      */
    var (parent, rank) = (this, 0)

    def root: Node[A] = {
      if (parent != this) {
        parent = parent.root     // path compression
      }
      parent
    }
  }

  private[DisjointSet] class OrderedNode[A](val entry: A) {
    /**
      * parent - the pointer to root node (by default itself)
      * rank - depth if we did not do path compression in find - else its upper bound on the distance from node to parent
      */
    var (parent, rank, head, next) = (this, 0, this, this)

    def root: OrderedNode[A] = {
      if (parent != this) {
        parent = parent.root     // path compression
      }
      parent
    }
  }

  /**
    * @return empty disjoint set
    */
  def empty[A] = new DisjointSet[A]

  /**
    * @return a disjoint set with each element in its own set
    */
  def apply[A](elements: A*) = {
    val d = empty[A]
    elements foreach {e => d add e}
    d
  }
}


class OrderedDisjointSet[A] {
  import OrderedDisjointSet.Node
  private[this] val parent = mutable.Map.empty[A, Node[A]]

  private[this] implicit def toNode(x: A) = {
    assume(contains(x))
    parent(x)
  }

  def contains(x: A) = parent contains x

  def setOrderBy[B: Ordering](x: A)(projectf: A => B): Unit =
    x.root.ordering = x.root.ordering.sortBy(projectf)

  def setOrderWith(x: A)(sortf: (A, A) => Boolean): Unit =
    x.root.ordering = x.root.ordering.sortWith(sortf)

  /**
    * Add a new singleton set with only x in it (assuming x is not already known)
    */
  def add(x: A) = {
    assume(!contains(x))
    parent(x) = new Node(x)
  }

  /**
    * Union the sets containing x and y
    */
  def union(x: A, y: A) = {
    val (xRoot, yRoot) = (x.root, y.root)
    if (xRoot != yRoot) {
      if (xRoot.rank < yRoot.rank) {        // change the root of the shorter/less-depth one
        xRoot.parent = yRoot

        yRoot.ordering = xRoot.ordering ++ yRoot.ordering
        xRoot.ordering = List.empty
      } else if (xRoot.rank > yRoot.rank) {
        yRoot.parent = xRoot

        xRoot.ordering = xRoot.ordering ++ yRoot.ordering
        yRoot.ordering = List.empty
      } else {
        yRoot.parent = xRoot
        xRoot.rank += 1   // else if there is tie, increment

        xRoot.ordering = xRoot.ordering ++ yRoot.ordering
        yRoot.ordering = List.empty
      }
    }
  }

  /**
    * @return the root (or the canonical element that contains x)
    */
  def getCanonical(x: A) = x.root.entry
  def apply(x: A) = x.root.entry

  def at(x: A) = x.root

  /**
    * @return Iterator over groups of items in same set
    */
  def sets(): Iterable[Iterable[A]] = {
    parent.keys
      .groupBy {_.root.entry}
      .keys.map(_.ordering)
  }
}


object OrderedDisjointSet {


  private[OrderedDisjointSet] class Node[A](val entry: A) {
    /**
      * parent - the pointer to root node (by default itself)
      * rank - depth if we did not do path compression in find - else its upper bound on the distance from node to parent
      */
    var (parent, rank) = (this, 0)

    var ordering: List[A] = List(entry)

    def reorderBy[B: Ordering](projectf: A => B): Unit =
      ordering = ordering.sortBy(projectf)

    def reorderWith(sortf: (A, A) => Boolean): Unit =
      ordering = ordering.sortWith(sortf)

    def root: Node[A] = {
      if (parent != this) {
        parent = parent.root     // path compression
      }
      parent
    }
  }

  /**
    * @return empty disjoint set
    */
  def empty[A] = new OrderedDisjointSet[A]

  /**
    * @return a disjoint set with each element in its own set
    */
  def apply[A](elements: A*) = {
    val d = empty[A]
    elements foreach {e => d add e}
    d
  }
}
