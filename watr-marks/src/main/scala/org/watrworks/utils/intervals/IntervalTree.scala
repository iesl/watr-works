package edu.umass.cs.iesl.watr
package utils.intervals


/**
 * An implementation of a Centered Interval Tree for efficient search in a set of intervals. See
 * <a href="https://en.wikipedia.org/wiki/Interval_tree">https://en.wikipedia.org/wiki/Interval_tree</a>.
 *
 * <p>
 * The tree functions as a set, meaning that it will not store an interval more than
 * once. More formally, for any two distinct intervals x and y within the tree, it is
 * guaranteed that x.equals(y) will evaluate to false. If you try to add an interval to the tree,
 * which is already in it, the tree will reject it. See the documentation of
 * {@link #add(Interval) the add method} for more information. The tree will also <strong>not</strong> accept
 * {@code null} or empty intervals, meaning intervals whose {@link Interval#isEmpty()}
 * method evaluates to {@code true}.
 * </p>
 * <p>
 * The {@link #iterator()} method of the tree returns a fail-fast iterator, which will
 * throw a {@link ConcurrentModificationException}, if the tree is modified in any form
 * during the iteration, other than by using the iterator's own {@code remove} method. However,
 * this is done in a best-effort manner, since it is generally hard to guarantee this behaviour
 * while using non-atomic and not synchronized methods.
 *</p>
 * <p>
 * The tree relies on the usage of a subclass of the {@link Interval} class to represent the
 * intervals. The majority of the interval methods are already implemented within the
 * {@code Interval} class and don't have to be implemented by the extending class. Comparisons
 * between the intervals are also pre-implemented in the {@code Interval} class and use the
 * start and endpoints to create a total order of all stored intervals. However, if the tree
 * needs to store intervals that have the same start and end points but represent different
 * logical entities, you need a subclass that overwrites the {@code equals}, {@code hashCode}
 * and {@code compareTo} methods. See the documentation of {@link Interval} for more information.
 *</p>
 *
 * @param <T> The type for the start and end point of the interval
 */

// public class IntervalTree<T extends Comparable<? super T>> extends AbstractSet<Interval<T>> {
object IntervalTree {

}

import textboxing.{TextBoxing => TB}, TB._

class IntervalTree[T: Ordering: MidpointHelper, W] {

  import collection.mutable


  /**
    * The root of the current interval tree. It is {@code null} initially, when the tree is
    * empty and may change as the result of adding or removing intervals to the tree.
    */
  var root: TreeNode[T, W] = null

  /**
    * The size of the interval tree, or the amount of intervals stored in it.
    */
  var size: Int = 0

  def asBox(): TB.Box = {
    val treeBox = if (root==null) {
      "null".box
    } else root.asBox()

    s"Tree size=${size}".hangIndent(
      treeBox
    )
  }

  /**
    * Adds an interval to the tree. If the interval is empty, it is rejected and not
    * stored in the tree. This operation may cause a rebalancing of the tree, which
    * in turn may cause intervals to be {@link TreeNode#assimilateOverlappingIntervals(TreeNode) assimilated}.
    * This is why this operation may run in {@code O(n)} worst-case time, even though
    * on average it should run in {@code O(logn)} due to the nature binary trees.
    *
    * @param interval The interval to be added to the tree.
    * @return {@code true}, if the tree has been modified as a result of the operation,
    *         or {@code false} otherwise.
    */
  def add(interval: Interval[T, W]): Boolean = {
    if (interval.isEmpty()) false else {

      val sizeBeforeOperation = size;
      root = TreeNode.addInterval(this, root, interval);
      size == sizeBeforeOperation;
    }
  }

  /**
    * Searches for and returns all intervals stored in the tree, that contain a given
    * query point. This operation is guaranteed to run in {@code O(logn + k)}, where
    * {@code n} is the size of the tree and {@code k} is the size of the returned set,
    * provided that the time complexity of iterating over the intervals stored in each
    * visited node is amortized {@code O(1)}. This assumption is met for the current
    * implementation of {@link TreeNode}, where {@link TreeSet}s are used.
    *
    * @param point The query point.
    * @return A set containing all intervals from the tree, intersecting the query point.
    */
  def query(point: T): Set[Interval[T, W]] = {
    TreeNode.query(root, point,  Set[Interval[T, W]]())
  }

  /**
    * Searches for and returns all intervals stored in the tree, that intersect a given
    * query interval. This operation is guaranteed to run in {@code O(logn + k)}, where
    * {@code n} is the size of the tree and {@code k} is the size of the returned set,
    * provided that the time complexity of iterating over the intervals stored in each
    * visited node is amortized {@code O(1)}. This assumption is met for the current
    * implementation of {@link TreeNode}, where {@link TreeSet}s are used.
    *
    * @param interval The query interval.
    * @return A set containing all intervals from the tree, intersecting the query interval.
    */
  def query(interval: Interval[T, W]): Set[Interval[T, W]] = {

    if (root == null || interval.isEmpty()) {
      Set()
    } else {
      val result = mutable.Set[Interval[T, W]]()
      var node = root;
      while (node != null){
        if (interval.contains(node.midpoint)){
          result ++= node.increasing
          result ++= TreeNode.rangeQueryLeft(node.left, interval);
          result ++= TreeNode.rangeQueryRight(node.right, interval);
          node = null
        } else if (interval.isLeftOfPoint(node.midpoint, inclusive=true)) {

          val incs = node.increasing
            .takeWhile { interval.intersects(_) }

          result ++= incs


          node = node.left;
        } else {
          val decs = node.decreasing
            .takeWhile { interval.intersects(_) }

          result ++= decs

          node = node.right;
        }
      }
      result.toSet;
    }
  }

  /**
    * Removes an interval from the tree, if it was stored in it. This operation may cause the
    * {@link TreeNode#deleteNode(TreeNode) deletion of a node}, which in turn may cause
    * rebalancing of the tree and the {@link TreeNode#assimilateOverlappingIntervals(TreeNode) assimilation}
    * of intervals from one node to another. This is why this operation may run in {@code O(n)}
    * worst-case time, even though on average it should run in {@code O(logn)} due to the
    * nature binary trees.
    *
    * @param interval
    * @return
    */
  def remove(interval: Interval[T, W]): Boolean = {
    (interval.nonEmpty() && root != null &&
      false
    )

    if (interval.isEmpty() || root == null)
      return false;
    val sizeBeforeOperation = size;
    root = TreeNode.removeInterval(this, root, interval);
    return size == sizeBeforeOperation;
  }

  def clear(): Unit = {
    size = 0
    root = null
  }

  def isEmpty(): Boolean = size == 0
  def contains(interval: Interval[T, W]): Boolean = {
    query(interval).contains(interval)
  }
  def contains(t: T): Boolean = {
    query(t).exists { interval =>
      interval.isPoint() && interval.start.exists(_==t)
    }
  }

  def iterator(): Iterator[Interval[T, W]]  = {
    if (root == null){
      Iterator[Interval[T, W]]()
    } else {
      root.intervalIterator()
    }
  }

}
