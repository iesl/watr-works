package org.watrworks
package utils.intervals

import scala.collection.mutable
import textboxing.{TextBoxing => TB}, TB._
import utils.CharBasedGraphPaper

object TreeNode {


  /**
    * Instantiates a new node in an {@link IntervalTree}.
    *
    * @param interval The initial interval stored in the node. The middlepoint of
    *                 the node will be set based on this interval.
    */
  def apply[T: Ordering: MidpointHelper, W](interval: Interval[T, W]) =
    new TreeNode[T, W](interval)

  import Interval._

  private def appendIntervalToNode[T: Ordering: MidpointHelper, W](node: TreeNode[T, W], interval: Interval[T, W]): Unit = {

    val decreaseOrd = new DecreaseOrdering[T]()
    val increaseOrd = new IncreaseOrdering[T]()

    node.decreasing.append(interval)
    node.decreasing = node.decreasing.sorted(decreaseOrd)
    node.increasing.append(interval);
    node.increasing = node.increasing.sorted(increaseOrd)
  }

  /**
    * A helper function for the {@link IntervalTree#add(Interval)} method. Adds a new
    * interval to the subtree rooted at a {@code TreeNode}.
    *
    * @param tree The {@link IntervalTree} containing the subtree. Used primarily for
    *             housekeeping, such as adjusting the size of the tree, if needed.
    * @param root The root of the subtree, to which we are adding a new interval.
    * @param interval The {@link Interval} that we are adding.
    * @param [T] The type of the start and end points of the interval.
    * @return The new root of the subtree. It may be different than the current root,
    *         if the subtree had to be rebalanced after the operation.
    */
  def addInterval[T: Ordering: MidpointHelper, W](
    tree: IntervalTree[T, W],
    root: TreeNode[T, W],
    interval: Interval[T, W]
  ): TreeNode[T, W] = {
    // println(s"addInterval( ${interval} )")
    // println(s"${tree.asBox()}")

    val res = if (root == null) {
      tree.size = tree.size+1;
      TreeNode[T, W](interval);
    } else if (interval.contains(root.midpoint)){

      // println(s"contains mid=true")
      if (!root.decreasing.contains(interval)) {
        tree.size += 1
      }
      appendIntervalToNode(root, interval)
      root
    } else {

      if (interval.isLeftOfPoint(root.midpoint, inclusive=true)){
        // println(s"leftOf mid")
        root.left = addInterval(tree, root.left, interval);
      } else {
        // println(s"rightOf mid")
        root.right = addInterval(tree, root.right, interval);
      }
      root._height = math.max(height(root.left), height(root.right))+1;

      root.balanceOut();
    }

    // println("After:")
    // println(s"${res.asBox()}")
    // println("\n\n")

    res
  }


  /**
    * Returns the height of a subtree, rooted at a given node. This function accepts
    * {@code null} values and returns 0 as height for them.
    *
    * @param node The node, whose height has to be determined.
    * @return The height of the subtree rooted at {@code node}. Returns 0, if {@code node}
    * is {@code null}.
    */
  def height[T](node: TreeNode[T, _]): Int = {
    if(node == null) 0 else node._height
  }

  /**
    * A helper function for the {@link IntervalTree#query(Comparable)} method.
    * It searches recursively for all intervals stored in the subtree rooted at
    * the current node, that intersect a target point.
    *
    * @param root The root of the currently traversed subtree. May be {@code null}.
    * @param point The query point.
    * @param res The set used to store all intervals to be returned.
    * @param [T] The type of the start and end points of the intervals, as well as
    *            the query point.
    * @return The set of all intervals from the current subtree, containing the query.
    */
  def query[T: Ordering, W](root: TreeNode[T, W], point: T, res: Set[Interval[T, W]]):  Set[Interval[T, W]] = {
    if (root == null) {
      res
    } else if (Interval.ord(point, root.midpoint) <= 0){
      val rights = root.increasing
        .takeWhile { ! _.isRightOfPoint(point, inclusive=true) }

      TreeNode.query(root.left, point, res ++ rights)
    } else {
      val lefts = root.decreasing
        .takeWhile { ! _.isLeftOfPoint(point, inclusive=true) }

      TreeNode.query(root.right, point, res ++ lefts);
    }
  }


  /**
    * A helper function for the {@link IntervalTree#remove(Interval)} method.
    * It searches recursively for the base node of a target interval and
    * removes the interval from the base node, if it is stored there. This is
    * a more efficient way to remove an interval from the tree, since it
    * doesn't iterate through all intervals, but performs a binary search in
    * O(logn).
    *
    * @param tree The {@link IntervalTree} containing the subtree. Used primarily for
    *             housekeeping, such as adjusting the size of the tree, if needed.
    * @param root The root of the currently traversed subtree. May be {@code null}.
    * @param interval The target interval to be removed.
    * @param [T] The type of the start and end points of the intervals, as well as
    *            the query point.
    * @return The new root of the subtree, rooted at the current node, after the
    *         interval has been removed. This could be {@code null} if the interval
    *         was the last one stored at the subtree.
    */
  def removeInterval[T, W](
    tree: IntervalTree[T, W],
    root: TreeNode[T, W],
    interval: Interval[T, W]
  ): TreeNode[T, W] = {
    if (root == null) {
      null
    } else if (interval.contains(root.midpoint)){
      if (root.decreasing.contains(interval)) {

        root.decreasing -= interval
        root.increasing -= interval

        tree.size = tree.size - 1
      }

      if (root.increasing.size == 0){
        deleteNode(root)
      } else root.balanceOut()

    } else if (interval.isLeftOfPoint(root.midpoint, inclusive=true)){
      root.left = removeInterval(tree, root.left, interval);
      root.balanceOut();
    } else {
      root.right = removeInterval(tree, root.right, interval);
      root.balanceOut();
    }
  }

  /**
    * Deletes a node from the tree. The caller of this method needs to check, if the
    * node is actually empty, because this method only performs the deletion.
    *
    * @param root The node that needs to be deleted.
    * @param [T] The type of the start and end points of the intervals.
    * @return The new root of the subtree rooted at the node to be deleted. It may
    *         be {@code null}, if the deleted node was the last in the subtree.
    */
  def deleteNode[T, W](root: TreeNode[T, W]): TreeNode[T, W] = {
    if (root.left == null && root.right == null) null else {
      if (root.left == null){
        // If the left child is empty, then the right subtree can consist of at most
        // one node, otherwise it would have been unbalanced. So, just return
        // the right child.
        root.right;
      } else {
        var node = root.left;
        val stack = mutable.Stack[TreeNode[T, W]]();
        while (node.right != null){
          stack.push(node);
          node = node.right;
        }
        if (stack.nonEmpty) {
          stack.top.right = node.left;
          node.left = root.left;
        }
        node.right = root.right;

        val newRoot = node;
        while (stack.nonEmpty) {
          node = stack.pop();
          if (stack.nonEmpty)
            stack.top.right = newRoot.assimilateOverlappingIntervals(node);
          else
            newRoot.left = newRoot.assimilateOverlappingIntervals(node);
        }
        newRoot.balanceOut();
      }
    }

  }

  /**
    * A helper method for the range search used in the interval intersection query in the tree.
    * This corresponds to the left branch of the range search, once we find a node, whose
    * midpoint is contained in the query interval. All intervals in the left subtree of that node
    * are guaranteed to intersect with the query, if they have an endpoint greater or equal than
    * the start of the query interval. Basically, this means that every time we branch to the left
    * in the binary search, we need to add the whole right subtree to the result set.
    *
    * @param node    The left child of the node, whose midpoint is contained in the query interval.
    * @param query   The query interval.
    * @param result  The set which stores all intervals in the tree, intersecting the query.
    */
  def rangeQueryLeft[T, W](node: TreeNode[T, W], query: Interval[T, _]): Set[Interval[T, W]] = {
    // println(s"rangeQueryLeft(${node}, q=${query})")

    def _loop(node: TreeNode[T, W], acc: Set[Interval[T, W]]): Set[Interval[T, W]] = {
      // println(s"rangeQueryLeft(): acc = ${acc}")
      if (node == null) acc else {
        if (query.contains(node.midpoint)) {
          // println("contains...")
          _loop(
            node.left,
            acc ++ node.increasing ++ node.rightIntervalIterator()
          )
        } else {
          val notLeftOfs = node.decreasing.takeWhile(!_.isLeftOf(query))

          // println(s"rangeQueryLeft, node.decreasing: ${node.decreasing}")
          // println(s"rangeQueryLeft, notLeftOfs     : ${notLeftOfs}")

          _loop(node.right, acc ++ notLeftOfs)
        }
      }
    }

    _loop(node, Set())
    // while (node != null) {
    // 	if (query.contains(node.midpoint)) {
    // 		result.addAll(node.increasing);
    // 		if (node.right != null) {
    // 			for (Interval[T] next : node.right)
    // 				result.add(next);
    // 		}
    // 		node = node.left;
    // 	} else {
    // 		for (Interval[T] next: node.decreasing){
    // 			if (next.isLeftOf(query))
    // 				break;
    // 			result.add(next);
    // 		}
    // 		node = node.right;
    // 	}
    // }
  }

  /**
    * A helper method for the range search used in the interval intersection query in the tree.
    * This corresponds to the right branch of the range search, once we find a node, whose
    * midpoint is contained in the query interval. All intervals in the right subtree of that node
    * are guaranteed to intersect with the query, if they have an endpoint smaller or equal than
    * the end of the query interval. Basically, this means that every time we branch to the right
    * in the binary search, we need to add the whole left subtree to the result set.
    *
    * @param node    The right child of the node, whose midpoint is contained in the query interval.
    * @param query   The query interval.
    * @param result  The set which stores all intervals in the tree, intersecting the query.
    */

  def rangeQueryRight[T, W](node: TreeNode[T, W], query: Interval[T, _]): Set[Interval[T, W]] = {
    // println(s"rangeQueryRight(${node}, q=${query})")

    def _loop(node: TreeNode[T, W], acc: Set[Interval[T, W]]): Set[Interval[T, W]] = {
      // println(s"rangeQueryRight(): acc = ${acc}")
      if (node == null) acc else {
        if (query.contains(node.midpoint)) {
          _loop(
            node.right,
            acc ++ node.increasing ++ node.leftIntervalIterator()
          )
        } else {
          val notRightOfs = node.increasing.takeWhile(!_.isRightOf(query))
          _loop(node.left, acc ++ notRightOfs)
        }
      }
    }

    _loop(node, Set[Interval[T, W]]())
    // while (node != null) {
    // 	if (query.contains(node.midpoint)) {
    // 		result.addAll(node.increasing);
    // 		if (node.left != null) {
    // 			for (Interval[T] next : node.left)
    // 				result.add(next);
    // 		}
    // 		node = node.right;
    // 	} else {
    // 		for (Interval[T] next: node.increasing){
    // 			if (next.isRightOf(query))
    // 				break;
    // 			result.add(next);
    // 		}
    // 		node = node.left;
    // 	}
    // }
    // ???
  }
}

class TreeNode[T: Ordering: MidpointHelper, W](
  init: Interval[T, W]
) { self =>

  import TreeNode._
  import Interval._

  val decreaseOrd = new DecreaseOrdering[T]()
  val increaseOrd = new IncreaseOrdering[T]()

  /**
    * The midpoint of the initial interval added to the node. It is an immutable value
    * and can not be changed, even if the initial interval has been removed from the
    * node.
    */

  val midpoint: Option[T] = implicitly[MidpointHelper[T]].getMidpoint(init)

  def asBox(): TB.Box = {

    assume(increasing.size == decreasing.size)

    val nodeGraphRepr = new CharBasedGraphPaper(increasing.size, increasing.size)

    increasing.toList
      .zipWithIndex.sortWith { case ((interval1, _), (interval2, _)) =>
        decreaseOrd.lt(interval1, interval2)
      }
      .zipWithIndex
      .foreach{ case ((interval, i1), i2) =>
        nodeGraphRepr.drawString(i1, i2, interval.toString())
      }

    val mp = TB.borderBottom(s"""mid:${midpoint.getOrElse("-")}""" )

    val nodeBox = vjoin(TB.center1, mp, nodeGraphRepr.asMonocolorString().box)

    val leftRepr = "Left" atop (if (self.left == null) {
      "null"
    } else {
      TB.borderInlineTop(left.asBox())
    })

    val rightRepr = "Right" atop (if (self.right == null) {
      "null"
    } else {
      TB.borderInlineTop(right.asBox())
    })

    val childs = hcat(
      top,
      List(leftRepr, "  ", rightRepr)
    )

    vjoin(center1,
      nodeBox,
      childs
    )
  }

  /**
    * A set containing all {@link Interval}s stored in this node, ordered by their
    * starting points.
    */

  var increasing: mutable.ArrayBuffer[Interval[T, W]] = {
    mutable.ArrayBuffer[Interval[T, W]](init)
  }

  /**
    * A set containing all {@link Interval}s stored in this node, ordered by their
    * end points.
    */
  var decreasing: mutable.ArrayBuffer[Interval[T, W]] = {
    mutable.ArrayBuffer[Interval[T, W]](init)
  }



  /**
    * A pointer to the left child of the current node. The left child must either be
    * {@code null} or have a midpoint, smaller than the midpoint of the current node. More
    * formally, {@code left.midpoint.compareTo(this.midpoint) < 0} must evaluate to {@code true}.
    */
  var left: TreeNode[T, W] = null

  /**
    * A pointer to the right child of the current node. The right child must either be
    * {@code null} or have a midpoint, larger than the midpoint of the current node. More
    * formally, {@code right.midpoint.compareTo(this.midpoint) > 0} must evaluate to {@code true}.
    */
  var right: TreeNode[T, W] = null

  var _height: Int = 1



  /**
    * Checks if the subtree rooted at the current node is balanced and balances it
    * if necessary.
    *
    * @return The new root of the subtree, after the balancing operation has been
    * performed. It may return a {@code null} value, if the balancing has been
    * triggered by a {@link #removeInterval(IntervalTree, TreeNode, Interval)} operation
    * and the removed interval had been the last one in the subtree.
    */
  def balanceOut(): TreeNode[T, W] = {
    val balance = height(left) - height(right);
    if (balance < -1){
      // The tree is right-heavy.
      if (height(right.left) > height(right.right)){
        right = right.rightRotate();
        leftRotate();
      } else{
        leftRotate();
      }
    } else if (balance > 1){
      // The tree is left-heavy.
      if (height(left.right) > height(left.left)){
        left = left.leftRotate();
        rightRotate();
      } else rightRotate();
    } else {
      // The tree is already balanced.
      this;
    }
  }

  /**
    * Performs a left rotation of the current node, by promoting its right child
    * and demoting the current node. After the left rotation, the promoted node
    * {@link #assimilateOverlappingIntervals(TreeNode) assimilates} the intervals in
    * the demoted node, which intersect its middlepoint.
    *
    * @return The new root of the subtree rooted at the current node, after the
    * rotation has been performed.
    */
  def leftRotate(): TreeNode[T, W] = {
    val head = right;
    right = head.left;
    head.left = this;
    _height = math.max(height(right), height(left)) + 1;
    head.left = head.assimilateOverlappingIntervals(this);
    return head;
  }

  /**
    * Performs a right rotation of the current node, by promoting its left child
    * and demoting the current node. After the right rotation, the promoted node
    * {@link #assimilateOverlappingIntervals(TreeNode) assimilates} the intervals in
    * the demoted node, which intersect its middlepoint.
    *
    * @return The new root of the subtree rooted at the current node, after the
    * rotation has been performed.
    */
  private def  rightRotate(): TreeNode[T, W] = {
    val head: TreeNode[T, W]  = left;
    left = head.right;
    head.right = this;
    _height = Math.max(height(right), height(left)) + 1;
    head.right = head.assimilateOverlappingIntervals(this);
    head;
  }

  /**
    * Transfers all intervals from a target node to the current node, if they
    * intersect the middlepoint of the current node. After this operation, it
    * is possible that the target node remains empty. If so, it needs to be
    * deleted, possible causing the subtree to be rebalanced.
    *
    * @param from The target node, from which intervals will be assimilated.
    * @return The new root of subtree, rooted at the current node.
    */
  private def assimilateOverlappingIntervals(from: TreeNode[T, W]): TreeNode[T, W] = {
    val tmp: mutable.ArrayBuffer[Interval[T, W]] = mutable.ArrayBuffer();

    if (Interval.ord(midpoint, from.midpoint) < 0){
      val rights = from.increasing
        .takeWhile { ! _.isRightOfPoint(midpoint, inclusive=true) }

      tmp ++= rights
    } else {
      val lefts = from.decreasing
        .takeWhile { ! _.isLeftOfPoint(midpoint, inclusive=true) }

      tmp ++= lefts
    }

    from.increasing --= tmp
    from.decreasing --= tmp
    tmp.foreach { t =>
      TreeNode.appendIntervalToNode(this, t)
    }
    // increasing ++= tmp
    // decreasing ++= tmp


    if (from.increasing.size == 0){
      deleteNode(from);
    } else from;
  }



  def intervalIterator(): Iterator[Interval[T, W]]  = {
    new TreeNodeIterator()
  }

  def rightIntervalIterator(): Iterator[Interval[T, W]]  = {
    if (right == null) Iterator.empty
    else right.intervalIterator()
  }

  def leftIntervalIterator(): Iterator[Interval[T, W]]  = {
    if (left == null) Iterator.empty
    else left.intervalIterator()
  }

  /**
    * An iterator over all intervals stored in subtree rooted at the current node. Traversal
    * is done via classic iterative in-order tree traversal where each iteration is in
    * amortized O(1) time. The iterator requires O(logn) space - at each point of the
    * traversal we keep a stack of the currently traversed branch of the tree.
    */

  class TreeNodeIterator extends Iterator[Interval[T, W]] {
    val stack = mutable.Stack[TreeNode[T, W]]();
    var subtreeRoot = self
    var currentNode: TreeNode[T, W] = null
    var currentInterval: Interval[T, W] = null
    // Iterator<Interval[T, W]] iterator = Collections.emptyIterator();
    var it = Iterator[Interval[T, W]]();

    override def hasNext: Boolean = {
      return subtreeRoot != null || stack.nonEmpty || it.hasNext
    }

    override def next(): Interval[T, W] = {
      if (!it.hasNext) {
        while (subtreeRoot != null) {
          stack.push(subtreeRoot);
          subtreeRoot = subtreeRoot.left;
        }
        if (stack.isEmpty)
          throw new NoSuchElementException();
        currentNode = stack.pop();
        it = currentNode.increasing.iterator;
        subtreeRoot = currentNode.right;
      }
      currentInterval = it.next();
      return currentInterval;
    }

  }
}
