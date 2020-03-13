package org.watrworks
package utils.intervals

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntervalTreeNodeTest extends AnyFlatSpec with Matchers {


  it should "_iteratorNormal" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.open(6, 10),
      Interval.bounded.create.closed(2, 100),
      Interval.bounded.create.closed(8, 20),
      Interval.bounded.create.closed(-2, 0),
      Interval.bounded.create.leftOpenRightClosed(-3, 0),
      Interval.bounded.create.leftClosedRightOpen(10, 20),
      Interval.bounded.create.leftClosedRightOpen(11, 14),
      Interval.bounded.create.closed(-20, -10),
      Interval.bounded.create.closed(-14, -11),
      Interval.bounded.create.open(-14, -10),
      Interval.bounded.create.open(0, 4)
    )

    for (interval <- arr) {
      tree.add(interval)
    }

    assert(tree.iterator().toSet == arr.toSet)

  }


  it should "_iteratorEmpty" in {
    val tree = new IntervalTree[Int, Unit]();
    assert(! tree.iterator().hasNext);
    tree.add(Interval.bounded.create.closed(1, 3));
    val it = tree.iterator();
    it.next();
    assert(! it.hasNext);
    try{
      it.next();
      fail();
    } catch {
      case e: Exception =>
        assert(e.isInstanceOf[NoSuchElementException]);
    }
  }

  //
  // it should "_iteratorBackToRootWithMultipleIntervals" in {
  // 	IntervalTree<Integer> tree = new IntervalTree<>();
  // 	Set<Interval<Integer>> set = new HashSet<>(new ArrayList<Interval<Integer>>(Arrays.asList(
  // 			Interval.bounded.create.leftOpenRightClosed(12, 22),
  // 			Interval.bounded.create.leftOpenRightClosed(12, 25),
  // 			Interval.bounded.create.leftOpenRightClosed(0, 10)
  // 	)));
  // 	for (Interval<Integer> next: set)
  // 		tree.add(next);
  // 	Set<Interval<Integer>> result = new HashSet<>();
  // 	for (Interval<Integer> next: tree)
  // 		result.add(next);
  // 	assertThat(set, is(result));
  // }

  //
  // it should "_iteratorRemove" in {
  // 	IntervalTree<Integer> tree = new IntervalTree<>();
  // 	Interval<Integer> target = Interval.bounded.create.leftOpenRightClosed(12, 22);
  // 	Interval<Integer> root = Interval.bounded.create.leftClosedRightOpen(2, 10);
  // 	Interval<Integer> left = Interval.bounded.create.open(1, 5);
  // 	tree.add(root);
  // 	tree.add(target);
  // 	tree.add(left);
  // 	Iterator<Interval<Integer>> it = tree.iterator();
  // 	while (it.hasNext()){
  // 		Interval<Integer> next = it.next();
  // 		if (next == target)
  // 			it.remove();
  // 	}

  // 	List<Interval<Integer>> list = new ArrayList<>();
  // 	for (Interval<Integer> next: tree)
  // 		list.add(next);

  // 	assert(list.contains(root));
  // 	assert(list.contains(left));
  // 	assert(! list.contains(target));
  // }

  //
  // it should "_iteratorRemoveChangesTheRoot" in {
  // 	IntervalTree<Integer> tree = new IntervalTree<>();
  // 	Interval<Integer> target = Interval.bounded.create.leftOpenRightClosed(12, 22);
  // 	Interval<Integer> root = Interval.bounded.create.leftClosedRightOpen(2, 10);
  // 	Interval<Integer> left = Interval.bounded.create.open(1, 5);
  // 	Interval<Integer> leftGrandchild = Interval.bounded.create.open(-10, 0);
  // 	tree.add(root);
  // 	tree.add(target);
  // 	tree.add(left);
  // 	tree.add(leftGrandchild);
  // 	TreeNode<Integer> newRoot = tree.root.left;

  // 	Iterator<Interval<Integer>> it = tree.iterator();
  // 	while (it.hasNext()){
  // 		Interval<Integer> next = it.next();
  // 		if (next == target)
  // 			it.remove();
  // 	}

  // 	List<Interval<Integer>> list = new ArrayList<>();
	// 	for (Interval<Integer> next: tree)
	// 		list.add(next);

	// 	assert(list.contains(root));
	// 	assert(list.contains(left));
	// 	assert(list.contains(leftGrandchild));
	// 	assert(! list.contains(target));

	// 	assertEquals(newRoot, tree.root);
	// }

	//
	// it should "_iteratorRemoveIntervalWithoutDeletingNode" in {
	// 	IntervalTree<Integer> tree = new IntervalTree<>();
	// 	IntegerInterval[] arr = new IntegerInterval[]{
	// 			Interval.bounded.create.leftClosedRightOpen(20, 30),
	// 			Interval.bounded.create.leftOpenRightClosed(0, 10),
	// 			Interval.bounded.create.open(30, 40)
	// 	};
	// 	val target = Interval.bounded.create.closed(-4, 18);
	// 	for (Interval<Integer> next: arr)
	// 		tree.add(next);
	// 	tree.add(target);

	// 	TreeNode<Integer> root = tree.root;
	// 	TreeNode<Integer> left = root.left;
	// 	TreeNode<Integer> right = root.right;

	// 	Iterator<Interval<Integer>> it = tree.iterator();
	// 	while(it.hasNext()){
	// 		Interval<Integer> next = it.next();
	// 		if (next == target)
	// 			it.remove();
	// 	}

	// 	assertEquals(tree.root, root);
	// 	assertEquals(tree.root.left, left);
	// 	assertEquals(tree.root.right, right);

	// 	List<Interval<Integer>> list = new ArrayList<>();
	// 	for (Interval<Integer> next: tree){
	// 		list.add(next);
	// 	}

	// 	for (Interval<Integer> next: arr)
	// 		assert(list.contains(next));
	// 	assert(! list.contains(target));
	// }

	//
	// it should "_iteratorRemoveDeletesInnerNode" in {
	// 	IntervalTree<Integer> tree = new IntervalTree<>();
	// 	val target = Interval.bounded.create.open(40, 50);
	// 	IntegerInterval[] arr = new IntegerInterval[]{
	// 			Interval.bounded.create.leftClosedRightOpen(100, 110),
	// 			Interval.bounded.create.leftClosedRightOpen(150, 160),
	// 			Interval.bounded.create.leftOpenRightClosed(50, 60),
	// 			Interval.bounded.create.leftClosedRightOpen(160, 170),
	// 			Interval.bounded.create.open(60, 70),
	// 			target,
	// 			Interval.bounded.create.open(70, 80),
	// 			Interval.bounded.create.leftClosedRightOpen(140, 150),

	// 	};
	// 	for (IntegerInterval next: arr)
	// 		tree.add(next);
	// 	Iterator<Interval<Integer>> it = tree.iterator();
	// 	List<Interval<Integer>> list = new ArrayList<>();
	// 	while(it.hasNext()){
	// 		Interval<Integer> next = it.next();
	// 		if (next == target)
	// 			it.remove();
	// 		else
	// 			list.add(next);
	// 	}

	// 	assertEquals(arr.length-1, list.size());
	// 	for (Interval<Integer> next: arr){
	// 		if (target == next)
	// 			assert(! list.contains(next));
	// 		else
	// 			assert(list.contains(next));
	// 	}
	// }

	//
	// it should "_iteratorRemoveDeletesInnerNodeAndPromotesTheSubtreeRoot" in {
	// 	IntervalTree<Integer> tree = new IntervalTree<>();
	// 	IntegerInterval target = Interval.bounded.create.open(40, 50);
	// 	IntegerInterval[] arr = new IntegerInterval[]{
	// 			Interval.bounded.create.leftClosedRightOpen(100, 110),
	// 			Interval.bounded.create.leftClosedRightOpen(150, 160),
	// 			Interval.bounded.create.leftOpenRightClosed(50, 60),
	// 			Interval.bounded.create.leftClosedRightOpen(160, 170),
	// 			Interval.bounded.create.open(60, 70),
	// 			target,
	// 			Interval.bounded.create.open(70, 80),
	// 			Interval.bounded.create.leftClosedRightOpen(140, 150),
	// 			Interval.bounded.create.closed(46, 49)

	// 	};
	// 	for (IntegerInterval next: arr)
	// 		tree.add(next);
	// 	Iterator<Interval<Integer>> it = tree.iterator();
	// 	List<Interval<Integer>> list = new ArrayList<>();
	// 	while(it.hasNext()){
	// 		Interval<Integer> next = it.next();
	// 		if (next == target)
	// 			it.remove();
	// 		else
	// 			list.add(next);
	// 	}

	// 	assertEquals(arr.length-1, list.size());
	// 	for (Interval<Integer> next: arr){
	// 		if (target == next)
	// 			assert(! list.contains(next));
	// 		else
	// 			assert(list.contains(next));
	// 	}
	// }


}
