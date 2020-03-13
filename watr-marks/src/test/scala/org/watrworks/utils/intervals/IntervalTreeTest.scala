package edu.umass.cs.iesl.watr
package utils.intervals

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntervalTreeTest extends AnyFlatSpec with Matchers {

  it should "deleteNodeAfterAssimilation" in {

		val tree = new IntervalTree[Int, Unit]()

    val a = Interval.bounded.closed(0, 100).get
    val b = Interval.bounded.leftClosedRightOpen(30, 40).get
    val c = Interval.bounded.leftOpenRightClosed(10, 20).get
		tree.add(a);
		tree.add(b);
		tree.add(c);

		assert(tree.root.right  ==  null)
		assert(tree.root.increasing.contains(a));
		assert(tree.root.increasing.contains(b));
		assert(tree.root.decreasing.contains(a));
		assert(tree.root.decreasing.contains(b));
		assert(2 == tree.root.increasing.size);
		assert(2 == tree.root.decreasing.size);
		assert(tree.root.left.decreasing.contains(c));
		assert(tree.root.left.increasing.contains(c));
		assert(1 == tree.root.left.increasing.size)
	}




	it should "removeIntervalForcesRootDelete" in {
		val tree = new IntervalTree[Int, Unit]();
		val a = Interval.bounded.closed(20, 30).get
		val b = Interval.bounded.leftClosedRightOpen(0, 10).get
		val c = Interval.bounded.leftOpenRightClosed(40, 50).get
		tree.add(a);
		tree.add(b);
		tree.add(c);
		tree.remove(a);

		assert(null == tree.root.left);
		assert(!tree.root.increasing.contains(a));
		assert(!tree.root.decreasing.contains(a));
		assert(tree.root.decreasing.contains(b));
		assert(tree.root.increasing.contains(b));
		assert(tree.root.decreasing.contains(c));
		assert(tree.root.increasing.contains(c));
		assert(1 == tree.root.decreasing.size);
		assert(1 == tree.root.increasing.size);
		assert(0 == tree.query(22).size);
	}


	it should "removeFromNodeWithEmptyLeftChild" in {
		val tree = new IntervalTree[Int, Unit]();
		val a = Interval.bounded.closed(20, 30).get
		val b = Interval.bounded.leftOpenRightClosed(40, 50).get
		tree.add(a);
		tree.add(b);
		tree.remove(a);
		assert(null == tree.root.left);
		assert(null == tree.root.right);
		assert(! tree.root.increasing.contains(a));
		assert(! tree.root.decreasing.contains(a));
		assert(tree.root.decreasing.contains(b));
		assert(tree.root.increasing.contains(b));
		assert(1 == tree.root.decreasing.size);
    assert(1 == tree.root.increasing.size);
  }


  it should "removeRootThenAssimilateIntervalsFromInnerNodeAfterBubbleUp" in {
    val tree = new IntervalTree[Int, Unit]();
    val a = Interval.bounded.closed(20, 30).get
    val b = Interval.bounded.leftClosedRightOpen(0, 10).get
    val c = Interval.bounded.leftOpenRightClosed(40, 50 ).get
    val d = Interval.bounded.open(7, 9).get
    tree.add(a);
    tree.add(b);
    tree.add(c);
    tree.add(d);
    tree.remove(a);

    assert(null == tree.root.left);
    assert(2 == tree.root.increasing.size);
    assert(2 ==tree.root.decreasing.size);
    assert(tree.root.increasing.contains(b));
    assert(tree.root.increasing.contains(d));
    assert(tree.root.decreasing.contains(b));
    assert(tree.root.decreasing.contains(d));
    assert(tree.root.decreasing.contains(c));
    assert(tree.root.increasing.contains(c));
  }


  it should "removeRootReplaceWithDeepNodeLeftAndAssimilateIntervals" in {
  	val tree = new IntervalTree[Int, Unit]();
  	val a = Interval.bounded.closed(0, 100).get
  	val b = Interval.bounded.leftClosedRightOpen(10, 30).get;
  	val c = Interval.bounded.leftOpenRightClosed(60, 70).get
  	val d = Interval.bounded.open(80, 90).get
  	val e = Interval.bounded.open(0, 5).get
  	val f = Interval.bounded.open(25, 49).get
  	val g = Interval.bounded.open(39, 44).get
  	tree.add(a);
  	tree.add(b);
  	tree.add(c);
  	tree.add(d);
  	tree.add(e);
  	tree.add(f);
  	tree.add(g);
  	tree.remove(a);

  	assert(null == tree.root.left.right);
  	assert(tree.root.decreasing.contains(g));
  	assert(tree.root.decreasing.contains(f));
  	assert(tree.root.increasing.contains(g));
  	assert(tree.root.increasing.contains(f));
  	assert(! tree.root.decreasing.contains(a));
  	assert(! tree.root.increasing.contains(a));
  }


  it should "removeRootReplaceWithDeepAssimilatingAnotherInnerNode" in {
  	val tree = new IntervalTree[Int, Unit]();
  	val a = Interval.bounded.closed(0, 100).get
  	val b = Interval.bounded.leftClosedRightOpen(10, 40).get;
  	val c = Interval.bounded.leftOpenRightClosed(60, 70).get
  	val d = Interval.bounded.open(80, 90).get
  	val e = Interval.bounded.open(0, 5).get
  	val f = Interval.bounded.open(25, 27).get
  	val g = Interval.bounded.open(37, 40).get
  	tree.add(a);
  	tree.add(b);
  	tree.add(c);
  	tree.add(d);
  	tree.add(e);
  	tree.add(f);
  	tree.add(g);

  	val nodeF = tree.root.left.right;
  	val nodeG = tree.root.left.right;
  	val nodeE = tree.root.left.left;
  	val nodeC = tree.root.right;

  	tree.remove(a);

  	assert(tree.root == nodeG);
  	assert(tree.root.left == nodeF);
  	assert(nodeF.left == nodeE);
  	assert(null == nodeF.right);
  	assert(nodeG.right == nodeC);
  }


  it should "addEmptyInterval" in {
  	val tree = new IntervalTree[Int, Unit]();
  	val a = Interval.bounded.open(5, 6).get
  	tree.add(a);
  	assert(null == tree.root);
	}


	it should "removeFromEmptyTree" in {
		val tree = new IntervalTree[Int, Unit]();
		tree.remove(Interval.bounded.closed(10, 20).get)
		assert(null == tree.root);
	}


	it should "removeEmptyInterval" in {
		val tree = new IntervalTree[Int, Unit]();
		val a = Interval.bounded.open(1, 4).get
		tree.add(a);
    val emptyInterval = Interval.bounded.create.closed(
      30, 20
    )
		tree.remove(emptyInterval)
		assert(1 == tree.root.decreasing.size);
		assert(1 == tree.root.increasing.size);
		assert(tree.root.decreasing.contains(a));
		assert(tree.root.increasing.contains(a));
	}

	it should "removeNonExistingInterval" in {
		var tree = new IntervalTree[Int, Unit]();
		val a = Interval.bounded.closed(20, 30).get
		val b = Interval.bounded.leftClosedRightOpen(0, 10).get;
		val c = Interval.bounded.leftOpenRightClosed(40, 50).get
		tree.add(a);
		tree.add(b);
		tree.add(c);
		tree.remove(Interval.bounded.closed(10, 20).get)
		assert(tree.root.decreasing.contains(a));
		assert(tree.root.increasing.contains(a));
		assert(1 == tree.root.decreasing.size);
		assert(tree.root.left.increasing.contains(b));
		assert(tree.root.left.decreasing.contains(b));
		assert(1 == tree.root.left.decreasing.size);
		assert(tree.root.increasing.contains(c));
		assert(tree.root.decreasing.contains(c));
		assert(1 == tree.root.decreasing.size);

		tree = new IntervalTree[Int, Unit]();
		tree.add(a);
		tree.remove(Interval.bounded.open(10, 40).get)
		assert(tree.root.decreasing.contains(a));
		assert(tree.root.increasing.contains(a));
		assert(1 == tree.root.increasing.size);
		assert(1 == tree.root.decreasing.size);
	}

	it should "queryIntervalNormal" in {
		val tree = new IntervalTree[Int, Unit]();
		val a = Interval.unbounded.rightClosed(22);
		val b = Interval.bounded.closed(7, 13).get
		val c = Interval.bounded.open(21, 24).get
		val d = Interval.unbounded.leftClosed(32);

		tree.add(a);
		tree.add(b);
		tree.add(c);
		tree.add(d);

		val queryInterval = Interval.bounded.closed(18, 29).get
		val res = tree.query(queryInterval);
    val rstr = res.map(_.toString()).mkString(", ")

		assert(2 == res.size);
		assert(res.contains(a));
		assert(res.contains(c));
		assert(! res.contains(b));
		assert(! res.contains(d));
	}


	it should "queryIntervalOpenAndClosedEndpoints" in {
		val tree = new IntervalTree[Int, Unit]();
		val aa = Interval.unbounded.rightClosed(18);
		val ab = Interval.unbounded.leftClosed(18);

		val ac = Interval.unbounded.leftOpen(18);
		val ad = Interval.unbounded.rightOpen(18);

		val ba = Interval.unbounded.rightClosed(29);
		val bb = Interval.unbounded.leftClosed(29);
		val bc = Interval.unbounded.leftOpen(29);
		val bd = Interval.unbounded.rightOpen(29);

		val c = Interval.bounded.closed(7, 13).get
		val d = Interval.bounded.open(21, 24).get
		val e = Interval.bounded.leftOpenRightClosed(32, 45).get

		val queryInterval = Interval.bounded.leftOpenRightClosed(18, 29).get

		val arr = List (
      aa, ab, ac, ad, ba, bb, bc, bd, c, d, e
    )

		for ((interval, i) <- arr.zipWithIndex) {
      // println(s"${i}: addInterval ${interval}")
			tree.add(interval);
      // println(tree.asBox)
    }

    // println(tree.asBox)

    val res = tree.query(queryInterval);


    val expected = List(ab, ac, ba, bb, bd, d).sorted(new Interval.IncreaseOrdering[Int]());

    // val dbg = res.toList.sorted(new Interval.IncreaseOrdering[Int]()).map(_.toString()).mkString(", ")
    // val dbg2 = expected.map(_.toString()).mkString(", ")
    // println(s"query(${queryInterval}) ")
    // println(s"got   : ${dbg}")
    // println(s"want  : ${dbg2}")

    assert(res.forall(expected.contains(_)))
    assert(6 == res.size);
  }




  it should "queryIntervalReturnsASuperInterval" in {
  	val tree = new IntervalTree[Int, Unit]();
  	val a = Interval.bounded.leftClosedRightOpen(8, 20).get;
  	val query = Interval.bounded.open(0, 100).get
  	tree.add(a);
  	val set = tree.query(query);
		assert(1 == set.size);
		assert(set.contains(a));
	}


  it should "queryIntervalChangesInTheTreeDontAffectReturnedSet" in {
    val tree = new IntervalTree[Int, Unit]();
    val aa = Interval.unbounded.rightClosed(18);
    val ab = Interval.unbounded.leftClosed(18);
    val ac = Interval.unbounded.leftOpen(18);
    val ad = Interval.unbounded.rightOpen(18);

    val ba = Interval.unbounded.rightClosed(29);
    val bb = Interval.unbounded.leftClosed(29);
    val bc = Interval.unbounded.leftOpen(29);
    val bd = Interval.unbounded.rightOpen(29);

    val c = Interval.bounded.closed(7, 13).get
    val d = Interval.bounded.open(21, 24).get
    val e = Interval.bounded.leftOpenRightClosed(32, 45).get

    val queryInterval = Interval.bounded.leftOpenRightClosed(18, 29).get
    val arr = List(
      aa, ab, ac, ad, ba, bb, bc, bd, c, d, e
    )

    for (interval <- arr)
      tree.add(interval);
    val res = tree.query(queryInterval);
    for (interval <-  arr)
      tree.remove(interval);

    val expected = List(ab, ac, ba, bb, bd, d);
    assert(expected.forall(res.contains(_)))
    assert(6 == res.size);
  }


  it should "queryIntervalWithNewEndPoints" in {
  	val tree = new IntervalTree[Int, Unit]();
  	val a = Interval.bounded.closed(1, 5).get
  	val b = Interval.bounded.open(7, 20).get
  	val c = Interval.bounded.leftClosedRightOpen(5, 18).get;

  	tree.add(a);
  	tree.add(b);
  	tree.add(c);
  	val queryInterval = Interval.bounded.closed(3, 6).get
  	val  set = tree.query(queryInterval);

  	assert(2 == set.size);
  	assert(set.contains(a));
  	assert(set.contains(c));
  }


  it should "queryIntervalOffByOne" in {
  	var tree = new IntervalTree[Int, Unit]();
  	var a = Interval.bounded.open(1, 5).get
  	tree.add(a);
  	assert(0 == tree.query(Interval.bounded.create.open(4, 20)).size)
  	assert(1 == tree.query(Interval.bounded.create.closed(4, 20)).size)
  	assert(1 == tree.query(Interval.bounded.create.leftClosedRightOpen(4, 20)).size);
  	assert(0 == tree.query(Interval.bounded.create.leftOpenRightClosed(4, 20)).size)

  	tree = new IntervalTree[Int, Unit]();
  	a = Interval.bounded.create.closed(1, 5)
  	tree.add(a);
  	assert(1 == tree.query(Interval.bounded.create.open(4, 20)).size)
  	assert(1 == tree.query(Interval.bounded.create.closed(4, 20)).size)
  	assert(1 == tree.query(Interval.bounded.create.leftClosedRightOpen(4, 20)).size);
  	assert(1 == tree.query(Interval.bounded.create.leftOpenRightClosed(4, 20)).size)

  	tree = new IntervalTree[Int, Unit]();
  	a = Interval.bounded.create.open(1, 5)
  	tree.add(a);
  	assert(0 == tree.query(Interval.bounded.create.open(-5, 2)).size)
  	assert(1 == tree.query(Interval.bounded.create.closed(-5, 2)).size)
  	assert(0 == tree.query(Interval.bounded.create.leftClosedRightOpen(-5, 2)).size);
  	assert(1 == tree.query(Interval.bounded.create.leftOpenRightClosed(-5, 2)).size)

  	tree = new IntervalTree[Int, Unit]();
  	a = Interval.bounded.create.closed(1, 5)
  	tree.add(a);
  	assert(1 == tree.query(Interval.bounded.create.open(-5, 2)).size)
  	assert(1 == tree.query(Interval.bounded.create.closed(-5, 2)).size)
  	assert(1 == tree.query(Interval.bounded.create.leftClosedRightOpen(-5, 2)).size);
  	assert(1 == tree.query(Interval.bounded.create.leftOpenRightClosed(-5, 2)).size)
  }


  it should "queryIntervalOppositeEndpointsEqual" in {
  	var tree = new IntervalTree[Int, Unit]();
  	var a = Interval.bounded.create.open(1, 5)
  	tree.add(a);

  	assert(0 == tree.query(Interval.bounded.create.open(5, 10)).size)
		assert(0 == tree.query(Interval.bounded.create.closed(5, 10)).size)
		assert(0 == tree.query(Interval.bounded.create.leftClosedRightOpen(5, 10)).size);
		assert(0 == tree.query(Interval.bounded.create.leftOpenRightClosed(5, 10)).size)

		tree = new IntervalTree[Int, Unit]();
		a = Interval.bounded.create.closed(1, 5)
		tree.add(a);

		assert(0 == tree.query(Interval.bounded.create.open(5, 10)).size)
		assert(1 == tree.query(Interval.bounded.create.closed(5, 10)).size)
		assert(1 == tree.query(Interval.bounded.create.leftClosedRightOpen(5, 10)).size);
		assert(0 == tree.query(Interval.bounded.create.leftOpenRightClosed(5, 10)).size)

		tree = new IntervalTree[Int, Unit]();
		a = Interval.bounded.create.open(1, 5)
		tree.add(a);

		assert(0 == tree.query(Interval.bounded.create.open(-8, 1)).size)
		assert(0 == tree.query(Interval.bounded.create.closed(-8, 1)).size)
		assert(0 == tree.query(Interval.bounded.create.leftClosedRightOpen(-8, 1)).size);
		assert(0 == tree.query(Interval.bounded.create.leftOpenRightClosed(-8, 1)).size)

		tree = new IntervalTree[Int, Unit]();
		a = Interval.bounded.create.closed(1, 5)
		tree.add(a);

		assert(0 == tree.query(Interval.bounded.create.open(-8, 1)).size)
		assert(1 == tree.query(Interval.bounded.create.closed(-8, 1)).size)
		assert(0 == tree.query(Interval.bounded.create.leftClosedRightOpen(-8, 1)).size);
		assert(1 == tree.query(Interval.bounded.create.leftOpenRightClosed(-8, 1)).size)
	}


  it should "rangeQueryToRightIntersectsMoreThanTwoMidpoints" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.open(6, 10),
      Interval.bounded.create.closed(-5, 0),
      Interval.bounded.create.closed(40, 100),
      Interval.bounded.create.closed(190, 300)
    )

    for (interval <- arr)
      tree.add(interval);

    val expected = Set(arr(0), arr(2))

    val query = Interval.bounded.create.leftClosedRightOpen(7, 170);
    assert(tree.query(query) == expected)
  }

  /**
   * Left branch of a range query, root of subtree contains no valid results,
   * right grandchild has some though.
   */

  it should "rangeQueryLeftWithOnlyRightGrandchildContainingValidResults" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.open(6, 10),
      Interval.bounded.create.closed(-5, 0),
      Interval.bounded.create.closed(40, 100),
      Interval.bounded.create.closed(1, 4)
    )
    for (interval <- arr) tree.add(interval);

    val expected = Set(arr(0), arr(3))

    val query = Interval.bounded.create.closed(3, 8);
    assert(tree.query(query) == expected)
  }

  /**
    * Left branch of a range query, root of subtree contains valid results and has
    * a right child.
    */

  it should "rangeQueryLeftWithRootOfSubtreeAndRightGrandchildContainingResults" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.open(6, 10),
      Interval.bounded.create.closed(12, 30),
      Interval.bounded.create.closed(-100, -50),
      Interval.bounded.create.closed(-1, 4)
    )

    for (interval <- arr) tree.add(interval);
    val expected = Set(arr(0), arr(2), arr(3))

    val query = Interval.bounded.create.leftClosedRightOpen(-80, 9);
    assert(tree.query(query) == expected)

    // Set<val> expected = new HashSet<>(Arrays.asList(arr[0], arr[2], arr[3]));
    // val query = Interval.bounded.create.leftClosedRightOpen(-80, 9).get;
    // assertThat(new HashSet<>(tree.query(query)), is(new HashSet<>(expected)));
  }

  /**
    * Left branch of a range query, root of subtree contains valid results and has
    * a right child. The middlepoint of the root of left tree is not within the query.
      */

  it should "rangeQueryLeftSubtreeMidpointNotInQuery" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(

      Interval.bounded.create.open(6, 10),
      Interval.bounded.create.closed(12, 30),
      Interval.bounded.create.closed(-100, -50),
      Interval.bounded.create.closed(-1, 4)
    )

    for (interval <- arr) tree.add(interval);
    val expected = Set(arr(0), arr(2), arr(3))

    val query = Interval.bounded.create.leftClosedRightOpen(-70, 9);
    assert(tree.query(query) == expected)

    // Set<val> expected = new HashSet<>(Arrays.asList(arr[0], arr[2], arr[3]));
    // val query = Interval.bounded.create.leftClosedRightOpen(-70, 9).get;
    // assertThat(new HashSet<>(tree.query(query)), is(new HashSet<>(expected)));
  }

  /** Right branch of a range query, root of subtree contains no valid results,
    * left grandchild has some though.
    */

  it should "rangeQueryRightWithOnlyLeftGrandchildContainingValidResults" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.open(6, 10),
      Interval.bounded.create.closed(-5, 0),
      Interval.bounded.create.closed(40, 100),
      Interval.bounded.create.closed(20, 30),
    )

    for (interval <- arr) tree.add(interval);
    val expected = Set(arr(0), arr(3))

    val query = Interval.bounded.create.leftClosedRightOpen(7, 22);
    assert(tree.query(query) == expected)

    // Set<val> expected = new HashSet<>(Arrays.asList(arr[0], arr[3]));
    // val query = Interval.bounded.create.leftClosedRightOpen(7, 22).get;
    // assertThat(new HashSet<>(tree.query(query)), is(new HashSet<>(expected)));
  }

  /**
    * Right branch of a range query, root of subtree contains valid results and has
    * a left child.
    */

  it should "rangeQueryRightWithRootOfSubtreeAndLeftGrandchildContainingResults" in {
    val tree = new IntervalTree[Int, Unit]();
      val arr = List(
      Interval.bounded.create.open(6, 10),
      Interval.bounded.create.closed(-5, 0),
      Interval.bounded.create.closed(40, 100),
      Interval.bounded.create.closed(20, 30),
      )
    for (interval <- arr) tree.add(interval);
    val expected = Set(arr(0), arr(2), arr(3))

    val query = Interval.bounded.create.leftClosedRightOpen(7, 75);
    assert(tree.query(query) == expected)

      // Set<val> expected = new HashSet<>(Arrays.asList(arr[0], arr[2], arr[3]));
      // val query = Interval.bounded.create.leftClosedRightOpen(7, 75).get;
      // assertThat(new HashSet<>(tree.query(query)), is(new HashSet<>(expected)));
    }


    it should "rangeQueryEmptyResult" in {
      val tree = new IntervalTree[Int, Unit]();
      assert(tree.query(Interval.unbounded[Int]()).isEmpty);
      tree.add(Interval.bounded.create.open(6, 10))
      tree.add(Interval.bounded.create.closed(-5, 0))
      tree.add(Interval.bounded.create.closed(40, 100))
      tree.add(Interval.bounded.create.closed(20, 30))
      assert(tree.query(Interval.bounded.create.closed(35, 39)).isEmpty)
      assert(tree.query(Interval.bounded.create.closed(80, 25)).isEmpty)
    }


    it should "sizeAfterAddingNonExistingIntervals" in {
    	val tree = new IntervalTree[Int, Unit]();
      val arr = List(
        Interval.bounded.create.closed(5, 25),
        Interval.bounded.create.leftClosedRightOpen(30, 50),
        Interval.bounded.create.leftOpenRightClosed(4, 20),
        Interval.bounded.create.open(-1, 8),
        Interval.bounded.create.open(-2, 8),
        Interval.bounded.create.open(22, 60),
        Interval.bounded.create.closed(100, 200),
        Interval.bounded.create.leftClosedRightOpen(300, 400),
      )

      assert(0 == tree.size);
      for ((next, size) <- arr.zipWithIndex) {
        tree.add(next);
        assert(size+1 == tree.size);
      }
    }


  it should "sizeAfterAddingExistingIntervals" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.closed(5, 25),
      Interval.bounded.create.leftClosedRightOpen(30, 50),
      Interval.bounded.create.leftOpenRightClosed(4, 20),
      Interval.bounded.create.open(-1, 8),
      Interval.bounded.create.open(-2, 8),
      Interval.bounded.create.open(22, 60),
      Interval.bounded.create.closed(100, 200),
      Interval.bounded.create.leftClosedRightOpen(300, 400),
    )
    for (interval <- arr) tree.add(interval);

    assert(tree.size == arr.length);
    for (next <- arr){
      tree.add(next);
      assert(tree.size == arr.length);
    }
  }


  it should "sizeAfterRemovingNonExistingIntervals" in {
  	val tree = new IntervalTree[Int, Unit]();
      val arr = List(

  			Interval.bounded.create.closed(5, 25),
  			Interval.bounded.create.leftClosedRightOpen(30, 50),
  			Interval.bounded.create.leftOpenRightClosed(4, 20),
  			Interval.bounded.create.open(-1, 8),
  			Interval.bounded.create.open(-2, 8),
  			Interval.bounded.create.open(22, 60),
  			Interval.bounded.create.closed(100, 200),
  			Interval.bounded.create.leftClosedRightOpen(300, 400),
      )
      val toRemove = List(
  			Interval.bounded.create.leftClosedRightOpen(9, 20),
  			Interval.bounded.create.closed(500, 500),
  			Interval.bounded.create.open(29, 49),
  			Interval.unbounded.leftClosed(100),
      )
  	for (next <- arr){
  		tree.add(next);
  	}
  	assert(arr.length == tree.size);

    val currSize = tree.size
  	for (next <- toRemove){
  		tree.remove(next);
  		assert(currSize == tree.size);
  	}
  }


  it should "sizeAfterRemovingExistingIntervals" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.closed(5, 25),
      Interval.bounded.create.leftClosedRightOpen(30, 50),
      Interval.bounded.create.leftOpenRightClosed(4, 20),
      Interval.bounded.create.open(-1, 8),
      Interval.bounded.create.open(-2, 8),
      Interval.bounded.create.open(22, 60),
      Interval.bounded.create.closed(100, 200),
      Interval.bounded.create.leftClosedRightOpen(300, 400),
    )
  	for (next <- arr){
  		tree.add(next);
  	}
  	assert(arr.length == tree.size);
    val currSize = tree.size
  	for ((next, i) <- arr.zipWithIndex) {
  		tree.remove(next);
  		assert(currSize-i-1 == tree.size);
  	}

  }


  it should "clearOnEmptyTree" in {
  	val tree = new IntervalTree[Int, Unit]();
  	tree.clear();
  	assert(tree.isEmpty);
  	assert(0 == tree.size);
  	tree.add(Interval.unbounded());
  	assert(1 == tree.size);
  }


  it should "clearOnNonEmptyTree" in {
		val tree = new IntervalTree[Int, Unit]();
		tree.add(Interval.unbounded());
		tree.add(Interval.bounded.create.leftClosedRightOpen(1, 5));
		tree.add(Interval.unbounded.rightOpen(228));
		assert(3 == tree.size);
		assert(! tree.isEmpty())
		tree.clear();
		assert(tree.isEmpty());
		assert(0 == tree.size);
	}


  it should "contains" in {
    val tree = new IntervalTree[Int, Unit]();
    val arr = List(
      Interval.bounded.create.leftClosedRightOpen(5, 22),
      Interval.bounded.create.open(23, 90),
      Interval.bounded.create.closed(0, 4),
      Interval.bounded.create.leftOpenRightClosed(-20, -10),
      Interval.bounded.create.closed(30, 80),
      Interval.bounded.create.open(8, 33),
      Interval.unbounded.rightClosed(-5),
      Interval.unbounded.leftClosed(10),
      Interval.unbounded.leftOpen(12),
      Interval.unbounded.rightOpen(0)
    )

    val notContained = List(

      Interval.bounded.create.leftOpenRightClosed(5, 22),
      Interval.unbounded.leftClosed(-5),
      Interval.unbounded.rightOpen(-5),
      Interval.bounded.create.closed(31, 80),
      Interval.bounded.create.open(-19, -11),
      Interval.bounded.create.closed(-100, -50),
    )

    for (next <- arr){
      tree.add(next);
    }

    for (next <- arr){
      tree.add(next);
      assert(tree.contains(next));
    }
    for (next <- notContained){
      assert(! tree.contains(next))
    }

  }


  it should "containsBroken" in {
  	val tree = new IntervalTree[Int, Unit]();
  	assert(! tree.contains(Interval.unbounded.leftClosed(1)));
  	assert(! tree.contains(Interval.bounded.create.closed(100, 0)))
  	assert(! tree.contains(9));

  	tree.add(Interval.unbounded());

  	assert(! tree.contains(Interval.unbounded.leftClosed(1)))
  	assert(! tree.contains(Interval.bounded.create.closed(100, 0)))
  	assert(! tree.contains(9));
  }


	it should "firstTest" in {
		var tree = new IntervalTree[Int, Unit]();

		tree.add(Interval.bounded.create.closed(10, 20))
		tree.add(Interval.bounded.create.leftClosedRightOpen(20, 40));
		tree.add(Interval.bounded.create.closed(15, 35))
		tree.add(Interval.bounded.create.open(-20, 15))
		tree.add(Interval.bounded.create.open(0, 16))
		tree.add(Interval.unbounded.leftClosed(32));
		tree.add(Interval.unbounded.rightClosed(17));
		tree.add(Interval.unbounded());

		assert(5 == tree.query(15).size);
		assert(2 == tree.query(-20).size);
		assert(3 == tree.query(-17).size);
		assert(5 == tree.query(11).size);
		assert(3 == tree.query(-8).size);

		tree = new IntervalTree[Int, Unit]();
		tree.add(Interval.bounded.create.open(10, 20))
		tree.add(Interval.bounded.create.open(10, 12))
		tree.add(Interval.bounded.create.closed(-1000, 8))

		assert(2 == tree.query(11).size);
		assert(0 == tree.query(9).size);
		assert(1 == tree.query(0).size);

		tree = new IntervalTree[Int, Unit]();
		tree.add(Interval.unbounded.leftOpen(7450));
		tree.add(Interval.unbounded.rightOpen(209));
		tree.add(Interval.unbounded.rightClosed(2774));

		assert(1 == tree.query(8659).size);

		tree = new IntervalTree[Int, Unit]();
		tree.add(Interval.unbounded.rightOpen(6213));
		tree.add(Interval.unbounded.leftClosed(684));
		tree.add(Interval.bounded.create.open(-4657, -4612))

		assert(1 == tree.query(359).size);

	}





}
