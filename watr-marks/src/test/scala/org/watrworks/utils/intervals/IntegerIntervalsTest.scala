package org.watrworks
package utils.intervals

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntegerIntervalsTest extends AnyFlatSpec with Matchers {

  it should "order intervals by start point, increasing" in {
    val (st, end) = (1, 1)

    val expectedOrder = List(
      Interval.unbounded.rightOpen(end),
      Interval.unbounded.rightClosed(end),
      Interval.unbounded[Int](),
      Interval.bounded.create.leftClosedRightOpen(st, end),
      Interval.bounded.create.closed(st, end),
      Interval.unbounded.leftClosed(st),
      Interval.bounded.create.open(st, end),
      Interval.bounded.create.leftOpenRightClosed(st, end),
      Interval.unbounded.leftOpen(st),
    )

    val incOrd = MidpointHelper.IntMidpointHelper.increaseIntervalOrdering

    expectedOrder.permutations.take(20).foreach { permutation =>
      val sorted = permutation.sorted(incOrd)
      assert(sorted == expectedOrder)
    }

    val fns = List[Int => Interval[Int, Unit]](
      Interval.unbounded.rightOpen(_),
      Interval.unbounded.rightClosed(_),
      (i => Interval.unbounded[Int]()),
      (i => Interval.bounded.create.leftClosedRightOpen(i, i)),
      (i => Interval.bounded.create.closed(i, i)),
      (i => Interval.unbounded.leftClosed(i)),
      (i => Interval.bounded.create.open(i, i)),
      (i => Interval.bounded.create.leftOpenRightClosed(i, i)),
      (i => Interval.unbounded.leftOpen(i)),
    )

    val applied = fns.zipWithIndex.map{ case (f, i) => f(i) }

    applied.permutations.take(20).foreach { permutation =>
      val sorted = permutation.sorted(incOrd)
      assert(sorted == applied)
    }


  }

  it should "order intervals by end point, decreasing" in {
    val (st, end) = (1, 1)
    val expectedOrder = List(
      Interval.unbounded.leftOpen(st),
      Interval.unbounded.leftClosed(st),
      Interval.unbounded[Int](),
      Interval.bounded.create.leftOpenRightClosed(st, end),
      Interval.bounded.create.closed(st, end),
      Interval.unbounded.rightClosed(end),
      Interval.bounded.create.open(st, end),
      Interval.bounded.create.leftClosedRightOpen(st, end),
      Interval.unbounded.rightOpen(end),
    )
    val ord = MidpointHelper.IntMidpointHelper.decreaseIntervalOrdering
    expectedOrder.permutations.take(20).foreach { permutation =>

      val sorted = permutation.sorted(ord)
      assert(sorted == expectedOrder)
    }

  }



  it should "test_emptyIntervalMidPoint" in {
    val intervals = List(
      Interval.bounded.create.open(1, 2),
      Interval.bounded.create.closed(2, 1),
      Interval.bounded.create.leftClosedRightOpen(2, 1),
      Interval.bounded.create.leftOpenRightClosed(2, 1),
      Interval.bounded.create.closed(2, 1),
      Interval.bounded.create.leftClosedRightOpen(2, 2),
      Interval.bounded.create.leftOpenRightClosed(2, 2),
      Interval.bounded.create.open(2, 2)
    )

    for (interval <- intervals) {
      val mid = interval.getMidpoint()
      assert(interval.getMidpoint().isEmpty);
    }
  }


  it should "test_isNotPoint" in {
    val intervals = List(
        Interval.bounded.create.open(1, 2),
  			Interval.bounded.create.closed(2, 1),
  			Interval.bounded.create.leftClosedRightOpen(2, 1),
  			Interval.bounded.create.leftOpenRightClosed(2, 1),
  			Interval.bounded.create.open(2, 1),
				Interval.bounded.create.leftClosedRightOpen(2, 2),
				Interval.bounded.create.leftOpenRightClosed(2, 2),
				Interval.bounded.create.open(2, 2),
				Interval.unbounded.leftClosed(2),
				Interval.unbounded.rightClosed(2),
				Interval.unbounded.rightOpen(2),
				Interval.unbounded.leftOpen(2),
				Interval.bounded.create.closed(9, 20)
	  )
		for (interval <- intervals)
			assert(! interval.isPoint());
	}


	it should "test_intersectsEndpointClosedRight" in {
		val main = Interval.bounded.create.leftOpenRightClosed(-2897, 19);
		assert(main.intersects(Interval.bounded.create.leftClosedRightOpen(19, 222)));
		assert(main.intersects(Interval.bounded.create.closed(19, 19)));
		assert(main.intersects(Interval.bounded.create.leftOpenRightClosed(18, 19)));

		assert(main.intersects(Interval.unbounded.leftClosed(19)));
		assert(main.intersects(Interval.unbounded.rightClosed(19)));
		assert(main.intersects(Interval.unbounded.rightOpen(19)));
	}


	it should "test_notIntersectsEndpointClosedRight" in {
		val main = Interval.bounded.leftOpenRightClosed(-2897, 19).get;
		assert(! main.intersects(Interval.bounded.leftOpenRightClosed(19, 3874).get));
		assert(! main.intersects(Interval.bounded.open(19, 20).get));

		assert(! main.intersects(Interval.unbounded.leftOpen(19)));
	}


	it should "test_notIntersectsEndpointOpenRight" in {
		val main = Interval.bounded.open(-2897, 19).get;
		assert(! main.intersects(Interval.bounded.leftClosedRightOpen(19, 3874).get));
		assert(! main.intersects(Interval.bounded.closed(19, 20).get));

		assert(! main.intersects(Interval.unbounded.leftOpen(19)));
		assert(! main.intersects(Interval.unbounded.leftClosed(19)));
	}


	it should "test_intersectsEndpointClosedLeft" in {
		val main = Interval.bounded.leftClosedRightOpen(-2897, 19).get;
		assert(main.intersects(Interval.bounded.leftOpenRightClosed(-2899, -2897).get));
		assert(main.intersects(Interval.bounded.closed(-2897, -2897).get));
		assert(main.intersects(Interval.bounded.leftOpenRightClosed(-2897, -2890).get));
		assert(main.intersects(Interval.bounded.leftOpenRightClosed(-47589, -2896).get));

		assert(main.intersects(Interval.unbounded.rightClosed(-2897)));
		assert(main.intersects(Interval.unbounded.leftOpen(-2897)));
		assert(main.intersects(Interval.unbounded.leftClosed(-2897)));
	}


	it should "test_notIntersectsEndpointClosedLeft" in {
		val main = Interval.bounded.create.leftClosedRightOpen(-2897, 19);

    assert(! main.intersects(Interval.bounded.create.leftClosedRightOpen(-47589, -2897)));

		assert(! main.intersects(Interval.bounded.create.open(-47589, -2897)));
		assert(! main.intersects(Interval.unbounded.rightOpen(-2897)));
	}


	it should "test_notIntersectsEndpointOpenLeft" in {
		val main = Interval.bounded.create.leftOpenRightClosed(-2897, 19);
		assert(! main.intersects(Interval.bounded.create.leftOpenRightClosed(-47589, -2897)));
		assert(! main.intersects(Interval.bounded.create.closed(-47589, -2897)));
		assert(! main.intersects(Interval.bounded.create.open(-47589, -2897)));
		assert(! main.intersects(Interval.bounded.create.leftClosedRightOpen(-47589, -2897)));

		assert(! main.intersects(Interval.unbounded.rightOpen(-2897)));
		assert(! main.intersects(Interval.unbounded.rightClosed(-2897)));
	}


	it should "test_intersectsOffByOne" in {
		val main = Interval.bounded.create.closed(-2897, 19);
		assert(main.intersects(Interval.bounded.create.leftClosedRightOpen(-47589, -2896)));
		assert(main.intersects(Interval.bounded.create.closed(18, 20)));

		assert(! main.intersects(Interval.bounded.create.closed(20, 21)));
		assert(! main.intersects(Interval.bounded.create.open(20, 21)));
		assert(! main.intersects(Interval.bounded.create.leftOpenRightClosed(20, 21)));
		assert(! main.intersects(Interval.bounded.create.leftClosedRightOpen(20, 21)));
		assert(! main.intersects(Interval.bounded.create.closed(-3000, -2898)));
		assert(! main.intersects(Interval.bounded.create.open(-3000, -2898)));
		assert(! main.intersects(Interval.bounded.create.leftOpenRightClosed(-3000, -2898)));
		assert(! main.intersects(Interval.bounded.create.leftClosedRightOpen(-3000, -2898)));
	}


	it should "test_intersectionExistsButEmpty" in {
		val main = Interval.bounded.create.leftOpenRightClosed(-2897, 19);
		val other = Interval.bounded.create.open(-47589, -2896);
		assert(! main.intersects(other));
		assert(null == main.getIntersection(other));
		assert(null == other.getIntersection(main));
	}


	it should "test_intersectsFullyContained" in {
		val main = Interval.bounded.create.leftOpenRightClosed(-2897, 19);
		assert(main.intersects(Interval.bounded.create.open(-15, 18)));
		assert(main.intersects(Interval.bounded.create.closed(-100, -100)));
	}


	it should "test_intersectsPoint" in {
		val main = Interval.bounded.create.closed(105, 105);

		assert(! main.intersects(Interval.bounded.create.leftOpenRightClosed(-90, 0)));
		assert(true  == main.intersects(Interval.bounded.create.leftOpenRightClosed(33, Integer.MAX_VALUE )));
		assert(true  == main.intersects(Interval.bounded.create.leftOpenRightClosed(33, 105               )));
		assert(false == main.intersects(Interval.bounded.create.leftOpenRightClosed(33, 104               )));
		assert(true  == main.intersects(Interval.bounded.create.leftOpenRightClosed(100, 200              )));
		assert(true  == main.intersects(Interval.bounded.create.leftClosedRightOpen(105, 180              )));
		assert(false == main.intersects(Interval.bounded.create.leftOpenRightClosed(105, 180              )));
		assert(true  == main.intersects(Interval.bounded.create.closed(105, 105                           )));
		assert(false == main.intersects(Interval.bounded.create.closed(106, 106                           )));
		assert(false == main.intersects(Interval.bounded.create.closed(104, 104                           )));
	}


	it should "openLeftContainsOtherWithCommonLeftEndpoint" in {
		val main = Interval.bounded.create.leftOpenRightClosed(-7392, -42);

		assert( false == main.contains(Interval.bounded.create.open(-7392, 15                  )));
		assert( false == main.contains(Interval.bounded.create.closed(-7392, 15                )));
		assert( false == main.contains(Interval.bounded.create.leftClosedRightOpen(-7392, 15   )));
		assert( false == main.contains(Interval.bounded.create.leftOpenRightClosed(-7392, 15   )));
		assert(main.contains(Interval.bounded.create.open(-7392, -100)));
		assert( false == main.contains(Interval.bounded.create.leftClosedRightOpen(-7392, -100 )));
		assert( true  == main.contains(Interval.bounded.create.leftOpenRightClosed(-7392, -100 )));
		assert( false == main.contains(Interval.bounded.create.closed(-7392, -100              )));
	}


	it should "closedLeftContainsOtherWithCommonLeftEndpoint" in {
		val main = Interval.bounded.create.leftClosedRightOpen(-7392, -42);

		assert( false == main.contains(Interval.bounded.create.open(-7392, 15                  )));
		assert( false == main.contains(Interval.bounded.create.closed(-7392, 15                )));
		assert( false == main.contains(Interval.bounded.create.leftOpenRightClosed(-7392, 15   )));
		assert( true  == main.contains(Interval.bounded.create.open(-7392, -100                )));
		assert( true  == main.contains(Interval.bounded.create.leftClosedRightOpen(-7392, -100 )));
		assert( true  == main.contains(Interval.bounded.create.leftOpenRightClosed(-7392, -100 )));
		assert( true  == main.contains(Interval.bounded.create.closed(-7392, -100              )));
		assert( true  == main.contains(Interval.bounded.create.open(-7391, -100                )));
	}


	it should "openRightContainsOtherWithCommonRightEndpoint" in {
		val main = Interval.bounded.create.leftClosedRightOpen(-7392, 42);

		assert(false == main.contains(Interval.bounded.create.open(-10000, 42)));
		assert(false == main.contains(Interval.bounded.create.closed(-10000, 42)));
		assert(false == main.contains(Interval.bounded.create.leftClosedRightOpen(-10000, 42)));
		assert(false == main.contains(Interval.bounded.create.leftOpenRightClosed(-10000, 42)));
		assert(true == main.contains(Interval.bounded.create.open(0, 42)));
		assert(true == main.contains(Interval.bounded.create.leftClosedRightOpen(0, 42)));
		assert(false == main.contains(Interval.bounded.create.leftOpenRightClosed(0, 42)));
		assert(false == main.contains(Interval.bounded.create.closed(0, 42)));
	}


	it should "closedRightContainsOtherWithCommonRightEndpoint" in {
		val main = Interval.bounded.create.leftOpenRightClosed(-7392, 42);

		assert(false == main.contains(Interval.bounded.create.open(-10000, 42)));
		assert(false == main.contains(Interval.bounded.create.closed(-10000, 42)));
		assert(false == main.contains(Interval.bounded.create.leftClosedRightOpen(-10000, 42)));
		assert(false == main.contains(Interval.bounded.create.leftOpenRightClosed(-10000, 42)));
		assert(true == main.contains(Interval.bounded.create.open(0, 42)));
		assert(true == main.contains(Interval.bounded.create.leftClosedRightOpen(0, 42)));
		assert(true == main.contains(Interval.bounded.create.leftOpenRightClosed(0, 42)));
		assert(true == main.contains(Interval.bounded.create.closed(0, 42)));
	}


	it should "openLeftContainsOffByOne" in {
		val main = Interval.bounded.create.leftOpenRightClosed(-7392, -42);

		assert(false == main.contains(Interval.bounded.create.open(-7393, -100)));
		assert(true == main.contains(Interval.bounded.create.closed(-7391, -100)));
		assert(true == main.contains(Interval.bounded.create.leftClosedRightOpen(-7391, -100)));
		assert(true == main.contains(Interval.bounded.create.open(-7391, -100)));
	}


	it should "openRightContainsOffByOne" in {
		val main = Interval.bounded.create.leftClosedRightOpen(-7392, 42);

		assert(false == main.contains(Interval.bounded.create.open(0, 43)));
		assert(false == main.contains(Interval.bounded.create.closed(0, 43)));
		assert(false == main.contains(Interval.bounded.create.leftClosedRightOpen(0, 43)));
		assert(false == main.contains(Interval.bounded.create.leftOpenRightClosed(0, 43)));
		assert(true == main.contains(Interval.bounded.create.closed(0, 41)));
		assert(true == main.contains(Interval.bounded.create.leftClosedRightOpen(0, 41)));
		assert(true == main.contains(Interval.bounded.create.open(0, 41)));
		assert(true == main.contains(Interval.bounded.create.leftOpenRightClosed(0, 41)));
	}


	it should "closedLeftContainsOffByOne" in {
		val main = Interval.bounded.create.leftClosedRightOpen(-7392, -42);

		assert(false == main.contains(Interval.bounded.create.open(-7393, -100)));
		assert(true == main.contains(Interval.bounded.create.closed(-7391, -100)));
		assert(true == main.contains(Interval.bounded.create.leftClosedRightOpen(-7391, -100)));
		assert(true == main.contains(Interval.bounded.create.open(-7391, -100)));
	}


	it should "test_containsSameEndpoints" in {
		var main = Interval.bounded.create.leftClosedRightOpen(10, 20);
		assert(true == main.contains(Interval.bounded.create.open(10, 20)));
		assert(false == main.contains(Interval.bounded.create.closed(10, 20)));
		assert(true == main.contains(Interval.bounded.create.leftClosedRightOpen(10, 20)));
		assert(false == main.contains(Interval.bounded.create.leftOpenRightClosed(10, 20)));

		main = Interval.bounded.create.leftOpenRightClosed(10, 20);
		assert(true == main.contains(Interval.bounded.create.open(10, 20)));
		assert(false == main.contains(Interval.bounded.create.closed(10, 20)));
		assert(false == main.contains(Interval.bounded.create.leftClosedRightOpen(10, 20)));
		assert(true == main.contains(Interval.bounded.create.leftOpenRightClosed(10, 20)));

		main = Interval.bounded.create.closed(10, 20);
		assert(true == main.contains(Interval.bounded.create.open(10, 20)));
		assert(true == main.contains(Interval.bounded.create.closed(10, 20)));
		assert(true == main.contains(Interval.bounded.create.leftClosedRightOpen(10, 20)));
		assert(true == main.contains(Interval.bounded.create.leftOpenRightClosed(10, 20)));

		main = Interval.bounded.create.open(10, 20);
		assert(true == main.contains(Interval.bounded.create.open(10, 20)));
		assert(false == main.contains(Interval.bounded.create.closed(10, 20)));
		assert(false == main.contains(Interval.bounded.create.leftClosedRightOpen(10, 20)));
		assert(false == main.contains(Interval.bounded.create.leftOpenRightClosed(10, 20)));
	}


	it should "test_containsUnboundedClosedLeft" in {
		val main = Interval.unbounded.leftClosed(200); // [200, +inf)
		assert(true == main.contains(Interval.bounded.create.closed(200, 250)));
		assert(true == main.contains(Interval.bounded.create.open(200, 250)));
		assert(true == main.contains(Interval.unbounded.leftClosed(201)));
		assert(true == main.contains(Interval.unbounded.leftClosed(200)));
		assert(false == main.contains(Interval.unbounded.leftClosed(199)));
		assert(false == main.contains(Interval.bounded.create.closed(199, 200)));
	}


	it should "test_containsUnboundedOpenLeft" in {
		val main = Interval.unbounded.leftOpen(200); // (200, +inf)
		assert(false == main.contains(Interval.bounded.create.closed(200, 250)));
		assert(true == main.contains(Interval.bounded.create.open(200, 250)));
		assert(true == main.contains(Interval.unbounded.leftClosed(201)));
		assert(false == main.contains(Interval.unbounded.leftClosed(200)));
		assert(false == main.contains(Interval.unbounded.leftClosed(199)));
		assert(false == main.contains(Interval.bounded.create.closed(199, 200)));
	}


	it should "test_containsUnboundedOpenRight" in {
		val main = Interval.unbounded.rightOpen(81); // (-inf, 81)
		assert(false == main.contains(Interval.bounded.create.closed(10, 81)));
		assert(true == main.contains(Interval.bounded.create.open(15, 81)));
		assert(false == main.contains(Interval.unbounded.leftClosed(10)));
		assert(false == main.contains(Interval.unbounded.rightClosed(81)));
		assert(true == main.contains(Interval.unbounded.rightClosed(80)));
		assert(false == main.contains(Interval.unbounded.rightClosed(82)));
		assert(false == main.contains(Interval.bounded.create.closed(80, 81)));
	}


	it should "test_containsUnboundedClosedRight" in {
		val main = Interval.unbounded.rightClosed(81); // (-inf, 81]
		assert(true == main.contains(Interval.bounded.create.closed(10, 81)));
		assert(true == main.contains(Interval.bounded.create.open(15, 81)));
		assert(false == main.contains(Interval.unbounded.leftClosed(10)));
		assert(true == main.contains(Interval.unbounded.rightClosed(81)));
		assert(true == main.contains(Interval.unbounded.rightClosed(80)));
		assert(false == main.contains(Interval.unbounded.rightClosed(82)));
		assert(true == main.contains(Interval.bounded.create.closed(80, 81)));
	}


	it should "test_containsUnboundedEverything" in {
		val main = Interval.unbounded[Int](); // (-inf, +inf)
		assert(true == main.contains(Interval.unbounded[Int]()));
		assert(true == main.contains(Interval.bounded.create.closed(10, 81)));
		assert(true == main.contains(Interval.bounded.create.open(10, 81)));
		assert(true == main.contains(Interval.unbounded.leftClosed(10)));
		assert(true == main.contains(Interval.unbounded.leftOpen(10)));
		assert(true == main.contains(Interval.unbounded.rightClosed(10)));
		assert(true == main.contains(Interval.unbounded.rightOpen(10)));
	}



}
