package edu.umass.cs.iesl.watr
package utils.intervals

import org.scalatest._

class IntervalsTest extends FlatSpec with Matchers {


	it should "test_isNotPoint" in {
		val intervals = List(
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
				Interval.bounded.create.closed(9, 20),
				Interval.bounded.create.closed(-1, 1),
				Interval.bounded.create.closed(-123123, -4),
				Interval.unbounded[Int]()
		)
		for (interval <- intervals)
			assert(!interval.isPoint());
	}

	//
	// it should "test_isPoint" in {
	// 	val intervals = List(
	// 			Interval.bounded.create.closed(22, 22),
	// 			Interval.bounded.create.closed(-11, -11),
	// 			Interval.bounded.create.closed(0, 0)
	// 	)
	// 	for (interval <- intervals)
	// 		assert(interval.isPoint());
	// }

	//
	// it should "test_isEmpty" in {
	// 	val intervals = List(
	// 			Interval.bounded.create.closed(2, 1),
	// 			Interval.bounded.create.leftClosedRightOpen(2, 1),
	// 			Interval.bounded.create.leftOpenRightClosed(2, 1),
	// 			Interval.bounded.create.open(2, 1), // 			Interval.bounded.create.leftClosedRightOpen(2, 2),
	// 			Interval.bounded.create.leftOpenRightClosed(2, 2),
	// 			Interval.bounded.create.open(2, 2)
	// 	)
	// 	for (interval <- intervals)
	// 		assert(interval.isEmpty());
	// }

	//
	// it should "test_isNotEmpty" in {
	// 	val intervals = List(
	// 			Interval.bounded.create.closed(2, 2),
	// 			Interval.unbounded.leftClosed(2),
	// 			Interval.unbounded.rightClosed(2),
	// 			Interval.unbounded.rightOpen(2),
	// 			Interval.unbounded.leftOpen(2),
	// 			Interval.bounded.create.closed(9, 20),
	// 			Interval.bounded.create.closed(-1, 1),
	// 			Interval.bounded.create.closed(-123123, -4),
	// 			Interval.unbounded[Int]()
	// 	)
	// 	for (interval <- intervals)
	// 		assert(!interval.isEmpty());
	// }

	//
	// it should "test_hashSet" in {
	// 	HashSet<IntMock> set = new HashSet<>();
	// 	set.add(Interval.bounded.create.closed(1, 10));
	// 	set.add(Interval.bounded.create.open(1, 10));
	// 	set.add(Interval.bounded.create.leftClosedRightOpen(1, 10));
	// 	set.add(Interval.bounded.create.leftOpenRightClosed(1, 10));
	// 	set.add(Interval.unbounded.leftClosed(1));
	// 	set.add(Interval.unbounded.rightClosed(1));
	// 	set.add(Interval.unbounded.leftOpen(1));
	// 	set.add(Interval.unbounded.rightOpen(1));
	// 	assertEquals(8, set.size());
	// }

	//
	// it should "test_equalSameEndPointsDifferentTypes" in {
	// 	IntMock a = Interval.bounded.create.closed(1, 10);
	// 	IntMock b = Interval.bounded.create.open(1, 10);
	// 	IntMock c = Interval.bounded.create.leftClosedRightOpen(1, 10);
	// 	IntMock d = Interval.bounded.create.leftOpenRightClosed(1, 10);

	// 	assertNotEquals(a, b);
	// 	assertNotEquals(a, c);
	// 	assertNotEquals(a, d);
	// 	assertNotEquals(b, c);
	// 	assertNotEquals(b, d);
	// 	assertNotEquals(c, d);

	// 	assertNotEquals(b, a);
	// 	assertNotEquals(c, a);
	// 	assertNotEquals(d, a);
	// 	assertNotEquals(c, b);
	// 	assertNotEquals(d, b);
	// 	assertNotEquals(d, c);

	// 	assert(!a.equals(null));
	// 	assert(!a.equals(Interval.unbounded[Int]()));

	// 	assert(!a.equals(Interval.bounded.create.open(99, 129)));
	// }

	//
	// it should "test_equalSame" in {
	// 	IntMock a = Interval.bounded.create.closed(1, 10);
	// 	IntMock b = Interval.bounded.create.open(1, 10);
	// 	IntMock c = Interval.bounded.create.leftClosedRightOpen(1, 10);
	// 	IntMock d = Interval.bounded.create.leftOpenRightClosed(1, 10);

	// 	IntMock aa = Interval.bounded.create.closed(1, 10);
	// 	IntMock bb = Interval.bounded.create.open(1, 10);
	// 	IntMock cc = Interval.bounded.create.leftClosedRightOpen(1, 10);
	// 	IntMock dd = Interval.bounded.create.leftOpenRightClosed(1, 10);

	// 	IntMock e = Interval.unbounded.leftClosed(20);
	// 	IntMock f = Interval.unbounded.rightClosed(20);
	// 	IntMock g = Interval.unbounded.leftOpen(20);
	// 	IntMock h = Interval.unbounded.rightOpen(20);

	// 	IntMock ee = Interval.unbounded.leftClosed(20);
	// 	IntMock ff = Interval.unbounded.rightClosed(20);
	// 	IntMock gg = Interval.unbounded.leftOpen(20);
	// 	IntMock hh = Interval.unbounded.rightOpen(20);

	// 	assert(a.equals(aa));
	// 	assertEquals(b, bb);
	// 	assertEquals(c, cc);
	// 	assertEquals(d, dd);
	// 	assertEquals(e, ee);
	// 	assertEquals(f, ff);
	// 	assertEquals(g, gg);
	// 	assertEquals(h, hh);
	// }

	//
	// public void nothingContainsDegenerate" in {
	// 	IntMock[] arr = Interval.unbounded.yyy[]{
	// 			Interval.bounded.create.open(0, 5),
	// 			Interval.bounded.create.closed(0, 5),
	// 			Interval.bounded.create.leftOpenRightClosed(0, 5),
	// 			Interval.bounded.create.leftClosedRightOpen(0, 5),
	// 			Interval.unbounded.leftClosed(0),
	// 			Interval.unbounded.rightClosed(0),
	// 			Interval.unbounded.leftOpen(0),
	// 			Interval.unbounded.rightOpen(0),
	// 			Interval.bounded.create.open(11, 10)
	// 	)

	// 	for (IntMock a: arr) {
	// 		assert(!a.contains(Interval.unbounded[Int]()));
	// 		assert(!a.contains((Integer) null));
	// 		assert(!a.contains((Interval<Integer>) null));
	// 		assert(!a.contains(Interval.bounded.create.open(30, 20)));
	// 	}
	// }

	//
	// it should "test_emptyContainsNothing" in {
	// 	IntMock a = Interval.bounded.create.closed(11, 10);

	// 	assert(!a.contains(Interval.bounded.create.open(11, 10)));
	// 	assert(!a.contains(Interval.bounded.create.leftOpenRightClosed(11, 10)));
	// 	assert(!a.contains(Interval.bounded.create.leftClosedRightOpen(11, 10)));
	// 	assert(!a.contains(Interval.bounded.create.closed(11, 10)));

	// 	assert(!a.contains(Interval.bounded.create.open(0, 5)));
	// 	assert(!a.contains(Interval.bounded.create.leftOpenRightClosed(0, 20)));
	// 	assert(!a.contains(Interval.bounded.create.leftClosedRightOpen(15, 30)));

	// 	assert(!a.contains(Interval.unbounded.leftClosed(0)));
	// 	assert(!a.contains(Interval.unbounded.rightClosed(0)));
	// 	assert(!a.contains(Interval.unbounded.leftOpen(0)));
	// 	assert(!a.contains(Interval.unbounded.rightOpen(0)));

	// 	assert(!a.contains(Interval.unbounded.leftClosed(100)));
	// 	assert(!a.contains(Interval.unbounded.rightClosed(100)));
	// 	assert(!a.contains(Interval.unbounded.leftOpen(100)));
	// 	assert(!a.contains(Interval.bounded.create.rightOpen(100)));

	// 	assert(!a.contains(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.contains((Interval<Integer>)null));
	// }

	//
	// it should "test_emptyIntersectsNothing" in {
	// 	IntMock a = Interval.bounded.create.closed(11, 10);

	// 	assert(!a.intersects(Interval.bounded.create.open(11, 10)));
	// 	assert(!a.intersects(Interval.bounded.create.leftOpenRightClosed(11, 10)));
	// 	assert(!a.intersects(Interval.bounded.create.leftClosedRightOpen(11, 10)));
	// 	assert(!a.intersects(Interval.bounded.create.closed(11, 10)));

	// 	assert(!a.intersects(Interval.bounded.create.open(0, 5)));
	// 	assert(!a.intersects(Interval.bounded.create.leftOpenRightClosed(0, 20)));
	// 	assert(!a.intersects(Interval.bounded.create.leftClosedRightOpen(15, 30)));

	// 	assert(!a.intersects(Interval.xxxbounded.create.leftClosed(0)));
	// 	assert(!a.intersects(Interval.xxxbounded.create.rightClosed(0)));
	// 	assert(!a.intersects(Interval.xxxbounded.create.leftOpen(0)));
	// 	assert(!a.intersects(Interval.xxxbounded.create.rightOpen(0)));

	// 	assert(!a.intersects(Interval.xxxbounded.create.leftClosed(100)));
	// 	assert(!a.intersects(Interval.xxxbounded.create.rightClosed(100)));
	// 	assert(!a.intersects(Interval.xxxbounded.create.leftOpen(100)));
	// 	assert(!a.intersects(Interval.xxxbounded.create.rightOpen(100)));

	// 	assert(!a.intersects(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.intersects((Interval<Integer>)null));
	// }

	//
	// public void getIntersectionCommonLeft" in {
	// 	IntMock a = Interval.bounded.create.open(0, 10);
	// 	assertEquals(Interval.bounded.create.open(0, 3), a.getIntersection(Interval.bounded.create.open(0, 3)));
	// 	assertEquals(Interval.bounded.create.open(0, 10), a.getIntersection(Interval.bounded.create.closed(0, 20)));
	// 	assertEquals(Interval.bounded.create.leftOpenRightClosed(0, 4), a.getIntersection(Interval.bounded.create.leftOpenRightClosed(0, 4)));

	// 	a = Interval.bounded.create.closed(0, 10);
	// 	assertEquals(Interval.bounded.create.open(0, 3), a.getIntersection(Interval.bounded.create.open(0, 3)));
	// 	assertEquals(Interval.bounded.create.closed(0, 10), a.getIntersection(Interval.bounded.create.leftClosedRightOpen(0, 20)));
	// 	assertEquals(Interval.bounded.create.leftOpenRightClosed(0, 4), a.getIntersection(Interval.bounded.create.leftOpenRightClosed(0, 4)));
	// }

	//
	// public void getIntersectionCommonRight" in {
	// 	IntMock a = Interval.bounded.create.open(0, 10);
	// 	assertEquals(Interval.bounded.create.open(4, 10), a.getIntersection(Interval.bounded.create.open(4, 10)));
	// 	assertEquals(Interval.bounded.create.open(0, 10), a.getIntersection(Interval.bounded.create.closed(-10, 10)));
	// 	assertEquals(Interval.bounded.create.leftClosedRightOpen(2, 10), a.getIntersection(Interval.bounded.create.leftClosedRightOpen(2, 10)));

	// 	a = Interval.bounded.create.closed(0, 10);
	// 	assertEquals(Interval.bounded.create.open(4, 10), a.getIntersection(Interval.bounded.create.open(4, 10)));
	// 	assertEquals(Interval.bounded.create.closed(0, 10), a.getIntersection(Interval.bounded.create.closed(-10, 10)));
	// 	assertEquals(Interval.bounded.create.leftClosedRightOpen(2, 10), a.getIntersection(Interval.bounded.create.leftClosedRightOpen(2, 10)));
	// }

	//
	// public void getIntersectionInfinityWithPoint" in {
	// 	IntMock a = Interval.bounded.create.closed(15, 15);
	// 	IntMock b = Interval.xxxbounded.create.yyy();

	// 	assertEquals(a, a.getIntersection(b));
	// 	assertEquals(a, b.getIntersection(a));
	// }

	//
	// public void getIntersectionWithNull" in {
	// 	assertNull(Interval.xxxbounded.create.yyy().getIntersection(null));
	// 	assertNull(Interval.bounded.create.open(1, 2).getIntersection(null));
	// 	assertNull(Interval.bounded.create.closed(1, 2).getIntersection(null));
	// 	assertNull(Interval.xxxbounded.create.leftClosed(1).getIntersection(null));
	// 	assertNull(Interval.xxxbounded.create.rightClosed(1).getIntersection(null));
	// }

	//
	// public void getIntersectionWithPoint" in {
	// 	IntMock a = Interval.bounded.create.open(2, 20);
	// 	assertNull(a.getIntersection(Interval.bounded.create.closed(2, 2)));
	// 	assertNull(a.getIntersection(Interval.bounded.create.closed(20, 20)));
	// 	assertEquals(Interval.bounded.create.closed(5, 5), a.getIntersection(Interval.bounded.create.closed(5, 5)));

	// 	a = Interval.bounded.create.closed(2, 20);
	// 	assertEquals(Interval.bounded.create.closed(2, 2), a.getIntersection(Interval.bounded.create.closed(2, 2)));
	// 	assertEquals(Interval.bounded.create.closed(20, 20), a.getIntersection(Interval.bounded.create.closed(20, 20)));

	// 	a = Interval.bounded.create.leftClosedRightOpen(2, 20);
	// 	assertEquals(Interval.bounded.create.closed(2, 2), a.getIntersection(Interval.bounded.create.closed(2, 2)));
	// 	assertNull(a.getIntersection(Interval.bounded.create.closed(20, 20)));

	// 	a = Interval.bounded.create.leftOpenRightClosed(2, 20);
	// 	assertNull(a.getIntersection(Interval.bounded.create.closed(2, 2)));
	// 	assertEquals(Interval.bounded.create.closed(20, 20), a.getIntersection(Interval.bounded.create.closed(20, 20)));
	// }

	//
	// public void boundedContainsGenericPoint" in {
	// 	IntMock a = Interval.bounded.create.open(2, 20);
	// 	assert(a.contains(5));
	// 	assert(!a.contains(2));
	// 	assert(!a.contains(20));
	// 	assert(!a.contains(21));
	// 	assert(a.contains(3));
	// 	assert(a.contains(19));
	// 	assert(!a.contains(1));

	// 	a = Interval.bounded.create.closed(2, 20);
	// 	assert(a.contains(6));
	// 	assert(a.contains(2));
	// 	assert(a.contains(20));
	// 	assert(!a.contains(21));
	// 	assert(a.contains(3));
	// 	assert(a.contains(19));
	// 	assert(!a.contains(1));

	// 	a = Interval.bounded.create.leftClosedRightOpen(2, 20);
	// 	assert(a.contains(6));
	// 	assert(a.contains(2));
	// 	assert(!a.contains(20));
	// 	assert(!a.contains(21));
	// 	assert(a.contains(3));
	// 	assert(a.contains(19));
	// 	assert(!a.contains(1));

	// 	a = Interval.bounded.create.leftOpenRightClosed(2, 20);
	// 	assert(a.contains(6));
	// 	assert(!a.contains(2));
	// 	assert(a.contains(20));
	// 	assert(!a.contains(21));
	// 	assert(a.contains(3));
	// 	assert(a.contains(19));
	// 	assert(!a.contains(1));
	// }

	//
	// public void unBoundedContainsGenericPoint" in {
	// 	IntMock a = Interval.xxxbounded.create.leftClosed(2);
	// 	assert(a.contains(2));
	// 	assert(a.contains(3));
	// 	assert(!a.contains(1));

	// 	a = Interval.xxxbounded.create.leftOpen(2);
	// 	assert(!a.contains(2));
	// 	assert(a.contains(3));
	// 	assert(!a.contains(1));

	// 	a = Interval.xxxbounded.create.rightClosed(2);
	// 	assert(a.contains(2));
	// 	assert(!a.contains(3));
	// 	assert(a.contains(1));

	// 	a = Interval.xxxbounded.create.rightOpen(2);
	// 	assert(!a.contains(2));
	// 	assert(!a.contains(3));
	// 	assert(a.contains(1));
	// }

	//
	// public void emptyContainsGenericPoint" in {
	// 	assert(!Interval.xxxbounded.create.open(10, 0).contains(5));
	// 	assert(!Interval.xxxbounded.create.open(10, 0).contains(10));
	// 	assert(!Interval.xxxbounded.create.open(10, 0).contains(0));
	// 	assert(!Interval.xxxbounded.create.open(10, 0).contains(20));
	// 	assert(!Interval.xxxbounded.create.open(10, 0).contains(-5));
	// }

	//
	// public void sortByStarts" in {
	// 	IntMock[] arr = Interval.xxxbounded.create.yyy[]{
	// 			Interval.xxxbounded.create.open(0, 5),
	// 			Interval.xxxbounded.create.leftOpenRightClosed(0, 5),
	// 			Interval.xxxbounded.create.leftClosedRightOpen(0, 5),
	// 			Interval.xxxbounded.create.closed(0, 5),
	// 			Interval.xxxbounded.create.leftClosed(0),
	// 			Interval.xxxbounded.create.rightClosed(0),
	// 			Interval.xxxbounded.create.rightOpen(0),
	// 			Interval.xxxbounded.create.leftOpen(0),
	// 			Interval.xxxbounded.create.leftClosed(5),
	// 			Interval.xxxbounded.create.rightClosed(5),
	// 			Interval.xxxbounded.create.rightOpen(5),
	// 			Interval.xxxbounded.create.leftOpen(5)
	// 	)

	// 	IntMock[] expected = Interval.xxxbounded.create.yyy[]{
	// 			Interval.xxxbounded.create.rightOpen(0),
	// 			Interval.xxxbounded.create.rightClosed(0),
	// 			Interval.xxxbounded.create.rightOpen(5),
	// 			Interval.xxxbounded.create.rightClosed(5),
	// 			Interval.xxxbounded.create.leftClosedRightOpen(0, 5),
	// 			Interval.xxxbounded.create.closed(0, 5),
	// 			Interval.xxxbounded.create.leftClosed(0),
	// 			Interval.xxxbounded.create.open(0, 5),
	// 			Interval.xxxbounded.create.leftOpenRightClosed(0, 5),
	// 			Interval.xxxbounded.create.leftOpen(0),
	// 			Interval.xxxbounded.create.leftClosed(5),
	// 			Interval.xxxbounded.create.leftOpen(5)
	// 	)

	// 	Arrays.sort(arr, Interval.sweepLeftToRight);
	// 	assertArrayEquals(expected, arr);
	// }

	//
	// public void sortByEnds" in {
	// 	IntMock[] arr = Interval.xxxbounded.create.rightOpen[]{
	// 			Interval.xxxbounded.create.open(0, 5),
	// 			Interval.xxxbounded.create.leftOpenRightClosed(0, 5),
	// 			Interval.xxxbounded.create.leftClosedRightOpen(0, 5),
	// 			Interval.xxxbounded.create.closed(0, 5),
	// 			Interval.xxxbounded.create.leftClosed(0),
	// 			Interval.xxxbounded.create.rightClosed(0),
	// 			Interval.xxxbounded.create.rightOpen(0),
	// 			Interval.xxxbounded.create.leftOpen(0),
	// 			Interval.xxxbounded.create.leftClosed(5),
	// 			Interval.xxxbounded.create.rightClosed(5),
	// 			Interval.xxxbounded.create.rightOpen(5),
	// 			Interval.xxxbounded.create.leftOpen(5)
	// 	)

	// 	IntMock[] expected = Interval.xxxbounded.create.rightOpen[]{
	// 			Interval.xxxbounded.create.leftOpen(5),
	// 			Interval.xxxbounded.create.leftClosed(5),
	// 			Interval.xxxbounded.create.leftOpen(0),
	// 			Interval.xxxbounded.create.leftClosed(0),
	// 			Interval.xxxbounded.create.leftOpenRightClosed(0, 5),
	// 			Interval.xxxbounded.create.closed(0, 5),
	// 			Interval.xxxbounded.create.rightClosed(5),
	// 			Interval.xxxbounded.create.open(0, 5),
	// 			Interval.xxxbounded.create.leftClosedRightOpen(0, 5),
	// 			Interval.xxxbounded.create.rightClosed(5),
	// 			Interval.xxxbounded.create.open(0),
	// 			Interval.xxxbounded.create.open(0)
	// 	)

	// 	Arrays.sort(arr, Interval.sweepRightToLeft);
	// 	for (int i=0; i<arr.length; i++)
	// 		assertEquals(arr[i], expected[i]);
	// 	assertArrayEquals(expected, arr);
	// }

	//
	// public void equalsOtherClasses" in {
	// 	IntMock a = Interval.xxxbounded.create.closed(5, 10);
	// 	assertNotEquals(a, new Integer(5));
	// 	assertEquals(a, new IntegerInterval(5, 10));
	// 	assertNotEquals(a, new IntegerInterval(5, 11));
	// 	assertNotEquals(a, new Date());
	// }

	//
	// public void builderWithOnlyOneMethod() {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);
	// 	assertEquals(Interval.xxxbounded.create.leftOpen(2), a.builder().greater(2).build());
	// 	assertEquals(Interval.xxxbounded.create.leftClosed(2), a.builder().greaterEqual(2).build());
	// 	assertEquals(Interval.xxxbounded.create.rightOpen(2), a.builder().less(2).build());
	// 	assertEquals(Interval.xxxbounded.create.rightClosed(2), a.builder().lessEqual(2).build());
	// }

	//
	// public void builderWithTwoMethods() {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);
	// 	assertEquals(Interval.xxxbounded.create.open(2, 10), a.builder().greater(2).less(10).build());
	// 	assertEquals(Interval.xxxbounded.create.leftOpenRightClosed(2, 10), a.builder().greater(2).lessEqual(10).build());

	// 	assertEquals(Interval.xxxbounded.create.leftClosedRightOpen(2, 10), a.builder().greaterEqual(2).less(10).build());
	// 	assertEquals(Interval.xxxbounded.create.closed(2, 10), a.builder().greaterEqual(2).lessEqual(10).build());

	// 	assertEquals(Interval.xxxbounded.create.open(0, 2), a.builder().less(2).greater(0).build());
	// 	assertEquals(Interval.xxxbounded.create.leftClosedRightOpen(0, 2), a.builder().less(2).greaterEqual(0).build());

	// 	assertEquals(Interval.xxxbounded.create.leftOpenRightClosed(0, 2), a.builder().lessEqual(2).greater(0).build());
	// 	assertEquals(Interval.xxxbounded.create.closed(0, 2), a.builder().lessEqual(2).greaterEqual(0).build());
	// }

	//
	// public void builderWithSameMethodTwice() {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);

	// 	assertEquals(Interval.xxxbounded.create.leftOpen(3), a.builder().greater(2).greater(3).build());
	// 	assertEquals(Interval.xxxbounded.create.leftOpen(1), a.builder().greater(2).greater(1).build());
	// 	assertEquals(Interval.xxxbounded.create.leftOpen(2), a.builder().greater(2).greater(2).build());

	// 	assertEquals(Interval.xxxbounded.create.leftClosed(3), a.builder().greaterEqual(2).greaterEqual(3).build());
	// 	assertEquals(Interval.xxxbounded.create.leftClosed(1), a.builder().greaterEqual(2).greaterEqual(1).build());
	// 	assertEquals(Interval.xxxbounded.create.leftClosed(2), a.builder().greaterEqual(2).greaterEqual(2).build());

	// 	assertEquals(Interval.xxxbounded.create.rightOpen(3), a.builder().less(2).less(3).build());
	// 	assertEquals(Interval.xxxbounded.create.rightOpen(1), a.builder().less(2).less(1).build());
	// 	assertEquals(Interval.xxxbounded.create.rightOpen(2), a.builder().less(2).less(2).build());

	// 	assertEquals(Interval.xxxbounded.create.rightClosed(3), a.builder().lessEqual(2).lessEqual(3).build());
	// 	assertEquals(Interval.xxxbounded.create.rightClosed(1), a.builder().lessEqual(2).lessEqual(1).build());
	// 	assertEquals(Interval.xxxbounded.create.rightClosed(2), a.builder().lessEqual(2).lessEqual(2).build());
	// }

	//
	// public void builderWithTwoDifferentMethodsOfSameType() {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);

	// 	assertEquals(Interval.xxxbounded.create.rightOpen(2), a.builder().lessEqual(2).less(2).build());
	// 	assertEquals(Interval.xxxbounded.create.rightClosed(2), a.builder().less(2).lessEqual(2).build());
	// 	assertEquals(Interval.xxxbounded.create.leftOpen(2), a.builder().greaterEqual(2).greater(2).build());
	// 	assertEquals(Interval.xxxbounded.create.leftClosed(2), a.builder().greater(2).greaterEqual(2).build());
	// }

	//
	// public void isLeftOfBoundedInterval" in {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.closed(10, 11)));
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.open(10, 11)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftOpenRightClosed(6, 12)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 7)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(1, 4)));

	// 	a = Interval.xxxbounded.create.closed(5, 10);
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(10, 11)));
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.open(10, 11)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftOpenRightClosed(6, 12)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 7)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(1, 4)));
	// }

	//
	// public void isLeftOfUnboundedInterval" in {
	// 	IntMock a = Interval.xxxbounded.create.leftClosed(5);
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(5, 11)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(5, 11)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 7)));

	// 	a = Interval.xxxbounded.create.leftOpen(5);
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(5, 11)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(5, 11)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 7)));

	// 	a = Interval.xxxbounded.create.rightClosed(5);
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.open(5, 11)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(5, 11)));
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));

	// 	a = Interval.xxxbounded.create.rightOpen(5);
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.open(5, 11)));
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.closed(5, 11)));
	// 	assert(a.isLeftOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));
	// }

	//
	// public void isRightOfInterval" in {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);
	// 	assert(a.isRightOf(Interval.xxxbounded.create.closed(4, 5)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.open(4, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpenRightClosed(6, 12)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 7)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(11, 14)));

	// 	a = Interval.xxxbounded.create.closed(5, 10);
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(4, 5)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.open(4, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpenRightClosed(6, 12)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 7)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(11, 14)));
	// }

	//
	// public void isRightOfUnboundedInterval" in {
	// 	IntMock a = Interval.xxxbounded.create.leftClosed(5);
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosed(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.rightClosed(5)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.rightOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(5, 11)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(5, 11)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 3)));

	// 	a = Interval.xxxbounded.create.leftOpen(5);
	// 	assert(a.isRightOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosed(5)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.rightClosed(5)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.rightOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(5, 11)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(5, 11)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));
	// 	assert(a.isRightOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 3)));

	// 	a = Interval.xxxbounded.create.rightClosed(5);
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(5, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(5, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosed(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.rightClosed(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.rightOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 3)));

	// 	a = Interval.xxxbounded.create.rightOpen(5);
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(5, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(5, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosed(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.rightClosed(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.rightOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpen(5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(0, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(0, 5)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.closed(6, 8)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftOpenRightClosed(4, 12)));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.leftClosedRightOpen(1, 3)));
	// }

	//
	// public void isRightOfDegenerate" in {
	// 	IntMock a = Interval.xxxbounded.create.leftClosed(5);
	// 	assert(!a.isRightOf((Interval<Integer>)null));
	// 	assert(!a.isRightOf((Integer)null));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(20, 10)));

	// 	a = Interval.xxxbounded.create.rightClosed(5);
	// 	assert(!a.isRightOf((Interval<Integer>)null));
	// 	assert(!a.isRightOf((Integer)null));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(20, 10)));

	// 	a = Interval.xxxbounded.create.leftOpen(5);
	// 	assert(!a.isRightOf((Interval<Integer>)null));
	// 	assert(!a.isRightOf((Integer)null));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(20, 10)));

	// 	a = Interval.xxxbounded.create.rightOpen(5);
	// 	assert(!a.isRightOf((Interval<Integer>)null));
	// 	assert(!a.isRightOf((Integer)null));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isRightOf(Interval.xxxbounded.create.open(20, 10)));
	// }

	//
	// public void isLeftOfDegenerate" in {
	// 	IntMock a = Interval.xxxbounded.create.leftClosed(5);
	// 	assert(!a.isLeftOf((Interval<Integer>)null));
	// 	assert(!a.isLeftOf((Integer)null));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(20, 10)));

	// 	a = Interval.xxxbounded.create.rightClosed(5);
	// 	assert(!a.isLeftOf((Interval<Integer>)null));
	// 	assert(!a.isLeftOf((Integer)null));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(20, 10)));

	// 	a = Interval.xxxbounded.create.leftOpen(5);
	// 	assert(!a.isLeftOf((Interval<Integer>)null));
	// 	assert(!a.isLeftOf((Integer)null));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(20, 10)));

	// 	a = Interval.xxxbounded.create.rightOpen(5);
	// 	assert(!a.isLeftOf((Interval<Integer>)null));
	// 	assert(!a.isLeftOf((Integer)null));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.yyy()));
	// 	assert(!a.isLeftOf(Interval.xxxbounded.create.open(20, 10)));
	// }

	//
	// public void isLeftOfPoint" in {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);
	// 	assert(a.isLeftOf(10));
	// 	assert(a.isLeftOf(11));
	// 	assert(!a.isLeftOf(9));
	// 	assert(!a.isLeftOf(5));
	// 	assert(!a.isLeftOf(3));

	// 	a = Interval.xxxbounded.create.closed(5, 10);
	// 	assert(!a.isLeftOf(10));
	// 	assert(a.isLeftOf(11));
	// 	assert(!a.isLeftOf(9));
	// 	assert(!a.isLeftOf(5));
	// 	assert(!a.isLeftOf(3));
	// }

	//
	// public void isRightOfPoint" in {
	// 	IntMock a = Interval.xxxbounded.create.open(5, 10);
	// 	assert(a.isRightOf(5));
	// 	assert(a.isRightOf(4));
	// 	assert(!a.isRightOf(6));
	// 	assert(!a.isRightOf(10));
	// 	assert(!a.isRightOf(12));

	// 	a = Interval.xxxbounded.create.closed(5, 10);
	// 	assert(!a.isRightOf(5));
	// 	assert(a.isRightOf(4));
	// 	assert(!a.isRightOf(6));
	// 	assert(!a.isRightOf(10));
	// 	assert(!a.isRightOf(12));
	// }

}
