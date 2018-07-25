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
  it should "test_isPoint" in {
    val intervals = List(
      Interval.bounded.create.closed(22, 22),
      Interval.bounded.create.closed(-11, -11),
      Interval.bounded.create.closed(0, 0)
    )
    for (interval <- intervals)
      assert(interval.isPoint());
  }


  it should "test_isEmpty" in {
    val intervals = List(
      Interval.bounded.create.closed(2, 1),
      Interval.bounded.create.leftClosedRightOpen(2, 1),
      Interval.bounded.create.leftOpenRightClosed(2, 1),
      Interval.bounded.create.open(2, 1),
      Interval.bounded.create.leftClosedRightOpen(2, 2),
      Interval.bounded.create.leftOpenRightClosed(2, 2),
      Interval.bounded.create.open(2, 2)
    )
    for (interval <- intervals)
      assert(interval.isEmpty());
  }

  //
  it should "test_isNotEmpty" in {
    val intervals = List(
      Interval.bounded.create.closed(2, 2),
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
      assert(!interval.isEmpty());
  }


  it should "test_hashSet" in {
    val set = Set[Interval[Int]](
      Interval.bounded.create.closed(1, 10),
      Interval.bounded.create.open(1, 10),
      Interval.bounded.create.leftClosedRightOpen(1, 10),
      Interval.bounded.create.leftOpenRightClosed(1, 10),
      Interval.unbounded.leftClosed(1),
      Interval.unbounded.rightClosed(1),
      Interval.unbounded.leftOpen(1),
      Interval.unbounded.rightOpen(1),
    )
    assert(8 == set.size);
  }


  def assertNull(a: Any): Unit = {
    assert(a == null)
  }
  def assertEquals(a: Any, b: Any): Unit = {
    assert(a == b)
  }

  def assertNotEquals(a: Any, b: Any): Unit = {
    assert(a != b)
  }

  it should "test_equalSameEndPointsDifferentTypes" in {
    val a = Interval.bounded.create.closed(1, 10);
    val b = Interval.bounded.create.open(1, 10);
    val c = Interval.bounded.create.leftClosedRightOpen(1, 10);
    val d = Interval.bounded.create.leftOpenRightClosed(1, 10);

    assertNotEquals(a, b);
    assertNotEquals(a, c);
    assertNotEquals(a, d);
    assertNotEquals(b, c);
    assertNotEquals(b, d);
    assertNotEquals(c, d);

    assertNotEquals(b, a);
    assertNotEquals(c, a);
    assertNotEquals(d, a);
    assertNotEquals(c, b);
    assertNotEquals(d, b);
    assertNotEquals(d, c);

    assert(!a.equals(null));
    assert(!a.equals(Interval.unbounded[Int]()));

    assert(!a.equals(Interval.bounded.create.open(99, 129)));
  }

  it should "test_equalSame" in {
    val a = Interval.bounded.create.closed(1, 10);
    val b = Interval.bounded.create.open(1, 10);
    val c = Interval.bounded.create.leftClosedRightOpen(1, 10);
    val d = Interval.bounded.create.leftOpenRightClosed(1, 10);

    val aa = Interval.bounded.create.closed(1, 10);
    val bb = Interval.bounded.create.open(1, 10);
    val cc = Interval.bounded.create.leftClosedRightOpen(1, 10);
    val dd = Interval.bounded.create.leftOpenRightClosed(1, 10);

    val e = Interval.unbounded.leftClosed(20);
    val f = Interval.unbounded.rightClosed(20);
    val g = Interval.unbounded.leftOpen(20);
    val h = Interval.unbounded.rightOpen(20);

    val ee = Interval.unbounded.leftClosed(20);
    val ff = Interval.unbounded.rightClosed(20);
    val gg = Interval.unbounded.leftOpen(20);
    val hh = Interval.unbounded.rightOpen(20);

    assert(a.equals(aa));
    assertEquals(b, bb);
    assertEquals(c, cc);
    assertEquals(d, dd);
    assertEquals(e, ee);
    assertEquals(f, ff);
    assertEquals(g, gg);
    assertEquals(h, hh);
  }


  // it should "nothingContainsDegenerate" in {
  //   val arr = List(
  //     Interval.bounded.create.open(0, 5),
  //     Interval.bounded.create.closed(0, 5),
  //     Interval.bounded.create.leftOpenRightClosed(0, 5),
  //     Interval.bounded.create.leftClosedRightOpen(0, 5),
  //     Interval.unbounded.leftClosed(0),
  //     Interval.unbounded.rightClosed(0),
  //     Interval.unbounded.leftOpen(0),
  //     Interval.unbounded.rightOpen(0),
  //     Interval.bounded.create.open(11, 10)
  //   )

  //   for (a <- arr) {
  //     assert(!a.contains(Interval.unbounded[Int]()));
  //     assert(!a.contains(Interval.bounded.create.open(30, 20)));
  //   }
  // }

  // it should "test_emptyContainsNothing" in {
  // 	val a = Interval.bounded.create.closed(11, 10);

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
  // 	assert(!a.contains(Interval.unbounded.rightOpen(100)));

  // 	assert(!a.contains(Interval.unbounded[Int]()));
  // 	// assert(!a.contains((Interval<Integer>)null));
  // }


  it should "test_emptyIntersectsNothing" in {
    val a = Interval.bounded.create.closed(11, 10);

    assert(!a.intersects(Interval.bounded.create.open(11, 10)));
    assert(!a.intersects(Interval.bounded.create.leftOpenRightClosed(11, 10)));
    assert(!a.intersects(Interval.bounded.create.leftClosedRightOpen(11, 10)));
    assert(!a.intersects(Interval.bounded.create.closed(11, 10)));

    assert(!a.intersects(Interval.bounded.create.open(0, 5)));
    assert(!a.intersects(Interval.bounded.create.leftOpenRightClosed(0, 20)));
    assert(!a.intersects(Interval.bounded.create.leftClosedRightOpen(15, 30)));

    assert(!a.intersects(Interval.unbounded.leftClosed(0)));
    assert(!a.intersects(Interval.unbounded.rightClosed(0)));
    assert(!a.intersects(Interval.unbounded.leftOpen(0)));
    assert(!a.intersects(Interval.unbounded.rightOpen(0)));

    assert(!a.intersects(Interval.unbounded.leftClosed(100)));
    assert(!a.intersects(Interval.unbounded.rightClosed(100)));
    assert(!a.intersects(Interval.unbounded.leftOpen(100)));
    assert(!a.intersects(Interval.unbounded.rightOpen(100)));

    assert(!a.intersects(Interval.unbounded[Int]()));
    // assert(!a.intersects((Interval<Integer>)null));
  }


  it should "getIntersectionCommonLeft" in {
    var a = Interval.bounded.create.open(0, 10);
    assertEquals(Interval.bounded.create.open(0, 3), a.getIntersection(Interval.bounded.create.open(0, 3)));
    assertEquals(Interval.bounded.create.open(0, 10), a.getIntersection(Interval.bounded.create.closed(0, 20)));
    assertEquals(Interval.bounded.create.leftOpenRightClosed(0, 4), a.getIntersection(Interval.bounded.create.leftOpenRightClosed(0, 4)));

    a = Interval.bounded.create.closed(0, 10);
    assertEquals(Interval.bounded.create.open(0, 3), a.getIntersection(Interval.bounded.create.open(0, 3)));
    assertEquals(Interval.bounded.create.closed(0, 10), a.getIntersection(Interval.bounded.create.leftClosedRightOpen(0, 20)));
    assertEquals(Interval.bounded.create.leftOpenRightClosed(0, 4), a.getIntersection(Interval.bounded.create.leftOpenRightClosed(0, 4)));
  }


  it should "getIntersectionCommonRight" in {
    var a = Interval.bounded.create.open(0, 10);
    assertEquals(Interval.bounded.create.open(4, 10), a.getIntersection(Interval.bounded.create.open(4, 10)));
    assertEquals(Interval.bounded.create.open(0, 10), a.getIntersection(Interval.bounded.create.closed(-10, 10)));
    assertEquals(Interval.bounded.create.leftClosedRightOpen(2, 10), a.getIntersection(Interval.bounded.create.leftClosedRightOpen(2, 10)));

    a = Interval.bounded.create.closed(0, 10);
    assertEquals(Interval.bounded.create.open(4, 10), a.getIntersection(Interval.bounded.create.open(4, 10)));
    assertEquals(Interval.bounded.create.closed(0, 10), a.getIntersection(Interval.bounded.create.closed(-10, 10)));
    assertEquals(Interval.bounded.create.leftClosedRightOpen(2, 10), a.getIntersection(Interval.bounded.create.leftClosedRightOpen(2, 10)));
  }


  it should "getIntersectionInfinityWithPoint" in {
    val a = Interval.bounded.create.closed(15, 15);
    val b = Interval.unbounded[Int]();

    assertEquals(a, a.getIntersection(b));
    assertEquals(a, b.getIntersection(a));
  }


  it should "getIntersectionWithNull" in {
    assertNull(Interval.unbounded[Int]().getIntersection(null));
    assertNull(Interval.bounded.create.open(1, 2).getIntersection(null));
    assertNull(Interval.bounded.create.closed(1, 2).getIntersection(null));
    assertNull(Interval.unbounded.leftClosed(1).getIntersection(null));
    assertNull(Interval.unbounded.rightClosed(1).getIntersection(null));
  }


  it should "getIntersectionWithPoint" in {
    var a = Interval.bounded.create.open(2, 20);
    assertNull(a.getIntersection(Interval.bounded.create.closed(2, 2)));
    assertNull(a.getIntersection(Interval.bounded.create.closed(20, 20)));
    assertEquals(Interval.bounded.create.closed(5, 5), a.getIntersection(Interval.bounded.create.closed(5, 5)));

    a = Interval.bounded.create.closed(2, 20);
    assertEquals(Interval.bounded.create.closed(2, 2), a.getIntersection(Interval.bounded.create.closed(2, 2)));
    assertEquals(Interval.bounded.create.closed(20, 20), a.getIntersection(Interval.bounded.create.closed(20, 20)));

    a = Interval.bounded.create.leftClosedRightOpen(2, 20);
    assertEquals(Interval.bounded.create.closed(2, 2), a.getIntersection(Interval.bounded.create.closed(2, 2)));
    assertNull(a.getIntersection(Interval.bounded.create.closed(20, 20)));

    a = Interval.bounded.create.leftOpenRightClosed(2, 20);
    assertNull(a.getIntersection(Interval.bounded.create.closed(2, 2)));
    assertEquals(Interval.bounded.create.closed(20, 20), a.getIntersection(Interval.bounded.create.closed(20, 20)));
  }


  it should "boundedContainsGenericPoint" in {
    var a = Interval.bounded.create.open(2, 20);

    assert(a.contains(5));
    assert(!a.contains(2));
    assert(!a.contains(20));
    assert(!a.contains(21));
    assert(a.contains(3));
    assert(a.contains(19));
    assert(!a.contains(1));

    a = Interval.bounded.create.closed(2, 20);
    assert(a.contains(6));
    assert(a.contains(2));
    assert(a.contains(20));
    assert(!a.contains(21));
    assert(a.contains(3));
    assert(a.contains(19));
    assert(!a.contains(1));

    a = Interval.bounded.create.leftClosedRightOpen(2, 20);
    assert(a.contains(6));
    assert(a.contains(2));
    assert(!a.contains(20));
    assert(!a.contains(21));
    assert(a.contains(3));
    assert(a.contains(19));
    assert(!a.contains(1));

    a = Interval.bounded.create.leftOpenRightClosed(2, 20);
    assert(a.contains(6));
    assert(!a.contains(2));
    assert(a.contains(20));
    assert(!a.contains(21));
    assert(a.contains(3));
    assert(a.contains(19));
    assert(!a.contains(1));
  }


  it should "unBoundedContainsGenericPoint" in {
    var a = Interval.unbounded.leftClosed(2);
    assert(a.contains(2));
    assert(a.contains(3));
    assert(!a.contains(1));

    a = Interval.unbounded.leftOpen(2);
    assert(!a.contains(2));
    assert(a.contains(3));
    assert(!a.contains(1));

    a = Interval.unbounded.rightClosed(2);
    assert(a.contains(2));
    assert(!a.contains(3));
    assert(a.contains(1));

    a = Interval.unbounded.rightOpen(2);
    assert(!a.contains(2));
    assert(!a.contains(3));
    assert(a.contains(1));
  }


  it should "emptyContainsGenericPoint" in {
    assert(!Interval.bounded.create.open(10, 0).contains(5));
    assert(!Interval.bounded.create.open(10, 0).contains(10));
    assert(!Interval.bounded.create.open(10, 0).contains(0));
    assert(!Interval.bounded.create.open(10, 0).contains(20));
    assert(!Interval.bounded.create.open(10, 0).contains(-5));
  }


  it should "sortByStarts" in {
    val arr = List(
      Interval.bounded.create.open(0, 5),
      Interval.bounded.create.leftOpenRightClosed(0, 5),
      Interval.bounded.create.leftClosedRightOpen(0, 5),
      Interval.bounded.create.closed(0, 5),
      Interval.unbounded.leftClosed(0),
      Interval.unbounded.rightClosed(0),
      Interval.unbounded.rightOpen(0),
      Interval.unbounded.leftOpen(0),
      Interval.unbounded.leftClosed(5),
      Interval.unbounded.rightClosed(5),
      Interval.unbounded.rightOpen(5),
      Interval.unbounded.leftOpen(5)
    )

    val expected = List(
      Interval.unbounded.rightOpen(0),
      Interval.unbounded.rightClosed(0),
      Interval.unbounded.rightOpen(5),
      Interval.unbounded.rightClosed(5),
      Interval.bounded.create.leftClosedRightOpen(0, 5),
      Interval.bounded.create.closed(0, 5),
      Interval.unbounded.leftClosed(0),
      Interval.bounded.create.open(0, 5),
      Interval.bounded.create.leftOpenRightClosed(0, 5),
      Interval.unbounded.leftOpen(0),
      Interval.unbounded.leftClosed(5),
      Interval.unbounded.leftOpen(5)
    )

    val incOrd = MidpointHelper.IntMidpointHelper.increaseIntervalOrdering
    assert(arr.sorted(incOrd) == expected)
  }

  it should "sortByEnds" in {
    val arr = List(
      Interval.bounded.create.open(0, 5),
      Interval.bounded.create.leftOpenRightClosed(0, 5),
      Interval.bounded.create.leftClosedRightOpen(0, 5),
      Interval.bounded.create.closed(0, 5),
      Interval.unbounded.leftClosed(0),
      Interval.unbounded.rightClosed(0),
      Interval.unbounded.rightOpen(0),
      Interval.unbounded.leftOpen(0),
      Interval.unbounded.leftClosed(5),
      Interval.unbounded.rightClosed(5),
      Interval.unbounded.rightOpen(5),
      Interval.unbounded.leftOpen(5)
    )

    val expected = List (
      Interval.unbounded.leftOpen(5),
      Interval.unbounded.leftClosed(5),
      Interval.unbounded.leftOpen(0),
      Interval.unbounded.leftClosed(0),
      Interval.bounded.create.leftOpenRightClosed(0, 5),
      Interval.bounded.create.closed(0, 5),
      Interval.unbounded.rightClosed(5),
      Interval.bounded.create.open(0, 5),
      Interval.bounded.create.leftClosedRightOpen(0, 5),
      Interval.unbounded.rightOpen(5),
      Interval.unbounded.rightClosed(0),
      Interval.unbounded.rightOpen(0)
    )

    val ord = MidpointHelper.IntMidpointHelper.decreaseIntervalOrdering
    assert(arr.sorted(ord) == expected)
  }


  it should "equalsOtherClasses" in {
    val a = Interval.bounded.create.closed(5, 10);
    val a2 = Interval.bounded.create.closed(5, 10);
    val a3 = Interval.bounded.create.closed(5, 11);
    assertNotEquals(a, new Integer(5));
    assertEquals(a, a2);
    assertNotEquals(a, a3);
    // assertNotEquals(a, new Date());
  }

  it should "isLeftOfBoundedInterval" in {
    var a = Interval.bounded.create.open(5, 10);
    assert(a.isLeftOf(Interval.bounded.create.closed(10, 11)));
    assert(! a.isLeftOf(Interval.bounded.create.open(10, 11))); // ACS
    assert(!a.isLeftOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isLeftOf(Interval.bounded.create.leftOpenRightClosed(6, 12)));
    assert(!a.isLeftOf(Interval.bounded.create.leftClosedRightOpen(1, 7)));
    assert(!a.isLeftOf(Interval.bounded.create.closed(1, 4)));

    a = Interval.bounded.create.closed(5, 10);
    assert(!a.isLeftOf(Interval.bounded.create.closed(10, 11)));
    assert(!a.isLeftOf(Interval.bounded.create.open(10, 11))); // ACS
    assert(!a.isLeftOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isLeftOf(Interval.bounded.create.leftOpenRightClosed(6, 12)));
    assert(!a.isLeftOf(Interval.bounded.create.leftClosedRightOpen(1, 7)));
    assert(!a.isLeftOf(Interval.bounded.create.closed(1, 4)));
  }

  //
  it should "isLeftOfUnboundedInterval" in {
    var a = Interval.unbounded.leftClosed(5);
    assert(!a.isLeftOf(Interval.bounded.create.closed(0, 5)));
    assert(!a.isLeftOf(Interval.bounded.create.open(0, 5)));
    assert(!a.isLeftOf(Interval.bounded.create.open(5, 11)));
    assert(!a.isLeftOf(Interval.bounded.create.closed(5, 11)));
    assert(!a.isLeftOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isLeftOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));
    assert(!a.isLeftOf(Interval.bounded.create.leftClosedRightOpen(1, 7)));

    a = Interval.unbounded.leftOpen(5);
    assert(!a.isLeftOf(Interval.bounded.create.closed(0, 5)));
    assert(!a.isLeftOf(Interval.bounded.create.open(0, 5)));
    assert(!a.isLeftOf(Interval.bounded.create.open(5, 11)));
    assert(!a.isLeftOf(Interval.bounded.create.closed(5, 11)));
    assert(!a.isLeftOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isLeftOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));
    assert(!a.isLeftOf(Interval.bounded.create.leftClosedRightOpen(1, 7)));

    a = Interval.unbounded.rightClosed(5);
    assert(!a.isLeftOf(Interval.bounded.create.closed(0, 5)));
    assert(!a.isLeftOf(Interval.bounded.create.open(0, 5)));
    assert(a.isLeftOf(Interval.bounded.create.open(5, 11)));
    assert(!a.isLeftOf(Interval.bounded.create.closed(5, 11)));
    assert(a.isLeftOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isLeftOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));

    a = Interval.unbounded.rightOpen(5);
    assert(!a.isLeftOf(Interval.bounded.create.closed(0, 5)));
    assert(!a.isLeftOf(Interval.bounded.create.open(0, 5)));
    assert(a.isLeftOf(Interval.bounded.create.open(5, 11)));
    assert(a.isLeftOf(Interval.bounded.create.closed(5, 11)));
    assert(a.isLeftOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isLeftOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));
  }


  it should "isRightOfInterval" in {
    var a = Interval.bounded.create.open(5, 10);
    assert(a.isRightOf(Interval.bounded.create.closed(4, 5)));
    assert(!a.isRightOf(Interval.bounded.create.open(4, 5)));
    assert(!a.isRightOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isRightOf(Interval.bounded.create.leftOpenRightClosed(6, 12)));
    assert(!a.isRightOf(Interval.bounded.create.leftClosedRightOpen(1, 7)));
    assert(!a.isRightOf(Interval.bounded.create.closed(11, 14)));

    a = Interval.bounded.create.closed(5, 10);
    assert(!a.isRightOf(Interval.bounded.create.closed(4, 5)));
    assert(!a.isRightOf(Interval.bounded.create.open(4, 5)));
    assert(!a.isRightOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isRightOf(Interval.bounded.create.leftOpenRightClosed(6, 12)));
    assert(!a.isRightOf(Interval.bounded.create.leftClosedRightOpen(1, 7)));
    assert(!a.isRightOf(Interval.bounded.create.closed(11, 14)));
  }


  it should "isRightOfUnboundedInterval" in {
    var a = Interval.unbounded.leftClosed(5);
    assert(!a.isRightOf(Interval.bounded.create.closed(0, 5)));
    assert(a.isRightOf(Interval.bounded.create.open(0, 5)));
    assert(!a.isRightOf(Interval.unbounded.leftClosed(5)));
    assert(!a.isRightOf(Interval.unbounded.rightClosed(5)));
    assert(a.isRightOf(Interval.unbounded.rightOpen(5)));
    assert(!a.isRightOf(Interval.unbounded.leftOpen(5)));
    assert(!a.isRightOf(Interval.bounded.create.open(5, 11)));
    assert(!a.isRightOf(Interval.bounded.create.closed(5, 11)));
    assert(!a.isRightOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isRightOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));
    assert(a.isRightOf(Interval.bounded.create.leftClosedRightOpen(1, 3)));

    a = Interval.unbounded.leftOpen(5);
    assert(a.isRightOf(Interval.bounded.create.closed(0, 5)));
    assert(a.isRightOf(Interval.bounded.create.open(0, 5)));
    assert(!a.isRightOf(Interval.unbounded.leftClosed(5)));
    assert(a.isRightOf(Interval.unbounded.rightClosed(5)));
    assert(a.isRightOf(Interval.unbounded.rightOpen(5)));
    assert(!a.isRightOf(Interval.unbounded.leftOpen(5)));
    assert(!a.isRightOf(Interval.bounded.create.open(5, 11)));
    assert(!a.isRightOf(Interval.bounded.create.closed(5, 11)));
    assert(!a.isRightOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isRightOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));
    assert(a.isRightOf(Interval.bounded.create.leftClosedRightOpen(1, 3)));

    a = Interval.unbounded.rightClosed(5);
    assert(!a.isRightOf(Interval.bounded.create.closed(5, 8)));
    assert(!a.isRightOf(Interval.bounded.create.open(5, 8)));
    assert(!a.isRightOf(Interval.unbounded.leftClosed(5)));
    assert(!a.isRightOf(Interval.unbounded.rightClosed(5)));
    assert(!a.isRightOf(Interval.unbounded.rightOpen(5)));
    assert(!a.isRightOf(Interval.unbounded.leftOpen(5)));
    assert(!a.isRightOf(Interval.bounded.create.open(0, 5)));
    assert(!a.isRightOf(Interval.bounded.create.closed(0, 5)));
    assert(!a.isRightOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isRightOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));
    assert(!a.isRightOf(Interval.bounded.create.leftClosedRightOpen(1, 3)));

    a = Interval.unbounded.rightOpen(5);
    assert(!a.isRightOf(Interval.bounded.create.closed(5, 8)));
    assert(!a.isRightOf(Interval.bounded.create.open(5, 8)));
    assert(!a.isRightOf(Interval.unbounded.leftClosed(5)));
    assert(!a.isRightOf(Interval.unbounded.rightClosed(5)));
    assert(!a.isRightOf(Interval.unbounded.rightOpen(5)));
    assert(!a.isRightOf(Interval.unbounded.leftOpen(5)));
    assert(!a.isRightOf(Interval.bounded.create.open(0, 5)));
    assert(!a.isRightOf(Interval.bounded.create.closed(0, 5)));
    assert(!a.isRightOf(Interval.bounded.create.closed(6, 8)));
    assert(!a.isRightOf(Interval.bounded.create.leftOpenRightClosed(4, 12)));
    assert(!a.isRightOf(Interval.bounded.create.leftClosedRightOpen(1, 3)));
  }


  it should "isRightOfDegenerate" in {
    var a = Interval.unbounded.leftClosed(5);
    assert(!a.isRightOf(Interval.unbounded[Int]()));
    assert(!a.isRightOf(Interval.bounded.create.open(20, 10)));

    a = Interval.unbounded.rightClosed(5);
    assert(!a.isRightOf(Interval.unbounded[Int]()));
    assert(!a.isRightOf(Interval.bounded.create.open(20, 10)));

    a = Interval.unbounded.leftOpen(5);
    assert(!a.isRightOf(Interval.unbounded[Int]()));
    assert(!a.isRightOf(Interval.bounded.create.open(20, 10)));

    a = Interval.unbounded.rightOpen(5);
    assert(!a.isRightOf(Interval.unbounded[Int]()));
    assert(!a.isRightOf(Interval.bounded.create.open(20, 10)));
  }


  it should "isLeftOfDegenerate" in {
    var a = Interval.unbounded.leftClosed(5);
    assert(!a.isLeftOf(Interval.unbounded[Int]()));
    assert(!a.isLeftOf(Interval.bounded.create.open(20, 10)));

    a = Interval.unbounded.rightClosed(5);
    assert(!a.isLeftOf(Interval.unbounded[Int]()));
    assert(!a.isLeftOf(Interval.bounded.create.open(20, 10)));

    a = Interval.unbounded.leftOpen(5);
    assert(!a.isLeftOf(Interval.unbounded[Int]()));
    assert(!a.isLeftOf(Interval.bounded.create.open(20, 10)));

    a = Interval.unbounded.rightOpen(5);
    assert(!a.isLeftOf(Interval.unbounded[Int]()));
    assert(!a.isLeftOf(Interval.bounded.create.open(20, 10)));
  }


  it should "isLeftOfPoint" in {
    var a = Interval.bounded.create.open(5, 10);
    assert(a.isLeftOfPoint(10));
    assert(a.isLeftOfPoint(11));
    assert(!a.isLeftOfPoint(9));
    assert(!a.isLeftOfPoint(5));
    assert(!a.isLeftOfPoint(3));

    a = Interval.bounded.create.closed(5, 10);
    assert(!a.isLeftOfPoint(10));
    assert(a.isLeftOfPoint(11));
    assert(!a.isLeftOfPoint(9));
    assert(!a.isLeftOfPoint(5));
    assert(!a.isLeftOfPoint(3));
  }


  it should "isRightOfPoint" in {
    var a = Interval.bounded.create.open(5, 10);
    assert(a.isRightOfPoint(5));
    assert(a.isRightOfPoint(4));
    assert(!a.isRightOfPoint(6));
    assert(!a.isRightOfPoint(10));
    assert(!a.isRightOfPoint(12));

    a = Interval.bounded.create.closed(5, 10);
    assert(!a.isRightOfPoint(5));
    assert(a.isRightOfPoint(4));
    assert(!a.isRightOfPoint(6));
    assert(!a.isRightOfPoint(10));
    assert(!a.isRightOfPoint(12));
  }

}
