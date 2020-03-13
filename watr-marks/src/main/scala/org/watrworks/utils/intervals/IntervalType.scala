package edu.umass.cs.iesl.watr
package utils.intervals

import utils.DoOrDieHandlers._

import scalaz.{@@ => _, Ordering => _, _} // , Scalaz._

object Interval {

  /**
    * A comparator that can be used as a parameter for sorting functions. The start comparator sorts the intervals
    * in <em>ascending</em> order by placing the intervals with a smaller start point before intervals with greater
    * start points. This corresponds to a line sweep from left to right.
    *
    * Intervals with start point null (negative infinity) are considered smaller than all other intervals.
    * If two intervals have the same start point, the closed start point is considered smaller than the open one.
    * For example, [0, 2) is considered smaller than (0, 2).
    *
    * To ensure that this comparator can also be used in sets it considers the end points of the intervals, if the
    * start points are the same. Otherwise the set will not be able to handle two different intervals, sharing
    * the same starting point, and omit one of the intervals.
    */

  class IncreaseOrdering[T] extends Ordering[Interval[T, Any]] {
    def compare(a:Interval[T, Any], b:Interval[T, Any]): Int = {
      var compare = a.compareStarts(b);
      if (compare != 0)
        compare;
      else {
        compare = a.compareEnds(b);
        if (compare != 0)
          compare;
        else a.compareSpecialization(b);
      }
    }
  }

  implicit def defaultIntervalOrdering[T, W](): Ordering[Interval[T, W]] = {
    // new Interval.IncreaseOrdering[Int]()
    new Ordering[Interval[T, W]] {
      def compare(a:Interval[T, W], b:Interval[T, W]): Int = {
        var compare = a.compareStarts(b);
        if (compare != 0)
          compare;
        else {
          compare = a.compareEnds(b);
          if (compare != 0)
            compare;
          else a.compareSpecialization(b);
        }
      }
    }
  }

  def increaseIntervalOrderingDouble: IncreaseOrdering[Double] = {
    new Interval.IncreaseOrdering[Double]()
  }

  /**
    * A comparator that can be used as a parameter for sorting functions. The end comparator sorts the intervals
    * in <em>descending</em> order by placing the intervals with a greater end point before intervals with smaller
    * end points. This corresponds to a line sweep from right to left.
    *
    * Intervals with end point null (positive infinity) are placed before all other intervals. If two intervals
    * have the same end point, the closed end point is placed before the open one. For example,  [0, 10) is placed
    * after (0, 10].
    *
    * To ensure that this comparator can also be used in sets it considers the start points of the intervals, if the
    * end points are the same. Otherwise the set will not be able to handle two different intervals, sharing
    * the same end point, and omit one of the intervals.
    *
    * Since this is a static method of a generic class, it involves unchecked calls to class methods. It is left to
    * ths user to ensure that she compares intervals from the same class, otherwise an exception might be thrown.
    *
    */
  class DecreaseOrdering[T] extends Ordering[Interval[T, _]] {
    def compare(a:Interval[T, _], b:Interval[T, _]): Int = {

      var compare = b.compareEnds(a);
      if (compare != 0)
        compare;
      else {

        compare = b.compareStarts(a);
        if (compare != 0)
          compare;
        else b.compareSpecialization(a);
      }
    }
  }

  def ord[T: Ordering](t1: Option[T], t2: Option[T]): Int = {
    val o1 = t1.orDie("can't order None")
    val o2 = t2.orDie("can't order None")
    Ordering[T].compare(o1, o2)
  }

  def ord[T: Ordering](t1: T, t2: Option[T]): Int = {
    val o1 = t1
    val o2 = t2.orDie("can't order None")
    Ordering[T].compare(o1, o2)
  }

  def ord[T: Ordering](t1: Option[T], t2: T): Int = {
    val o1 = t1.orDie("can't order None")
    val o2 = t2
    Ordering[T].compare(o1, o2)
  }
  def ord[T: Ordering](t1: T, t2: T): Int = {
    Ordering[T].compare(t1, t2)
  }


  def nonEmpty[T](interval: Interval[T, _]): Boolean = {
    interval != null && interval.nonEmpty()
  }


  // implicit object StringShow extends Show[String] {
  //   override def show(f: String): Cord = new Cord(FingerTree.three("\"", f, "\"")(Cord.sizer).toTree)
  //   override def shows(f: String): String = f
  // }

  implicit object UnitShow extends Show[Unit] {
    // override def show(f: Unit): Cord = new Cord(FingerTree.three("\"", f, "\"")(Cord.sizer).toTree)
    override def shows(f: Unit): String = ""
  }

  object bounded {
    object create {

      def leftClosedRightOpen[T: Numeric: MidpointHelper](lower: T, upper: T): Interval[T, Unit] = {
        new Interval[T, Unit](Some(lower), Some(upper),
          isStartInclusive=true,
          isEndInclusive=false,
          ()
        )
      }

      def leftOpenRightClosed[T: Numeric: MidpointHelper](lower: T, upper: T): Interval[T, Unit] = {
        new Interval[T, Unit](Some(lower), Some(upper),
          isStartInclusive=false,
          isEndInclusive=true,
          ()
        )
      }

      def closed[T: Numeric: MidpointHelper](lower: T, upper: T): Interval[T, Unit] = {
        new Interval[T, Unit](Some(lower), Some(upper),
          isStartInclusive=true,
          isEndInclusive=true,
          ()
        )
      }

      def open[T: Numeric: MidpointHelper](lower: T, upper: T): Interval[T, Unit] = {
        new Interval[T, Unit](Some(lower), Some(upper),
          isStartInclusive=false,
          isEndInclusive=false,
          ()
        )
      }
    }



    def leftClosedRightOpen[T: Numeric: MidpointHelper](lower: T, upper: T): Option[Interval[T, Unit]] = {
      if (Ordering[T].lt(lower, upper)) {
        Some(create.leftClosedRightOpen(lower, upper))
      } else None
    }

    def leftOpenRightClosed[T: Numeric: MidpointHelper](lower: T, upper: T): Option[Interval[T, Unit]] = {
      if (Ordering[T].lt(lower, upper)) {
        Some(create.leftOpenRightClosed(lower, upper))
      } else None
    }

    def closed[T: Numeric: MidpointHelper](lower: T, upper: T): Option[Interval[T, Unit]] = {
      if (Ordering[T].lteq(lower, upper)) {
        Some(create.closed(lower, upper))
      } else None
    }

    def open[T: Numeric: MidpointHelper](lower: T, upper: T): Option[Interval[T, Unit]] = {
      if (Ordering[T].lt(lower, upper)) {
        Some(create.open(lower, upper))
      } else None
    }

  }

  object unbounded {
    def leftOpen[T: Numeric: MidpointHelper](value: T): Interval[T, Unit] = {
      new Interval[T, Unit](
        Some(value), None,
        isStartInclusive=false,
        isEndInclusive=true,
        ()
      )
    }

    def leftClosed[T: Numeric: MidpointHelper](value: T): Interval[T, Unit] = {
      new Interval[T, Unit](
        Some(value), None,
        isStartInclusive=true,
        isEndInclusive=true,
        ()
      )
    }

    def rightOpen[T: Numeric: MidpointHelper](value: T): Interval[T, Unit] = {
      new Interval[T, Unit](
        None, Some(value),
        isStartInclusive=true,
        isEndInclusive=false,
        ()
      )
    }

    def rightClosed[T: Numeric: MidpointHelper](value: T): Interval[T, Unit] = {
      new Interval[T, Unit](
        None, Some(value),
        isStartInclusive=true,
        isEndInclusive=true,
        ()
      )
    }

    def apply[T: Numeric: MidpointHelper](): Interval[T, Unit] = {
      new Interval[T, Unit](
        None, None,
        isStartInclusive=true,
        isEndInclusive=true,
        ()
      )
    }


  }
}

trait MidpointHelper[T] {
  def getMidpoint(t: Interval[T, _]): Option[T]
	def isEmpty(t: Interval[T, _]): Boolean
  def minValue(): T
  def maxValue(): T

  def increaseIntervalOrdering(): Interval.IncreaseOrdering[T]
  def decreaseIntervalOrdering(): Interval.DecreaseOrdering[T]
}

object MidpointHelper {

	def _startEndDefined[T: Numeric](t: Interval[T, _]): Boolean = {
    t.start.nonEmpty && t.end.nonEmpty
  }

	def _isEmpty[T: Numeric](t: Interval[T, _]): Boolean = {
    val compare = Interval.ord(t.start, t.end)

    compare > 0 || (
      compare == 0 && (!t.isEndInclusive || !t.isStartInclusive)
    )
  }

	def start[T](t: Interval[T, _]): T = t.start.orDie("undefined t.start")
	def end[T](t: Interval[T, _]): T = t.end.orDie("undefined t.end")


  implicit object IntMidpointHelper extends MidpointHelper[Int] {
    def minValue(): Int = Int.MinValue
    def maxValue(): Int = Int.MaxValue

    def getMidpoint(t: Interval[Int, _]): Option[Int] = {
      // println(s"getMidpoint(${t}): isEmpty=${isEmpty(t)}")
      if (isEmpty(t)) None else {
        val from = t.start.map(_.toLong).getOrElse(Int.MinValue.toLong)
        val to = t.end.map(_.toLong).getOrElse(Int.MaxValue.toLong)
        val range = to - from
        Some(
          ((from + to) / 2).toInt
        )
      }
    }

    def isEmpty(t: Interval[Int, _]): Boolean = {
      _startEndDefined(t) && {
        lazy val isOneGreater = start(t) + 1 == end(t)
        lazy val emptyInt = isOneGreater && !t.isEndInclusive && !t.isStartInclusive

        emptyInt || _isEmpty(t)
      }
    }

    override val increaseIntervalOrdering: Interval.IncreaseOrdering[Int] = {
      new Interval.IncreaseOrdering[Int]
    }

    override val decreaseIntervalOrdering: Interval.DecreaseOrdering[Int] = {
      new Interval.DecreaseOrdering[Int]
    }
  }

  implicit object DoubleMidpointHelper extends MidpointHelper[Double] {
    val OFFSET = 1000

    def minValue(): Double = Double.MinValue
    def maxValue(): Double = Double.MaxValue


    def getMidpoint(t: Interval[Double, _]): Option[Double] = {
      if (isEmpty(t)) None else {
        // Handle empty values
        val mid = if (!_startEndDefined(t))
          0.0;
        else if (t.start.isEmpty)
          end(t) - OFFSET;
        else if (t.end.isEmpty)
          start(t) + OFFSET;

        // Now we are sure there are no more empty values involved
        else if (start(t) == Double.NegativeInfinity && end(t) == Double.PositiveInfinity)
          0.0;
        else if (start(t) == Double.NegativeInfinity)
          end(t) - OFFSET;
        else if (end(t) == Double.PositiveInfinity)
          start(t) + OFFSET;
        else
          start(t) + (end(t) - start(t))/2;

        Some(mid)
      }
    }
    def isEmpty(t: Interval[Double, _]): Boolean = {
      _startEndDefined(t) && _isEmpty(t)
    }
    override val increaseIntervalOrdering: Interval.IncreaseOrdering[Double] = {
      new Interval.IncreaseOrdering[Double]
    }

    override val decreaseIntervalOrdering: Interval.DecreaseOrdering[Double] = {
      new Interval.DecreaseOrdering[Double]
    }
  }

}


class Interval[T: Numeric: MidpointHelper, +W: Show](
  val start: Option[T],
  val end: Option[T],
  val isStartInclusive: Boolean,
  val isEndInclusive: Boolean,
  val attr: W
) {

  import Interval._


  val mphelp = implicitly[MidpointHelper[T]]

  def withAttr[W2: Show](w2: W2): Interval[T, W2] = {
    new Interval[T, W2](start, end, isStartInclusive, isEndInclusive, w2)
  }

  override def toString(): String = {
    val startstr = start.map(_.toString()).getOrElse("∞")
    val endstr = end.map(_.toString()).getOrElse("∞")
    val lp = if (isStartInclusive) "[" else "("
    val rp = if (isEndInclusive) "]" else ")"
    val attrStr = implicitly[Show[W]].shows(attr)
    val att = if (attrStr.isEmpty()) "" else s" #${attrStr}#"

    s"${lp}${startstr}, ${endstr}<${att}>${rp}"

  }

	/**
	  * Checks if the current interval contains no points.
	  *
	  * <p>In particular, if the end point is less than the start point, then the interval is
	  * considered to be empty. There are, however other instances, in which an interval is empty.
	  * For example, in the class {@link IntegerInterval}, an open interval, whose start and end
	  * points differ by one, for example the interval (4, 5), is empty, because it contains no integers
	  * in it. The same interval, however, will <strong>not</strong> be considered empty in the
	  * {@link DoubleInterval} class, because there are Double numbers within this interval.
	  * </p>
	  *
	  * @return {@code true}, if the current interval is empty or {@code false} otherwise.
	  */
	def isEmpty(): Boolean = {
    mphelp.isEmpty(this)
	}

	def nonEmpty(): Boolean = !isEmpty()

	/**
	  * Determines if the current interval is a single point.
	  *
	  * @return {@code true}, if the current interval represents a single point.
	  */
  def isPoint(): Boolean = {
    (MidpointHelper._startEndDefined(this)
      && Interval.ord(start, end) == 0
      && isStartInclusive
      && isEndInclusive
    )
  }

  def getMidpoint(): Option[T] = {
    mphelp.getMidpoint(this)
  }

	/**
	 * Returns an interval, representing the intersection of two intervals. More formally, for every
	 * point {@code x} in the returned interval, {@code x} will belong in both the current interval
	 * and the {@code other} interval.
	 *
	 * @param other The other interval
	 * @return The intersection of the current interval wih the {@code other} interval.
    */
  def getIntersection(other: Interval[T, Any]): Interval[T, Unit] = {

    if (other == null || isEmpty() || other.isEmpty()) {
      null
    } else {

      val cmpThisOther = mphelp.increaseIntervalOrdering.compare(this, other)

      // Make sure that the one with the smaller starting point gets intersected with the other.
      // If necessary, swap the intervals
      // if ((other.start.isEmpty && start.nonEmpty) || (start.nonEmpty && Interval.ord(start, other.start) > 0)) {
      if (cmpThisOther > 0) {
        other.getIntersection(this);
      } else if (end.nonEmpty
        && other.start.nonEmpty
        && (Interval.ord(end, other.start) < 0 || (Interval.ord(end, other.start) == 0 && (!isEndInclusive || !other.isStartInclusive)))
      ) {
        null
      } else {


        var newStart: Option[T] = None
        var newEnd: Option[T] = None

        var isNewStartInclusive: Boolean = false
        var isNewEndInclusive: Boolean = false

        // If other.start is null, this means my start is also null, because we made sure
        // that the caller object hast the smaller start point => the new start is null
        if (other.start.isEmpty){
          newStart = None
          isNewStartInclusive = true
        } else {
          newStart = other.start;
          if (start.nonEmpty && Interval.ord(other.start, start) == 0)
            isNewStartInclusive = other.isStartInclusive && isStartInclusive;
          else
            isNewStartInclusive = other.isStartInclusive;
        }

        if (end.isEmpty){
          newEnd = other.end;
          isNewEndInclusive = other.isEndInclusive;
        } else if (other.end.isEmpty){
          newEnd = end;
          isNewEndInclusive = isEndInclusive;
        } else {
          if (Interval.ord(end, other.end) == 0){
            newEnd = end;
            isNewEndInclusive = isEndInclusive && other.isEndInclusive;
          } else if (Interval.ord(end, other.end) < 0){
            newEnd = end;
            isNewEndInclusive = isEndInclusive;
          } else {
            newEnd = other.end;
            isNewEndInclusive = other.isEndInclusive;
          }
        }
        val intersection = new Interval[T, Unit](
          newStart, newEnd,
          isNewStartInclusive, isNewEndInclusive, ()
        )
        if (intersection.isEmpty()) null else intersection
      }
    }
  }


  /**
    * Determines if the current interval contains a query point.
    *
    * @param query The point.
    * @return {@code true}, if the current interval contains the {@code query} point or false otherwise.
    */
  def contains(query: T): Boolean = {
    contains(Some(query))
  }

  /**
    * Determines if the current interval contains a query point.
    *
    * @param query The point.
    * @return {@code true}, if the current interval contains the {@code query} point or false otherwise.
    */
  def contains(query: Option[T]): Boolean = {
    (query.isDefined && nonEmpty() && {
      val startCompare = if (start.isEmpty)  1 else Interval.ord(query, start)
      val endCompare = if (end.isEmpty)  -1 else Interval.ord(query, end)

      ((startCompare > 0 && endCompare < 0)
        || (startCompare == 0 && isStartInclusive)
        || (endCompare == 0 && isEndInclusive))
    })
  }

  /**
    * Checks if the current interval contains the entirety of another interval. More formally,
    * this method returns {@code true}, if for every point {@code x} from the interval {@code another}
    * this point {@code x} also belongs to the current interval.
    *
    * @param another Another interval.
    * @return {@code true}, if the interval {@code another} is contained in the current interval in
    * its entirety, or {@code false} otherwise.
    */
  def contains(another: Interval[T, _]): Boolean = {
    (another != null && nonEmpty() && another.nonEmpty() && {
      val intersection = getIntersection(another);
		  intersection != null && intersection.equals(another);
    })
	}


	/**
	  * Checks if the current interval intersects another interval. More formally, this method
	  * returns {@code true} if there is at least one point the current interval, that also
	  * belongs to the {@code query} interval.
	  *
	  * @param query The interval being checked for intersection with the current interval.
	  * @return {@code true}, if the two intervals intersect or {@code false} otherwise.
	  */
	def intersects(query: Interval[T, _]): Boolean = {
    query != null && getIntersection(query) != null
  }


	/**
	 * This method checks, if this current interval is entirely to the right of a point. More formally,
	 * the method will return {@code true}, if for every point {@code x} from the current interval the inequality
	 * {@code x} &gt; {@code point} holds. If the parameter {@code inclusive} is set to {@code false}, this
	 * method will return {@code true} also if the start point of the interval is equal to the reference
	 * {@code point}.
	 *
	 * @param point      The reference point
	 * @param inclusive  {@code false} if the reference {@code point} is allowed to be the start point
	 *                   of the current interval.
	 * @return {@code true}, if the current interval is entirely to the right of the {@code other}
	 * interval, or {@code false} instead.
	 */
	def isRightOfPoint(point: T, inclusive: Boolean=true): Boolean = {
    start.map{ st =>
      val compare = Interval.ord(point, st)

		  if (compare != 0) compare < 0;
		  else !isStartInclusive || !inclusive;
    } getOrElse(false)
	}


	def isRightOfPoint(point: Option[T], inclusive: Boolean): Boolean = {
    point.map(isRightOfPoint(_, inclusive))
      .getOrElse(false)
	}

	/**
	 * This method checks, if this current interval is entirely to the right of another interval
	 * with no common points. More formally, the method will return true, if for every point {@code x}
	 * from the current interval and for every point {@code y} from the {@code other} interval the
	 * inequality {@code x} &gt; {@code y} holds. This formal definition implies in particular that if the start point
	 * of the current interval is equal to the end point of the {@code other} interval, the method
	 * will return {@code false} only if both points are inclusive and {@code true} in all other cases.
	 *
	 * @param other The reference interval
	 * @return {@code true}, if the current interval is entirely to the right of the {@code other}
	 * interval, or {@code false} instead.
	 */
	def isRightOf(other: Interval[T, _]): Boolean = {
    Interval.nonEmpty(other) && isRightOfPoint(other.end, other.isEndInclusive)
	}

	/**
	 * This method checks, if this current interval is entirely to the left of a point. More formally,
	 * the method will return {@code true}, if for every point {@code x} from the current interval the inequality
	 * {@code x} &lt; {@code point} holds. If the parameter {@code inclusive} is set to {@code false}, this
	 * method will return {@code true} also if the end point of the interval is equal to the reference
	 * {@code point}.
	 *
	 * @param point      The reference point
	 * @param inclusive  {@code false} if the reference {@code point} is allowed to be the end point
	 *                   of the current interval.
	 * @return {@code true}, if the current interval is entirely to the left of the {@code other}
	 * interval, or {@code false} instead.
	 */
	def isLeftOfPoint(point: T, inclusive: Boolean=true): Boolean = {
		end.nonEmpty && {
		  val compare = Interval.ord(point, end)

		  if (compare != 0) compare > 0;
		  else !isEndInclusive || !inclusive;
    }
  }


	def isLeftOfPoint(point: Option[T], inclusive: Boolean): Boolean = {
    point.map(isLeftOfPoint(_, inclusive))
      .getOrElse(false)
	}


	/**
	 * This method checks, if this current interval is entirely to the left of another interval
	 * with no common points. More formally, the method will return true, if for every point {@code x}
	 * from the current interval and for every point {@code y} from the {@code other} interval the
	 * inequality {@code x} &lt; {@code y} holds. This formal definition implies in particular that if the end point
	 * of the current interval is equal to the start point of the {@code other} interval, the method
	 * will return {@code false} only if both points are inclusive and {@code true} in all other cases.
	 *
	 * @param other The reference interval
	 * @return {@code true}, if the current interval is entirely to the left of the {@code other}
	 * interval, or {@code false} instead.
	 */
	def isLeftOf(other: Interval[T, _]): Boolean = {
		Interval.nonEmpty(other) && isLeftOfPoint(other.start, other.isStartInclusive)
	}

  override def equals(obj: Any): Boolean = {
    if (obj == null || !(obj.isInstanceOf[Interval[T, _]])) {
      false;
    } else {

      val other = obj.asInstanceOf[Interval[T, _]];
      if (start.isEmpty ^ other.start.isEmpty)
        false;
      else if (end.isEmpty ^ other.end.isEmpty)
        false;
      else if (isEndInclusive ^ other.isEndInclusive)
        false;
      else if (isStartInclusive ^ other.isStartInclusive)
        false;
      else if (start.nonEmpty && !start.equals(other.start))
        false;
      else if (end.nonEmpty && !end.equals(other.end))
        false;
      else true;
    }
  }

	override def hashCode(): Int = {
		val prime = 31;
		var result = start.##
		result = prime * result + end.##;
		result = prime * result + isStartInclusive.##
		result = prime * result + isEndInclusive.##
		return result;
	}

	/**
	 * A {@link Comparator} that only considers the start points of the intervals. It can not and must
	 * not be used as a standalone {@link Comparator}. It only serves to create a more readable and
	 * modular code.
    */
  def compareStarts(other: Interval[T, _]): Int = {
    if (start.isEmpty && other.start.isEmpty)
      0;
    else if (start.isEmpty)
      -1;
    else if (other.start.isEmpty)
      1;
    else {

      val compare = Interval.ord(start, other.start)
      if (compare != 0)
        compare;
      else if (isStartInclusive ^ other.isStartInclusive)
        if( isStartInclusive ) -1 else 1;
      else  0;
    }
  }

  /**
    * A {@link Comparator} that only considers the end points of the intervals. It can not and must
   * not be used as a standalone {@link Comparator}. It only serves to create a more readable and
	 * modular code.
    */
  def compareEnds(other: Interval[T, _]): Int = {
    if (end.isEmpty && other.end.isEmpty)
       0;
    else if (end.isEmpty)
       1;
    else if (other.end.isEmpty)
       -1;
    else {

      val compare = Interval.ord(end, other.end)
      if (compare != 0)
         compare;
      else if (isEndInclusive ^ other.isEndInclusive)
        if(isEndInclusive)  1 else -1;
      else  0;
    }
	}

	def compareSpecialization(other: Interval[T, _]): Int = {
		0
	}
}
