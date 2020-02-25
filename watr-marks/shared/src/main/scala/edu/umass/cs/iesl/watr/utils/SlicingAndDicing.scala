package edu.umass.cs.iesl.watr
package utils

import scala.collection.mutable
// import scala.{ collection => sc }
// import scala.collection.{ immutable => sci }
// import scala.collection.{ mutable => scm }

object SlicingAndDicing { outer =>

  private def splitAtBreaks[A](bis: Seq[Int], cs: Seq[A]): Seq[Seq[A]] = {
    if (bis.isEmpty) Seq(cs) else {
      val (pre, post) = cs.splitAt(bis.head+1)
      pre +: splitAtBreaks(bis.tail.map(_-bis.head-1), post)
    }
  }

  private def groupByStartIndexes[A](lengths: Seq[Int], cs: Seq[A]): Seq[Seq[A]] = {
    val groups = mutable.ListBuffer[Seq[A]]()
    var _cs = cs

    for {
     len <- lengths
    } {
      val (group, post) = _cs.splitAt(len)
      groups.append(group)
      _cs = post
    }

    groups.toSeq
  }

  def uniqueCountBy[A, O: scala.Ordering](ss: Seq[A], f: (A) => O): Seq[(Int, A)] = {
    ss.sortBy(f)
      .groupByPairs((a, b) => f(a) == f(b))
      .map(g => (g.length, g.head))
  }

  def uniqueBy[A, O: scala.Ordering](ss: Seq[A], f: (A) => O): Seq[A] = {
    uniqueCountBy(ss, f).map(_._2)
  }

  implicit class RicherSeq[A](val thisSeq: Seq[A]) extends AnyVal {
    import scala.collection.mutable


    def groupByWindow(f: (Seq[A], A) => Boolean): Seq[Seq[A]] = {
      Cursors.groupByWindow(f, thisSeq)
    }

    def trimLeftRightBy(f: (A) => Boolean): Seq[A] = {
      thisSeq
        .dropWhile(f(_)).reverse
        .dropWhile(f(_)).reverse
    }

    def groupByPairsWithIndex(f: (A, A, Int) => Boolean): Seq[Seq[A]] = {
      if (thisSeq.isEmpty) Seq() else {

        val groupSpans = mutable.Stack[(Int, Int)]((0, 1))

        thisSeq.sliding(2).toSeq
          .zipWithIndex
          .foreach({
            case (Seq(a1, a2), i) =>
              if (f(a1, a2, i)) {
                val (start, len) = groupSpans.pop()
                groupSpans.push((start, len+1))
              } else {
                groupSpans.push((i+1, 1))
              }

            case (Seq(a), i) => // noop
            case (Seq(), i) => // noop
          })

        groupByStartIndexes(groupSpans.toSeq.map(_._2).reverse, thisSeq)
      }
    }

    def splitOnPairsWithIndex(f: (A, A, Int) => Boolean): Seq[Seq[A]] = {
      val splits: Seq[Int] = thisSeq
        .sliding(2).toSeq
        .zipWithIndex
        .map({
          case (Seq(a1, a2), i) => if (f(a1, a2, i)) Some(i) else None
          case (Seq(a), i)      => None
          case (Seq(), i)       => None
        })
        .flatten

      splitAtBreaks(splits, thisSeq)
    }

    def groupByPairs(f: (A, A) => Boolean): Seq[Seq[A]] =
      groupByPairsWithIndex((a, b, _) => f(a, b))


    def splitOnPairs(f: (A, A) => Boolean): Seq[Seq[A]] =
      splitOnPairsWithIndex((a, b, _) => f(a, b))


    def foreachPair(f: (A, A) => Unit): Unit = {
      splitOnPairs((a, b) => {f(a, b); true})
    }

    def clusterBy(f: (A, A)=>Boolean): Seq[Seq[A]] = {
      clusterSeqBy(thisSeq)(f)
    }

    /**
      */
    def uniqueCountBy[O: scala.Ordering](f: (A) => O): Seq[(Int, A)] = {
      outer.uniqueCountBy(thisSeq, f)
    }

    def uniqueBy[O: scala.Ordering](f: (A) => O): Seq[A] = {
      outer.uniqueBy(thisSeq, f)
    }

  }


  def groupSeqBy[A](as: Seq[A])(f: (A, A)=>Boolean): Seq[Seq[A]] = {

    def loop(ns: Seq[A]): Seq[Seq[A]] = {
      ns.headOption.map ({ headA =>

        val matches = ns.tail.filter(n => f(headA, n))

        (headA +: matches) +: loop(ns.tail diff matches)

      }).getOrElse({
        Seq.empty[Seq[A]]
      })

    }

    loop(as)
  }
  def clusterSeqBy[A](as: Seq[A])(f: (A, A)=>Boolean): Seq[Seq[A]] = {

    def loop(ns: Seq[A]): Seq[Seq[A]] = {
      ns.headOption.map ({ headA =>

        val matches = ns.tail.filter(n => f(headA, n))

        (headA +: matches) +: loop(ns.tail diff matches)

      }).getOrElse({
        Seq.empty[Seq[A]]
      })

    }

    loop(as)
  }


}
