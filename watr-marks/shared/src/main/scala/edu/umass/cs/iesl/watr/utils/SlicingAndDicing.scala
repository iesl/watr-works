package edu.umass.cs.iesl.watr
package utils

// import scala.annotation.tailrec
import scala.collection.mutable


object SlicingAndDicing {

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
    groups
  }

  implicit class RicherSeq[A](val thisSeq: Seq[A]) extends AnyVal {

    import scala.collection.mutable

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

        groupByStartIndexes(groupSpans.map(_._2).reverse, thisSeq)
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
