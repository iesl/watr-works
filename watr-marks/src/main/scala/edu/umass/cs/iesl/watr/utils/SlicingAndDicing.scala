package edu.umass.cs.iesl.watr
package utils

object SlicingAndDicing {

  private def splitAtBreaks[A](bis: Seq[Int], cs: Seq[A]): Seq[Seq[A]] = {
    if (bis.isEmpty) Seq(cs) else {
      val (pre, post) = cs.splitAt(bis.head+1)
      pre +: splitAtBreaks(bis.tail.map(_-bis.head-1), post)
    }
  }

  implicit class RicherSeq[A](val aas: Seq[A]) extends AnyVal {


    def splitOnPairsWithIndex(f: (A, A, Int) => Boolean): Seq[Seq[A]] = {
      val splits: Seq[Int] = aas
        .sliding(2).toSeq
        .zipWithIndex
        .map({
          case (Seq(a1, a2), i) => if (f(a1, a2, i)) Some(i) else None
          case (Seq(a), i)      => None
          case (Seq(), i)       => None
        })
        .flatten

      splitAtBreaks(splits, aas)
    }


    def splitOnPairs(f: (A, A) => Boolean): Seq[Seq[A]] =
      splitOnPairsWithIndex((a, b, _) => f(a, b))


    def clusterBy(f: (A, A)=>Boolean): Seq[Seq[A]] = {
      clusterSeqBy(aas)(f)
    }

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
