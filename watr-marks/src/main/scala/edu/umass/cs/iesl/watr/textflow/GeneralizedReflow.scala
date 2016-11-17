package edu.umass.cs.iesl.watr
package textflow

import watrmarks._
import textboxing.{TextBoxing => TB}

import scalaz._, Scalaz.{fix => _, _}

import matryoshka._
import matryoshka.data._

import Recursive.ops._
// import Corecursive.ops._
import FunctorT.ops._
import utils.Ranges


class TextReflowAtomOps(
  val chars: Seq[Char]
) {
  override def toString = chars.mkString
}


sealed trait TextReflowF[+A]

object TextReflowF {
  // case class Atom[T](c: T, ops: TextReflowAtomOps)        extends TextReflowF[Nothing]
  case class Atom(c: Any, ops: TextReflowAtomOps)        extends TextReflowF[Nothing]
  case class Insert(value: String)                        extends TextReflowF[Nothing]
  case class Rewrite[A](from: A, to: String)              extends TextReflowF[A]
  case class Bracket[A](pre: String, post: String, a: A)  extends TextReflowF[A]
  case class Flow[A](labels: Set[Label], as: List[A])     extends TextReflowF[A]
  case class Labeled[A](labels: Set[Label], a: A)         extends TextReflowF[A]

  implicit val ReflowTraverse: Traverse[TextReflowF] = new Traverse[TextReflowF] {
    def traverseImpl[G[_], A, B](fa: TextReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[TextReflowF[B]] = fa match {
      case Atom(c, ops)               => G.point(Atom(c, ops))
      case Insert(value)              => G.point(Insert(value))
      case Rewrite(from, to)          => f(from).map(Rewrite(_, to))
      case Bracket(pre, post, a)      => f(a).map(Bracket(pre, post, _))
      case Flow(labels, atoms)        => atoms.traverse(f).map(Flow(labels, _))
      case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
    }
  }

  implicit val show: Delay[Show, TextReflowF] = new Delay[Show, TextReflowF] {
    def apply[A](show: Show[A]) = Show.show {
      case Atom(c, ops)               => ops.toString
      case Insert(value)              => s"+'$value'"
      case Rewrite(from, to)          => s"-+'${to}'"
      case Bracket(pre, post, a)      => s"""${pre}`${a.toString}`{post} """
      case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
      case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
    }
  }


}


// TODO rename this to avoid object name/type clashes
object TextReflow {
  import TextReflowF._

  type TextReflow = Fix[TextReflowF]

  type TextReflowU = TextReflowF[Fix[TextReflowF]]

  def fixf = Fix[TextReflowF](_)


  def atom[AtomT](c: AtomT, ops:TextReflowAtomOps) = fixf(Atom(c, ops))

  def flow(as:TextReflow*) = flows(as)
  def flows(as: Seq[TextReflow]) = fixf(Flow(Set(), as.toList))

  def bracket(pre: Char, post: Char, a:TextReflow) = fixf(
    Bracket(pre.toString, post.toString, a)
  )
  def bracket(pre: String, post: String, a:TextReflow) = fixf(
    Bracket(pre, post, a)
  )

  def labeled(l: Label, a:TextReflow) = fixf(Labeled(Set(l), a))
  def insert(s: String) = fixf(Insert(s))
  def space() = insert(" ")



  def addLabel(l: Label): TextReflow => TextReflow = tr => fixf(tr.unFix match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  })

  import utils.SlicingAndDicing._


  def groupByPairs(reflow: TextReflowU)(
    groupf: (TextReflowU, TextReflowU, Int) => Boolean,
    onGrouped: List[TextReflowU] => List[TextReflowU] = (w => w)
  ): TextReflowU = {
    reflow match {
      case f @ Flow(labels, as) =>
        val grouped = as
          .groupByPairsWithIndex({
            case (a, b, i) => groupf(a.unFix, b.unFix, i)
          })
          .map(g =>Flow(labels, g.toList))
          .toList

        f.copy(as = onGrouped(grouped).map(fixf(_)))

      case x =>
        println(s"unmatched ${x}")
        x

    }
  }

  def hasLabel(l: Label): TextReflowU => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def everySequence(r: TextReflow)(f: List[TextReflow] => List[TextReflow]): TextReflow = {
    def atFlows: TextReflowU => TextReflowU = r => r match {
      case fl @ Flow(labels: Set[Label], as: List[TextReflow]) =>
        fl.copy(as = f(as))
      case fl => fl
    }

    r.transCata(atFlows)
  }

  def everyLabel(l: Label, r: TextReflow)(f: TextReflow => TextReflow): TextReflow = {
    def ifLabeled(r:TextReflowU): TextReflowU =  {
      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (a, fWhole)) => fWhole(f(a))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def everywhere(r: TextReflow)(f: TextReflowU => TextReflowU): TextReflow = {
    r.transCata(f)
  }

  def prettyPrintTree(reflow: TextReflow): TB.Box = {
    import utils.ScalazTreeImplicits._
    reflow.cata(toTree).draw
  }
  def prettyPrintCofree[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B],
    CS: Delay[Show, Cofree[TextReflowF, ?]]
  ): String = {
    CS(BS).shows(cof)
  }
  // Tree.Node(c.head, c.tail.map(from(_)))
  def cofreeToTree[A](c: Cofree[TextReflowF, A]): Tree[A] = {
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeToTree(_))
    )
  }
  def printCofree[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B],
    CS: Delay[Show, Cofree[TextReflowF, ?]]
  ): TB.Box = {
    import utils.ScalazTreeImplicits._
    // CS(BS).shows(cof)
    cofreeToTree(cof).draw
  }

  private def mkPad(s: String): TextReflow = insert(s)

  def join(sep:String)(bs:TextReflow*): TextReflow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextReflow]): TextReflow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextReflow]): TextReflow = {
    flows(bs)
  }


  implicit object RangesInts extends Monoid[Ranges.Ints] with Show[Ranges.Ints] with Equal[Ranges.Ints] with Order[Ranges.Ints] with IsEmpty[Lambda[a => Ranges.Ints]] {
    import Ranges.Ints

    type SA[A] = Ints
    def append(f1: Ranges.Ints, f2: => Ranges.Ints) = Ints(math.min(f1.min, f2.min), math.max(f1.max, f2.max))
    def zero: Ranges.Ints = Ranges.Ints(0, 0)
    override def shows(f: Ranges.Ints): String = s"[${f.min}-${f.max}]"
    def order(r1: Ranges.Ints, r2: Ranges.Ints) = Ordering.fromLessThan(r1, r2) { (a, b) => a.min < b.min && a.max < b.max }
    override def equal(x: Ranges.Ints, y: Ranges.Ints) = x.min==y.min && x.max==y.max
    override def equalIsNatural: Boolean = true
    def empty[A] = zero
    def plus[A](f1: SA[A], f2: => SA[A]) = append(f1, f2)
    def isEmpty[A](s: SA[A]) = equal(s, empty)
  }

}

    // def modifyCharAtom[T](i: Int)(func : TextReflow => TextReflow): TextReflow = {
    //   val cRanges: scalaz.Cofree[TextReflowF, IRange] = theReflow.annotateCharRanges

    //   val CFT = recCorecFunctorT[Cofree[?[_], IRange]]

    //   // CFT.transCata(t: Cofree[F, (Int, Int)]) { (_0:  => Cofree[α$5$, (Int, Int)], _1: F) => G }
    //   val scanRes = cRanges.scanr[TextReflow]({ case (r, trc:TextReflowF[Cofree[TextReflowF, TextReflow]]) =>
    //     val range1 = r._1
    //     val range2 = r._2
    //     val scr = if (range1 == i) {
    //       println("(modifying)")
    //       // val qwer = trc.traverse(_.head.project)

    //       val sdf =trc.map({ rf =>
    //         func(rf.head)
    //       })
    //       fixf(sdf)

    //     } else {
    //       val sdf =trc.map({ rf =>
    //         rf.head
    //       })
    //       fixf(sdf)
    //     }
    //     println(s"scanr:  ${r}")
    //     println(prettyPrintTree(scr))

    //     scr
    //   })

    //   scanRes.head
    //   // // cof: Cofree[TextReflowF, (Int, Int)]
    //   // CFT.transCataT(cRanges)({  cof =>
    //   //   val charN = cof.head._1
    //   //   if (i == charN) {
    //   //     cof match {
    //   //       case ll @ Cofree(h, t) =>
    //   //         t.map({case uio =>
    //   //         })
    //   //     }
    //   //     cof
    //   //   } else {
    //   //     cof
    //   //   }
    //   // })

    // }

// def clipToTargetRegion(targetRegion: TargetRegion): Option[(TextReflow, Int@@Offset, Int@@Length)] = {
//   // val clippedFlow = theReflow.flow
//   //   .zipWithIndex
//   //   .dropWhile({case (funit, _) =>
//   //     val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
//   //       !intersects
//   //   })
//   //   .reverse
//   //   .dropWhile({case (funit, _) =>
//   //     val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
//   //       !intersects
//   //   })
//   //   .reverse

//   // if (clippedFlow.isEmpty) None else {
//   //   val start = clippedFlow.head._2
//   //   val end = clippedFlow.last._2

//   //   Some((TextFlow(clippedFlow.map(_._1)), Offset(start), Length(end-start)))
//   // }
//   ???
// }
