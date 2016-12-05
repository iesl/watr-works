package edu.umass.cs.iesl.watr
package textreflow

import watrmarks._
import textboxing.{TextBoxing => TB}

import scalaz._
import Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

class TextReflowAtomOps(
  val chars: Seq[Char]
) {
  override def toString = chars.mkString
}


sealed trait TextReflowF[+A]

object TextReflowF {
  case class Atom[A](c: Any, ops: TextReflowAtomOps)      extends TextReflowF[A]
  case class Insert[A](value: String)                     extends TextReflowF[A]
  case class Rewrite[A](from: A, to: String)              extends TextReflowF[A]
  case class Bracket[A](pre: String, post: String, a: A)  extends TextReflowF[A]
  case class Flow[A](labels: Set[Label], as: List[A])     extends TextReflowF[A]
  case class Labeled[A](labels: Set[Label], a: A)         extends TextReflowF[A]

  implicit val TextReflowTraverse: Traverse[TextReflowF] = new Traverse[TextReflowF] {
    def traverseImpl[G[_], A, B](fa: TextReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[TextReflowF[B]] = fa match {
      case Atom(c, ops)               => G.point(Atom(c, ops))
      case Insert(value)              => G.point(Insert(value))
      case Rewrite(fromA, to)         => f(fromA).map(Rewrite(_, to))
      case Bracket(pre, post, a)      => f(a).map(Bracket(pre, post, _))
      case Flow(labels, as)           => as.traverse(f).map(Flow(labels, _))
      case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
    }
  }

  implicit val TextReflowShow: Delay[Show, TextReflowF] = new Delay[Show, TextReflowF] {
    def apply[A](show: Show[A]) = Show.show {
      case Atom(c, ops)               => ops.toString
      case Insert(value)              => s"+'$value'"
      case Rewrite(from, to)          => s"-+'${to}'"
      case Bracket(pre, post, a)      => s"""${pre}`${a.toString}`{post} """
      case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
      case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
    }
  }

  implicit val equal: Delay[Equal, TextReflowF] = new Delay[Equal, TextReflowF] {
    def apply[α](eq: Equal[α]) = Equal.equal((a, b) => {
      implicit val ieq = eq;
      val isEq= (a, b) match {
        case (Atom(c, ops)          , Atom(c2, ops2))           => ops.toString()==ops2.toString()
        case (Insert(value)         , Insert(value2))           => value == value2
        case (Rewrite(from, to)     , Rewrite(from2, to2))      => from === from2 && to == to2
        case (Bracket(pre, post, a) , Bracket(pre2, post2, a2)) => pre==pre && post==post && a===a2
        case (Flow(ls, atoms)       , Flow(ls2, atoms2))        => ls == ls2 && atoms === atoms2
        case (Labeled(ls, a)        , Labeled(ls2, a2))         => ls == ls2 && a === a2
        case (_                     , _)                        => false
      }

      isEq
    })
  }

}


trait TextReflowFunctions {
  import TextReflowF._
  import utils.SlicingAndDicing._

  def fixf = Fix[TextReflowF](_)

  def atom[AtomT](c: AtomT, ops:TextReflowAtomOps) = fixf(Atom(c, ops))
  def rewrite(t: TextReflow, s: String) = fixf(Rewrite(t, s))
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

  private def mkPad(s: String): TextReflow = insert(s)

  def addLabel(l: Label): TextReflow => TextReflow = tr => fixf(tr.unFix match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  })


  def join(sep:String)(bs:TextReflow*): TextReflow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextReflow]): TextReflow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextReflow]): TextReflow = {
    flows(bs)
  }

  def groupByPairs(reflow: TextReflowT)(
    groupf: (TextReflowT, TextReflowT, Int) => Boolean,
    onGrouped: List[TextReflowT] => List[TextReflowT] = (w => w)
  ): TextReflowT = {
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

  def hasLabel(l: Label): TextReflowT => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def everySequence(r: TextReflow)(f: List[TextReflow] => List[TextReflow]): TextReflow = {
    def atFlows: TextReflowT => TextReflowT = r => r match {
      case fl @ Flow(labels: Set[Label], as: List[TextReflow]) =>
        fl.copy(as = f(as))
      case fl => fl
    }

    r.transCata(atFlows)
  }

  def everyLabel(l: Label, r: TextReflow)(f: TextReflow => TextReflow): TextReflow = {
    def ifLabeled(r:TextReflowT): TextReflowT =  {
      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (a, fWhole)) => fWhole(f(a))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def everywhere(r: TextReflow)(f: TextReflowT => TextReflowT): TextReflow = {
    r.transCata(f)
  }

  import utils.ScalazTreeImplicits._

  def boxTF[T, F[_]: Foldable: Functor](
    tf: T
  )(implicit
    TR: Recursive.Aux[T, F],
    FShow: Delay[Show, F]
  ): TB.Box = {
    tf.cata(toTree).drawBox
  }

  def prettyPrintTree(reflow: TextReflow): TB.Box = {
    reflow.cata(toTree).drawBox
    // boxTF(reflow)
  }


  def prettyPrintCofree[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B],
    CS: Delay[Show, Cofree[TextReflowF, ?]]
  ): String = {
    CS(BS).shows(cof)
  }

  def cofreeToTree[A](c: Cofree[TextReflowF, A]): Tree[A] = {
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeToTree(_))
    )
  }

}
