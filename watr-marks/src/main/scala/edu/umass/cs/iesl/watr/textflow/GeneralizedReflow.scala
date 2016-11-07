package edu.umass.cs.iesl.watr
package textflow

import spindex._
import watrmarks._
import textboxing.{TextBoxing => TB}

import scalaz._, Scalaz.{fix => _, _}


class GeneralizedReflow[AtomT](
  implicit showAtomT: Show[AtomT]
) {
  import matryoshka._,  Recursive.ops._, TraverseT.ops._
  import matryoshka.data._

  sealed trait ReflowF[+A]

  type Reflow = Fix[ReflowF]

  type TextReflow = Reflow

  def fixf = Fix[ReflowF](_)


  object Reflow {
    case class Atom(c: AtomT)                               extends ReflowF[Nothing]
    case class Insert(value: String)                        extends ReflowF[Nothing]
    case class Rewrite[A](from: A, to: String)              extends ReflowF[A]
    case class Bracket[A](pre: String, post: String, a: A)  extends ReflowF[A]

    case class Flow[A](labels: Set[Label], as: List[A])     extends ReflowF[A]
    case class Labeled[A](labels: Set[Label], a: A)         extends ReflowF[A]

    implicit val ReflowTraverse: Traverse[ReflowF] = new Traverse[ReflowF] {
      def traverseImpl[G[_], A, B](fa: ReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[ReflowF[B]] = fa match {
        case Atom(c)                    => G.point(Atom(c))
        case Insert(value)              => G.point(Insert(value))
        case Rewrite(from, to)          => f(from).map(Rewrite(_, to))
        case Bracket(pre, post, a)      => f(a).map(Bracket(pre, post, _))
        case Flow(labels, atoms)        => atoms.traverse(f).map(Flow(labels, _))
        case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
      }
    }

    // implicit def show[A]: Show[ReflowF[A]] = Show.show { _ match {
    //   case Atom(c)                    => c.toString
    //   case Insert(value)              => s"+$value"
    //   case Rewrite(from, to)          => s"rewrite"
    //   case Bracket(pre, post, a)      => s"""${pre}`${a.toString}`{post} """
    //   case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
    //   case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
    // }}

    implicit val show: Delay[Show, ReflowF] = new Delay[Show, ReflowF] {
      def apply[A](show: Show[A]) = Show.show {
          case Atom(c)                    => c.toString
          case Insert(value)              => s"+$value"
          case Rewrite(from, to)          => s"rewrite"
          case Bracket(pre, post, a)      => s"""${pre}`${a.toString}`{post} """
          case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
          case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""

          // case Num(v)       => v.shows
          // case Mul(a, b)    =>
          //   "Mul(" + show.shows(a) + ", " + show.shows(b) + ")"
          // case Var(s)       => "$" + s.name
          // case Lambda(p, a) => "Lambda(" + p.name + ", " + show.shows(a) + ")"
          // case Apply(f, a)  =>
          //   "Apply(" + show.shows(f) + ", " + show.shows(a) + ")"
          // case Let(n, v, i) =>
          //   "Let(" + n.name + ", " + show.shows(v) + ", " + show.shows(i) + ")"
        }
    }

  }

  import Reflow._

  def atom(c: AtomT) = fixf(Atom(c))
  def flow(as: Reflow*) = flows(as)
  def flows(as: Seq[Reflow]) = fixf(Flow(Set(), as.toList))

  def bracket(pre: Char, post: Char, a: Reflow) = fixf(
    Bracket(pre.toString, post.toString, a)
  )
  def bracket(pre: String, post: String, a: Reflow) = fixf(
    Bracket(pre, post, a)
  )

  def labeled(l: Label, a: Reflow) = fixf(Labeled(Set(l), a))
  def insert(s: String) = fixf(Insert(s))
  def space() = insert(" ")


  type ReflowU = ReflowF[Fix[ReflowF]]

  def addLabel(l: Label): ReflowU => ReflowU = _ match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  }

  // Linearize a tree structure
  // import watrmarks.{StandardLabels => LB, _}
  import utils.SlicingAndDicing._


  def groupByPairs(reflow: ReflowU)(
    groupf: (ReflowU, ReflowU, Int) => Boolean,
    onGrouped: List[Flow[Reflow]] => List[ReflowU] = (w => w)
  ): ReflowU = {
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


  def hasLabel(l: Label): ReflowU => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def allSiblings(label: Label, r: Reflow)(
    f: List[Reflow] => List[Reflow]
  ): Reflow = {

    // everyLabel(label, r)({labeled => })

    // everySequence(r)({ seq: (List[Reflow]) => 
    //   seq
    // })
    // r.children.exists { ch: Fix[ReflowF] =>


    //   false
    // }

    // def ifLabeled(r:ReflowU): ReflowU =  {
    //   if (hasLabel(label)(r)) holes(r) match {
    //     case Labeled(labels, (a, fWhole)) =>
    //     case _ => r
    //   } else r
    // }

    ???
  }


  def everySequence(r: Reflow)(f: List[Reflow] => List[Reflow]): Reflow = {
    def atFlows: ReflowU => ReflowU = r => r match {
      case fl @ Flow(labels: Set[Label], as: List[Reflow]) =>
        fl.copy(as = f(as))
      case fl => fl
    }

    r.transCata(atFlows)
  }

  def everyLabel(l: Label, r: Reflow)(f: Reflow => Reflow): Reflow = {
    def ifLabeled(r:ReflowU): ReflowU =  {
      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (a, fWhole)) => fWhole(f(a))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def everywhere(r: Reflow)(f: ReflowU => ReflowU): Reflow = {
    r.transCata(f)
  }

  def prettyPrintTree(reflow: Reflow): TB.Box = {
    import utils.ScalazTreeImplicits._
    reflow.cata(toTree).draw
  }

  def toText(reflow: Reflow): String = {
    ???
  }


  private def mkPad(s: String): TextReflow = ???

  def join(sep:String)(bs:TextReflow*): TextReflow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextReflow]): TextReflow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextReflow]): TextReflow = {
    ???
  }

  implicit class RicherTextReflow(val theReflow: TextReflow)   {

    def toText(): String = {
      ???
    }

    def slice(begin: Int, end:Int): TextReflow = ???

    def targetRegions(): Seq[TargetRegion] = ???

    def intersect(other: TextReflow): TextReflow = ???

    def intersectPage(other: PageIndex): Seq[Component] = {
      ???
    }
  }



}

object GeneralizedReflow {
  implicit def ShowAtomicComponent: Show[AtomicComponent] =
    Show.show { _.chars }

  // implicit class RicherTextReflow(val theReflow: TextReflow) extends AnyVal {
  val textReflow = new GeneralizedReflow[Char]()

  val componentReflow = new GeneralizedReflow[AtomicComponent]()
}
