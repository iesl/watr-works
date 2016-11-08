package edu.umass.cs.iesl.watr
package textflow

import spindex._
import watrmarks._
import textboxing.{TextBoxing => TB}

import scalaz._, Scalaz.{fix => _, _}

import matryoshka._,  Recursive.ops._, TraverseT.ops._
import matryoshka.data._

sealed trait TextReflowF[+A]

object TextReflowF {
  case class Atom[T](c: T)                                extends TextReflowF[Nothing]
  case class Insert(value: String)                        extends TextReflowF[Nothing]
  case class Rewrite[A](from: A, to: String)              extends TextReflowF[A]
  case class Bracket[A](pre: String, post: String, a: A)  extends TextReflowF[A]
  case class Flow[A](labels: Set[Label], as: List[A])     extends TextReflowF[A]
  case class Labeled[A](labels: Set[Label], a: A)         extends TextReflowF[A]

  implicit val ReflowTraverse: Traverse[TextReflowF] = new Traverse[TextReflowF] {
    def traverseImpl[G[_], A, B](fa: TextReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[TextReflowF[B]] = fa match {
      case Atom(c)                    => G.point(Atom(c))
      case Insert(value)              => G.point(Insert(value))
      case Rewrite(from, to)          => f(from).map(Rewrite(_, to))
      case Bracket(pre, post, a)      => f(a).map(Bracket(pre, post, _))
      case Flow(labels, atoms)        => atoms.traverse(f).map(Flow(labels, _))
      case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
    }
  }


  implicit val show: Delay[Show, TextReflowF] = new Delay[Show, TextReflowF] {
    def apply[A](show: Show[A]) = Show.show {
      case Atom(c)                    => c.toString
      case Insert(value)              => s"+$value"
      case Rewrite(from, to)          => s"rewrite"
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


  def atom[AtomT](c: AtomT) = fixf(Atom(c))
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



  def addLabel(l: Label): TextReflowU => TextReflowU = _ match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  }

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


  def allSiblings(label: Label, r:TextReflow)(
    f: List[TextReflow] => List[TextReflow]
  ):TextReflow = {
    ???
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

  private def mkPad(s: String): TextReflow = insert(s)

  def join(sep:String)(bs:TextReflow*): TextReflow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextReflow]): TextReflow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextReflow]): TextReflow = {
    flows(bs)
  }

  implicit class RicherReflowU(val theReflow: TextReflowU) extends AnyVal  {

    def hasLabel(l: Label): Boolean = theReflow match {
      case Labeled(labels, _) if labels.contains(l) => true
      case _ => false
    }
  }

  implicit class RicherReflow(val theReflow: TextReflow) extends AnyVal  {

    def slice(begin: Int, end:Int): TextReflow = ???

    def targetRegions(): Seq[TargetRegion] = ???

    def intersect(other: TextReflow): TextReflow = ???

    def intersectPage(other: PageIndex): Seq[Component] = {
      ???
    }


    def clipToTargetRegion(targetRegion: TargetRegion): Option[(TextReflow, Int@@Offset, Int@@Length)] = {
      // val clippedFlow = theReflow.flow
      //   .zipWithIndex
      //   .dropWhile({case (funit, _) =>
      //     val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
      //       !intersects
      //   })
      //   .reverse
      //   .dropWhile({case (funit, _) =>
      //     val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
      //       !intersects
      //   })
      //   .reverse

      // if (clippedFlow.isEmpty) None else {
      //   val start = clippedFlow.head._2
      //   val end = clippedFlow.last._2

      //   Some((TextFlow(clippedFlow.map(_._1)), Offset(start), Length(end-start)))
      // }
      ???
    }


  }

}

