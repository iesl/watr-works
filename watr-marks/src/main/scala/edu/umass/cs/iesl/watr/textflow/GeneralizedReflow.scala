package edu.umass.cs.iesl.watr
package textflow

import watrmarks._

import textboxing.{TextBoxing => TB}
import utils.ScalazTreeImplicits._


object GeneralizedReflow {
  import scalaz._, Scalaz.{fix => _, _}
  import matryoshka._,  Recursive.ops._, TraverseT.ops._, Corecursive.ops._


  sealed trait TextFlowLeaf

  sealed trait Reflow[+A]

  type FixReflow = Fix[Reflow]
  type ReflowF = Reflow[FixReflow]

  def fixf = Fix[Reflow](_)


  object Reflow {
    case class Atom(c: Char)                          extends Reflow[Nothing] with TextFlowLeaf
    case class Space(breaking: Boolean=true)          extends Reflow[Nothing] with TextFlowLeaf
    case class Anchor(labels: Set[BioPin])            extends Reflow[Nothing] with TextFlowLeaf

    // Edits
    case class Insert(value: String)                  extends Reflow[Nothing]
    case class Rewrite[A](from: A, to: String)        extends Reflow[A]
    case class Delete[A](a: A)                        extends Reflow[A]

    case class Flow[A](labels: Set[Label], as: List[A])  extends Reflow[A]
    case class Labeled[A](labels: Set[Label], a: A)      extends Reflow[A]

    case class Window[A](
      hole: A,
      prevs: List[A],
      nexts: List[A]
    ) extends Reflow[A]



    implicit val ReflowTraverse: Traverse[Reflow] = new Traverse[Reflow] {
      def traverseImpl[G[_], A, B](fa: Reflow[A])(f: A => G[B])(implicit G: Applicative[G]): G[Reflow[B]] = fa match {
        case Atom(c)                    => G.point(Atom(c))
        case Space(breaking)            => G.point(Space(breaking))
        case Anchor(ls)                 => G.point(Anchor(ls))
        case Insert(value)              => G.point(Insert(value))
        case Rewrite(from, to)          => f(from).map(Rewrite(_, to))
        case Delete(a)                  => f(a).map(Delete(_))
        case Flow(labels, atoms)        => atoms.traverse(f).map(Flow(labels, _))
        // case Flow(atoms)                => atoms.traverse(f).map(Flow( _))
        case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
        case Window(hole, prevs, nexts) =>

          implicitly[Apply[G]].apply3(
            f(hole), prevs.traverse(f), nexts.traverse(f)
          )(Window(_, _, _))

      }
    }
    implicit val ReflowUnzip = new Unzip[Reflow] {
      def unzip[A, B](f: Reflow[(A, B)]) = (f.map(_._1), f.map(_._2))
    }

    implicit def show[A]: Show[Reflow[A]] = Show.show { _ match {
      case Atom(c)                    => s"$c"
      case Space(breaking)            => " "
      case Anchor(ls: Set[BioPin])    => s"""anchor:${ls.mkString(",")}"""
      case Insert(value)              => s"edit:$value"
      case Rewrite(from, to)          => s"rewrite"
      case Delete(a)                  => s"del"
      case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
      case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
      case Window(hole, prevs, nexts) => s"window"
    }}

    def asString[A]: Show[Reflow[A]] = Show.show { _ match {
      case Atom(c)                    => c.toString()
      case Space(breaking)            => " "
      case Insert(value)              => value
      case Rewrite(from, to)          => to
      case Anchor(ls: Set[BioPin])    => ""
      case Delete(a)                  => ""
      case Flow(ls, atoms)            => ""
      case Labeled(ls, _)             => ""
      case Window(hole, prevs, nexts) => ""
    }}

  }

  import Reflow._


  def atom(c: Char) = fixf(Atom(c))
  def space() = fixf(Space())
  def anchor(ls: BioPin) = fixf(Anchor(Set(ls)))
  def anchors(ls: Set[BioPin]) = fixf(Anchor(ls))
  def flow(as: FixReflow*) = flows(as)
  def flows(as: Seq[FixReflow]) = fixf(Flow(Set(), as.toList))

  def labeled(l: Label, a: FixReflow) = fixf(Labeled(Set(l), a))

  def insert(s: String) = fixf(Insert(s))
  def window[F[_]](hole: FixReflow, prevs: List[FixReflow], nexts: List[FixReflow]) = fixf(Window(hole, prevs, nexts))


  def addLabel(l: Label): ReflowF => ReflowF = _ match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  }

  // Linearize a tree structure
  import watrmarks.{StandardLabels => LB, _}
  import utils.SlicingAndDicing._


  // onGrouped: Window[FixReflow] => Window[FixReflow] = (w => w)
  def groupByPairs(reflow: ReflowF)(
    groupf: (ReflowF, ReflowF, Int) => Boolean,
    onGrouped: List[Flow[FixReflow]] => List[ReflowF] = (w => w)
  ): ReflowF = {
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


  def hasLabel(l: Label): ReflowF => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def transLabeled(r: FixReflow, l: Label, f: ReflowF => ReflowF): FixReflow = {

    val ifLabeled: ReflowF => ReflowF = r => {

      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (Fix(a), fWhole)) =>  fWhole(fixf(f(a)))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def explode(r: FixReflow): List[FixReflow] = {
    r.unFix match {
      case f @ Flow(labels, atoms) =>

        if (labels.isEmpty) atoms else {
          val bAnchors = anchors(labels.map(_.B))
          val lAnchors = anchors(labels.map(_.L))
          (bAnchors :: atoms) :+ lAnchors
        }

      case Labeled(labels, a)  =>

        if (labels.isEmpty) List(a) else {
          val bAnchors = anchors(labels.map(_.B))
          val lAnchors = anchors(labels.map(_.L))
          List(bAnchors, a, lAnchors)
        }

      case x => List(r)
    }
  }

  def flattenf(r: ReflowF): FixReflow = fixf { r match {
    case Flow(labels, as)     => Flow(labels, as.flatMap(explode(_)))
    case Labeled(labels, a)   => Labeled(labels, flatten(a))
    case x                    => x
  }}

  def flatten(r: FixReflow): FixReflow = fixf {
    r.unFix match {
      case Flow(labels, as)     => Flow(labels, as.flatMap(explode(_)))
      case Labeled(labels, a)   => Labeled(labels, flatten(a))
      case x                    => x
    }
  }

  def linearize(r: ReflowF): ReflowF = {
    fixf(r).cata(flattenf).unFix
  }

  def toLeafList(r: ReflowF): List[TextFlowLeaf]= {
    linearize(r)
      .foldLeft( List[TextFlowLeaf]() ){
      case (acc, e) =>
      if (e.unFix.isInstanceOf[TextFlowLeaf]) {
        e.unFix.asInstanceOf[TextFlowLeaf] :: acc
      } else acc
    }
  }



  def prettyPrintTree(reflow: FixReflow): TB.Box = {
    reflow.cata(toTree).draw
  }



}
