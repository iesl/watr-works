package edu.umass.cs.iesl.watr
package textflow

import watrmarks._

import textboxing.{TextBoxing => TB}
import utils.ScalazTreeImplicits._


object GeneralizedReflow {
  import scalaz._, Scalaz.{fix => _, _}
  import matryoshka._,  Recursive.ops._, TraverseT.ops._, Corecursive.ops._


  sealed trait TextFlowLeaf

  sealed trait ReflowF[+A]

  type Reflow = Fix[ReflowF]

  def fixf = Fix[ReflowF](_)


  object Reflow {
    case class Element[T](t: T, chars: List[Char])    extends ReflowF[Nothing]
    case class Element1[T](t: T, char: Char)          extends ReflowF[Nothing]

    case class Atom(c: Char)                          extends ReflowF[Nothing] with TextFlowLeaf
    case class Space(breaking: Boolean=true)          extends ReflowF[Nothing] with TextFlowLeaf
    case class Anchor(labels: Set[BioPin])            extends ReflowF[Nothing] with TextFlowLeaf

    // Edits
    case class Insert(value: String)                  extends ReflowF[Nothing]
    case class Rewrite[A](from: A, to: String)        extends ReflowF[A]
    case class Delete[A](a: A)                        extends ReflowF[A]

    case class Flow[A](labels: Set[Label], as: List[A])  extends ReflowF[A]
    case class Labeled[A](labels: Set[Label], a: A)      extends ReflowF[A]

    case class Window[A](
      hole: A,
      prevs: List[A],
      nexts: List[A]
    ) extends ReflowF[A]



    implicit val ReflowTraverse: Traverse[ReflowF] = new Traverse[ReflowF] {
      def traverseImpl[G[_], A, B](fa: ReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[ReflowF[B]] = fa match {
        case Atom(c)                    => G.point(Atom(c))
        case e:Element[_]               => G.point(e)
        case e:Element1[_]              => G.point(e)
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
    implicit val ReflowUnzip = new Unzip[ReflowF] {
      def unzip[A, B](f: ReflowF[(A, B)]) = (f.map(_._1), f.map(_._2))
    }

    implicit def show[A]: Show[ReflowF[A]] = Show.show { _ match {
      case Atom(c)                    => c.toString
      case e:Element[_]               => e.chars.mkString
      case e:Element1[_]              => e.char.toString
      case Space(breaking)            => " "
      case Anchor(ls: Set[BioPin])    => s"""anchor:${ls.mkString(",")}"""
      case Insert(value)              => s"edit:$value"
      case Rewrite(from, to)          => s"rewrite"
      case Delete(a)                  => s"del"
      case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
      case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
      case Window(hole, prevs, nexts) => s"window"
    }}

    def asString[A]: Show[ReflowF[A]] = Show.show { _ match {
      case Atom(c)                    => c.toString()
      case e:Element[_]               => show.shows(e)
      case e:Element1[_]              => show.shows(e)
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


  def element[T](t: T, charf: (T) => Seq[Char]) = fixf({
    val cs = charf(t)

    if (cs.length == 1) Element1(t, cs(0))
    else Element(t, cs.toList)
  })

  def atom(c: Char) = fixf(Atom(c))
  def space() = fixf(Space())
  def anchor(ls: BioPin) = fixf(Anchor(Set(ls)))
  def anchors(ls: Set[BioPin]) = fixf(Anchor(ls))
  def flow(as: Reflow*) = flows(as)
  def flows(as: Seq[Reflow]) = fixf(Flow(Set(), as.toList))

  def labeled(l: Label, a: Reflow) = fixf(Labeled(Set(l), a))

  def insert(s: String) = fixf(Insert(s))
  def window[F[_]](hole: Reflow, prevs: List[Reflow], nexts: List[Reflow]) = fixf(Window(hole, prevs, nexts))


  type ReflowU = ReflowF[Fix[ReflowF]]

  def addLabel(l: Label): ReflowU => ReflowU = _ match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  }

  // Linearize a tree structure
  import watrmarks.{StandardLabels => LB, _}
  import utils.SlicingAndDicing._


  // onGrouped: Window[Reflow] => Window[Reflow] = (w => w)
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


  def transLabeled(r: Reflow, l: Label, f: ReflowU => ReflowU): Reflow = {

    val ifLabeled: ReflowU => ReflowU = r => {

      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (Fix(a), fWhole)) =>  fWhole(fixf(f(a)))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def explode(r: Reflow): List[Reflow] = {
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

  def unFlow(r: Reflow): Reflow = fixf { r.unFix match {
    case Flow(labels, as)     => Flow(labels, as.flatMap(explode(_)))
    // case Labeled(labels, a)   => Labeled(labels, flattenf(a.unFix))
    case x                    => x
  }}

  // def flatten(r: Reflow): Reflow = fixf {
  //   r.unFix match {
  //     case Flow(labels, as)     => Flow(labels, as.flatMap(explode(_)))
  //     case Labeled(labels, a)   => Labeled(labels, flatten(a))
  //     case x                    => x
  //   }
  // }

  // def linearize(r: Reflow): Reflow = {
  //   r.cata(unFlow).unFix
  // }

  def toLeafList(r: ReflowU): List[TextFlowLeaf] = ???


  // def toLeafList(r: ReflowU): List[TextFlowLeaf]= {
  //   linearize(r)
  //     .foldLeft( List[TextFlowLeaf]() ){
  //     case (acc, e) =>
  //     if (e.unFix.isInstanceOf[TextFlowLeaf]) {
  //       e.unFix.asInstanceOf[TextFlowLeaf] :: acc
  //     } else acc
  //   }
  // }



  def prettyPrintTree(reflow: Reflow): TB.Box = {
    reflow.cata(toTree).draw
  }

  def toText(reflow: Reflow): String = {
    ???
  }


}
