package edu.umass.cs.iesl.watr
package textflow

import watrmarks._

object GeneralizedReflow {
  import scalaz._, Scalaz._
  import matryoshka._,  Recursive.ops._, TraverseT.ops._, Corecursive.ops._


  sealed trait Reflow[+A]


  object Reflow {
    case class Atom(c: Char)                                          extends Reflow[Nothing]
    case class Edit(value: String)                                    extends Reflow[Nothing]
    case class Flow[A](atoms: List[A])                               extends Reflow[A]
    case class Window[A](hole: A, prevs: List[A], nexts: List[A])    extends Reflow[A]
    // case class Focus[A](hole: A, prevs: A, nexts: A)    extends Reflow[A]

    // case class Contiguous[A](a: A)                                   extends Reflow[A]
    // case class Rewrite[A](from: A, to: String)                     extends Reflow[A]
    // case class Labeled[A](a: A, l: Label)                            extends Reflow[A]
    // case class Clipped[A](cc: A, start: Int, len: Int) extends Reflow[A]
    // case class Superimposed


    implicit val ReflowTraverse: Traverse[Reflow] = new Traverse[Reflow] {
      def traverseImpl[G[_], A, B](fa: Reflow[A])(f: A => G[B])(implicit G: Applicative[G]): G[Reflow[B]] = fa match {
        case Atom(c)                    => G.point(Atom(c))
        case Edit(value)                => G.point(Edit(value))
        case Flow(atoms)                => atoms.traverse(f).map(Flow(_))
        case Window(hole, prevs, nexts) =>

          implicitly[Apply[G]].apply3(
            f(hole), prevs.traverse(f), nexts.traverse(f)
          )(Window(_, _, _))

        // case Focus(hole, prevs, nexts) =>

        //   implicitly[Apply[G]].apply3(
        //     f(hole), f(prevs), f(nexts)
        //   )(Focus(_, _, _))
      }
    }
    implicit val ReflowUnzip = new Unzip[Reflow] {
      def unzip[A, B](f: Reflow[(A, B)]) = (f.map(_._1), f.map(_._2))
    }

    implicit def show[A]: Show[Reflow[A]] = Show.show {
      case Atom(c)                    => s"atom:$c"
      case Edit(value)                => s"edit:$value"
      case Flow(atoms)                => s"flow"
      case Window(hole, prevs, nexts) => s"window"
      // case Focus(hole, prevs, nexts)  => s"focus"
    }

  }

  import Reflow._

  type ReflowF = Fix[Reflow]

  def fix = Fix[Reflow](_)

  def atom(c: Char) = fix(Atom(c))
  def flow(as: Fix[Reflow]*) = fix(Flow(as.toList))
  def flows(as: Seq[Fix[Reflow]]) = fix(Flow(as.toList))

  def edit(s: String) = fix(Edit(s))
  def window[F[_]](hole: ReflowF, prevs: List[ReflowF], nexts: List[ReflowF]) = fix(Window(hole, prevs, nexts))

}
