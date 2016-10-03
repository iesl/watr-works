package edu.umass.cs.iesl.watr
package spindex

import watrmarks._

object GeneralizedReflow {
  import matryoshka._,  Recursive.ops._, TraverseT.ops._
  import scalaz._, Scalaz._
  // import matryoshka.instances.fixedpoint
  // import matryoshka.helpers._
  // import matryoshka.patterns._
  // import matryoshka.specs2.scalacheck.CheckAll


  sealed trait Reflow[+A]

  object Reflow {
    // case class Atom(atom: AtomicComponent, origin: Int@@ComponentID) extends Reflow[Nothing]
    case class Atom[C](c: C)                                         extends Reflow[Nothing]
    case class Edit(value: String)                                   extends Reflow[Nothing]
    case class Flow[A](atoms: List[A])                               extends Reflow[A]
    // case class Contiguous[A](a: A)                                   extends Reflow[A]
    // case class Rewrite[A](from: A, to: String)                     extends Reflow[A]
    // case class Labeled[A](a: A, l: Label)                            extends Reflow[A]
    // case class Focus[A](hole: A, rprevs: List[A], nexts: List[A])    extends Reflow[A]

    // case class Clipped[A](cc: A, start: Int, len: Int) extends Reflow[A]
    // case class Superimposed




    implicit val ReflowTraverse: Traverse[Reflow] = new Traverse[Reflow] {
      def traverseImpl[G[_], A, B](fa: Reflow[A])(f: A => G[B])(implicit G: Applicative[G]): G[Reflow[B]] = fa match {
        // case Atom(atom, origin)         => G.point(Atom(atom, origin))
        case Atom(c)                    => G.point(Atom(c))
        case Edit(value)                => G.point(Edit(value))
        case Flow(atoms)                => atoms.traverse(f).map(Flow(_))
        // case Contiguous(a)              => f(a).map(Contiguous(_))
        // case Rewrite(from, to)     => ??? // G.apply2(f(v), f(i))(Let(n, _, _))
        // case Labeled(flow, l)           => ??? // G.apply2(f(v), f(i))(Let(n, _, _))
        // case Focus(hole, rprevs, nexts) => ??? // G.apply2(f(v), f(i))(Let(n, _, _))
      }
    }
    implicit val ReflowUnzip = new Unzip[Reflow] {
      def unzip[A, B](f: Reflow[(A, B)]) = (f.map(_._1), f.map(_._2))
    }

  }

  import Reflow._

  def atom[C](c: C) = Atom(c)
  def edit(s: String) = Edit(s)
  def flow[A](as: Reflow[A]*) = Flow(as.toList)

}
