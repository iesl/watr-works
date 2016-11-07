package edu.umass.cs.iesl.watr
package textflow

import matryoshka._
import matryoshka.data._
import org.scalacheck._
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._


sealed trait Exp[+A]

object Exp  {


  case class Num[A](value: Int) extends Exp[A]
  case class Mul[A](left: A, right: A) extends Exp[A]
  case class Var[A](value: Symbol) extends Exp[A]
  case class Lambda[A](param: Symbol, body: A) extends Exp[A]
  case class Apply[A](func: A, arg: A) extends Exp[A]
  case class Let[A](name: Symbol, value: A, inBody: A) extends Exp[A]

  def num(v: Int) = Fix[Exp](Num(v))
  def mul(left: Fix[Exp], right: Fix[Exp]) = Fix[Exp](Mul(left, right))
  def vari(v: Symbol) = Fix[Exp](Var(v))
  def lam(param: Symbol, body: Fix[Exp]) = Fix[Exp](Lambda(param, body))
  def ap(func: Fix[Exp], arg: Fix[Exp]) = Fix[Exp](Apply(func, arg))
  def let(name: Symbol, v: Fix[Exp], inBody: Fix[Exp]) = Fix[Exp](Let(name, v, inBody))


  implicit val arbSymbol = Arbitrary(Arbitrary.arbitrary[String].map(Symbol(_)))

  implicit val arbitrary: Delay[Arbitrary, Exp] = new Delay[Arbitrary, Exp] {
    def apply[α](arb: Arbitrary[α]): Arbitrary[Exp[α]] =
      Arbitrary(Gen.oneOf(
        Arbitrary.arbitrary[Int].map(Num[α](_)),
        (arb.arbitrary |@| arb.arbitrary)(Mul(_, _)),
        Arbitrary.arbitrary[Symbol].map(Var[α](_)),
        (Arbitrary.arbitrary[Symbol] ⊛ arb.arbitrary)(Lambda(_, _)),
        (arb.arbitrary ⊛ arb.arbitrary)(Apply(_, _)),
        (Arbitrary.arbitrary[Symbol] ⊛ arb.arbitrary ⊛ arb.arbitrary)(
          Let(_, _, _))))
  }

  implicit val traverse: Traverse[Exp] = new Traverse[Exp] {
    def traverseImpl[G[_], A, B](fa: Exp[A])(f: A => G[B])(implicit G: Applicative[G]): G[Exp[B]] = fa match {
      case Num(v)           => G.point(Num(v))
      case Mul(left, right) => G.apply2(f(left), f(right))(Mul(_, _))
      case Var(v)           => G.point(Var(v))
      case Lambda(p, b)     => G.map(f(b))(Lambda(p, _))
      case Apply(func, arg) => G.apply2(f(func), f(arg))(Apply(_, _))
      case Let(n, v, i)     => G.apply2(f(v), f(i))(Let(n, _, _))
    }
  }

  // implicit val recurse: Recursive[Exp] = traverse
  // implicit val corecurse: Corecursive[Exp] = traverse
  // implicit val functor: Functor[Exp] = traverse
  // implicit val foldable: Foldable[Exp] = traverse

  // NB: an unusual definition of equality, in that only the first 3 characters
  //     of variable names are significant. This is to distinguish it from `==`
  //     as well as from a derivable Equal.
  implicit val equal: Delay[Equal, Exp] = new Delay[Equal, Exp] {
    def apply[α](eq: Equal[α]) =
      Equal.equal[Exp[α]] {
        case (Num(v1), Num(v2))                 => v1 ≟ v2
        case (Mul(a1, b1), Mul(a2, b2))         =>
          eq.equal(a1, a2) && eq.equal(b1, b2)
        case (Var(s1), Var(s2))                 =>
          s1.name.substring(0, 3 min s1.name.length) ==
          s2.name.substring(0, 3 min s2.name.length)
        case (Lambda(p1, a1), Lambda(p2, a2))   =>
          p1 == p2 && eq.equal(a1, a2)
        case (Apply(f1, a1), Apply(f2, a2))     =>
          eq.equal(f1, f2) && eq.equal(a1, a2)
        case (Let(n1, v1, i1), Let(n2, v2, i2)) =>
          n1 == n2 && eq.equal(v1, v2) && eq.equal(i1, i2)
        case (_, _)                             => false
      }
  }


  // NB: Something like this currently needs to be defined for any Functor in
  //     order to get the generalize operations for the algebra.
  implicit def toExpAlgebraOps[A](a: Algebra[Exp, A]): AlgebraOps[Exp, A] =
    toAlgebraOps[Exp, A](a)

  implicit val show: Delay[Show, Exp] = new Delay[Show, Exp] {
    def apply[α](show: Show[α]) =
      Show.show {
        case Num(v)       => v.shows
        case Mul(a, b)    =>
          "Mul(" + show.shows(a) + ", " + show.shows(b) + ")"
        case Var(s)       => "$" + s.name
        case Lambda(p, a) => "Lambda(" + p.name + ", " + show.shows(a) + ")"
        case Apply(f, a)  =>
          "Apply(" + show.shows(f) + ", " + show.shows(a) + ")"
        case Let(n, v, i) =>
          "Let(" + n.name + ", " + show.shows(v) + ", " + show.shows(i) + ")"
      }
  }

  // implicit val unzip = new Unzip[Exp] {
  //   def unzip[A, B](f: Exp[(A, B)]) = (f.map(_._1), f.map(_._2))
  // }


}

sealed trait Exp2[A]

object Exp2 {

  case class Const[A]() extends Exp2[A]
  case class Num2[A](value: Int) extends Exp2[A]
  case class Single[A](a: A) extends Exp2[A]

  implicit val arbitrary: Delay[Arbitrary, Exp2] = new Delay[Arbitrary, Exp2] {
    def apply[α](arb: Arbitrary[α]): Arbitrary[Exp2[α]] =
      Arbitrary(Gen.oneOf(
        Gen.const(Const[α]),
        Arbitrary.arbitrary[Int].map(Num2[α](_)),
        arb.arbitrary.map(Single(_))))
  }

  // NB: This isn’t implicit in order to allow us to test our low-priority
  //     instances for CoEnv.
  val traverse: Traverse[Exp2] = new Traverse[Exp2] {
    def traverseImpl[G[_], A, B](
      fa: Exp2[A])(
      f: (A) ⇒ G[B])(
      implicit G: Applicative[G]) =
      fa match {
        case Const()   => G.point(Const[B]())
        case Num2(v)   => G.point(Num2[B](v))
        case Single(a) => f(a) ∘ (Single(_))
      }
  }

  implicit val functor: Functor[Exp2] = traverse
  implicit val foldable: Foldable[Exp2] = traverse

  implicit val show: Delay[Show, Exp2] = new Delay[Show, Exp2] {
    def apply[α](show: Show[α]) =
      Show.show {
        case Const()   => "Const()"
        case Num2(v)   => "Num2(" + v.shows + ")"
        case Single(a) => "Single(" + show.shows(a) + ")"
      }
  }

  implicit val equal: Delay[Equal, Exp2] = new Delay[Equal, Exp2] {
    def apply[α](eq: Equal[α]) =
      Equal.equal[Exp2[α]] {
        case (Const(), Const())       => true
        case (Num2(v1), Num2(v2))     => v1 ≟ v2
        case (Single(a1), Single(a2)) => eq.equal(a1, a2)
        case _                        => false
      }
  }

  def const = Fix[Exp2](Const())
  def num2(v: Int) = Fix[Exp2](Num2(v))
  def single(a: Fix[Exp2]) = Fix[Exp2](Single(a))
}
