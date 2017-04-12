package edu.umass.cs.iesl.watr
package utils


import matryoshka.data.Fix
import matryoshka.Recursive
import shapeless.ops.hlist.Tupler

import scala.language.higherKinds
import scala.reflect.ClassTag
import scalaz.Functor

object UnFixable {

  import shapeless._

  trait Extractor[A] {
    type Out
    def unapply(v: A): Option[Out]
  }

  object Extractor {
    type Aux[T, R] = Extractor[T] { type Out = R }
    def apply[T, R](implicit extractor: Aux[T, R]): Aux[T, R] = extractor
  }

  final class RecursiveExtractor[Re, T[_] <: F[_], F[_]](implicit aux: Recursive.Aux[Re, F], func: Functor[F]) {
    def tuple[R <: HList, O]
    (implicit
     tag: ClassTag[T[Re]],
     gen: Generic[T[Re]] {type Repr = R},
     tupler: Tupler[R] {type Out = O}
    )
    : Extractor.Aux[Re, O] = new Extractor[Re] {

      override type Out = O
      override def unapply(t: Re): Option[O] = tag.unapply(aux.project(t)).map(v => gen.to(v).tupled)
    }

    def single[O]
    (implicit
     tag: ClassTag[T[Re]],
     gen: Generic[T[Re]] {type Repr = O :: HNil}
    )
    : Extractor.Aux[Re, O] = new Extractor[Re] {

      override type Out = O
      override def unapply(t: Re): Option[O] = tag.unapply(aux.project(t)).map(v => gen.to(v).head)
    }
  }

  object RecursiveExtractor {
    def apply[Re, T[_] <: F[_], F[_]](implicit aux: Recursive.Aux[Re, F], func: Functor[F]) =
      new RecursiveExtractor[Re, T, F]
  }
}

object Main {
  import UnFixable._

  sealed trait Expr[A] {
    def name = toString
  }

  final case class Add[A](l: A, r: A) extends Expr[A]
  final case class Mul[A](l: A, r: A) extends Expr[A]
  final case class Pow[A](l: A, exp: Int) extends Expr[A]
  final case class Lit[A](v: Int) extends Expr[A]


  object Expr {
    implicit val functor = new Functor[Expr] {
      override def map[A, B](fa: Expr[A])(f: (A) => B): Expr[B] = fa match {
        case Add(l, r) => Add(f(l), f(r))
        case Mul(l, r) => Mul(f(l), f(r))
        case Pow(l, expr) => Pow(f(l), expr)
        case Lit(v) => Lit(v)
      }
    }
  }

  type ExprF = Fix[Expr]

  val AddF = RecursiveExtractor[ExprF, Add, Expr].tuple
  val MulF = RecursiveExtractor[ExprF, Mul, Expr].tuple
  val PowF = RecursiveExtractor[ExprF, Pow, Expr].tuple
  val LitF = RecursiveExtractor[ExprF, Lit, Expr].single[Int]

  def main(args: Array[String]): Unit = {
    val v1: ExprF = Fix(Add(Fix(Lit(1)), Fix(Lit(2))))
    val v2: ExprF = Fix(Mul(v1, v1))
    val v3: ExprF = Fix(Pow(v2, 2))

    v3 match {
      case PowF(MulF(AddF(LitF(a), LitF(b)), AddF(LitF(c), LitF(d))), exp) =>
        println(Math.pow((a + b) * (c + d), exp))
    }
  }
}
