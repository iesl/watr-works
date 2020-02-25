package edu.umass.cs.iesl.watr
package utils

import scala.{ collection => sc }
import sc.Seq

object FunctionalHelpers {
  import scalaz._


  object IO {

    def putStrLn[F[_]](s: String)(implicit F: Applicative[F]): F[Unit] =  {
      F.point { println(s) }
    }

    def putStr[F[_]](s: String)(implicit F: Applicative[F]): F[Unit] =  {
      F.point { print(s) }
    }
  }

  /** Converts a failable fold into a non-failable, by simply returning the
    * argument upon failure.
    */
  def orOriginal[A](f: A => Option[A]): A => A =
    expr => f(expr).getOrElse(expr)

  /** Converts a failable fold into a non-failable, by returning the default
    * upon failure.
    */
  def orDefault[A, B](default: B)(f: A => Option[B]): A => B =
    expr => f(expr).getOrElse(default)

  def liftSeq[A](a: A): Seq[A] = { Seq(a) }
  def liftOpt[A](a: A): Option[A] = { Option(a) }

  // On seq `as`, mark Right(a) iff f(a)==true else Left(a), then collect contiguous spans of Right/Left
  // into e.g., Right(Seq(a,...))
  def collectSpanEither[A](as: Seq[A], f: A => Boolean): Seq[Either[Seq[A], Seq[A]]] = {
    import SlicingAndDicing._

    val eithers = as.map{ a => if (f(a)) Right(a) else Left(a) }
    val groups = eithers.groupByPairs { case (a1, a2) =>
      a1.isLeft && a2.isLeft || a1.isRight && a2.isRight
    }
    groups.map{ group =>
      if (group.head.isRight) {
        Right(group.map(_.right.get))
      } else {
        Left(group.map(_.left.get))
      }
    }
  }




}
