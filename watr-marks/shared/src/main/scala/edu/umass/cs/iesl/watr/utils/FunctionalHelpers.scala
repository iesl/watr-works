package edu.umass.cs.iesl.watr
package utils



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

}
