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
}
