package edu.umass.cs.iesl.watr
package watrcolors

import cats._
import cats.data._

package object server {
  implicit final class OptionTSyntax[F[_], A](val o: OptionT[F, A])   {
    def getOrRaise(e: Throwable)(implicit F: MonadError[F, Throwable]): F[A] =
      o.getOrElseF(F.raiseError(e))
  }
}
