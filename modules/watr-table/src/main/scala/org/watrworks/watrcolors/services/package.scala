package org.watrworks
package watrcolors

import cats.MonadError
import cats.data.{OptionT}

package object services {
  implicit final class OptionTSyntax[F[_], A](val o: OptionT[F, A])   {
    def getOrRaise(e: Throwable)(implicit F: MonadError[F, Throwable]): F[A] =
      o.getOrElseF(F.raiseError(e))
  }
}
