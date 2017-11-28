package edu.umass.cs.iesl.watr
package watrcolors

// import java.util.UUID

import cats.MonadError
import cats.data.OptionT
// import models.User
// import tsec.authentication.{AuthEncryptedCookie, RequestAuthenticator}
// import tsec.cipher.symmetric.imports.AES128

package object services {

  implicit final class OptionTSyntax[F[_], A](val o: OptionT[F, A]) extends AnyVal {
    def getOrRaise(e: Throwable)(implicit F: MonadError[F, Throwable]): F[A] =
      o.getOrElseF(F.raiseError(e))
  }

  // type Authenticator[F[_]] = RequestAuthenticator[F, AES128, UUID, User, AuthEncryptedCookie[?, UUID]]

}
