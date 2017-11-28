package edu.umass.cs.iesl.watr
package watrcolors
package services

import java.util.UUID
import cats.effect.{Effect, IO}
import org.http4s.{HttpService, Response, Status}
import org.http4s.dsl.Http4sDsl
import models._
import tsec.passwordhashers._
import tsec.passwordhashers.imports._
import cats.syntax.all._
import tsec.common._
import tsec.authentication._
import persistence.{PasswordStore, UserStore}
import tsec.cipher.symmetric.imports.AES128

import models.users._
import models.formdata._
import LoginForm.LoginError
import SignupForm.SignupError
import TypeTags._

case class UserAuthenticationService(
  userStore: UserStore,
  authStore: PasswordStore,
  authenticator: EncryptedCookieAuthenticator[IO, Int, User, AES128]
)(implicit F: Effect[IO])
    extends Http4sDsl[IO] {

  private def checkOrRaise(rawFromLogin: String, hashed: SCrypt): IO[Unit] =
    if (rawFromLogin.base64Bytes.toAsciiString.checkWithHash(hashed))
      IO.unit
    else
      IO.raiseError[Unit](LoginError)

  val signupRoute: HttpService[IO] = HttpService[IO] {
    case request @ POST -> Root / "api" / "signup" =>
      val response = for {
        signup   <- request.as[SignupForm]
        exists   <- userStore.exists(EmailAddr(signup.email)).fold(())(_ => throw SignupError)
        password <- F.pure(signup.password.base64Bytes.toAsciiString.hashPassword[SCrypt])
        newUser = User(UserID(0), EmailAddr(signup.email), Username(signup.username))
        _      <- userStore.put(newUser)
        _      <- authStore.put(AuthInfo(UUID.randomUUID(), newUser.id, password))
        cookie <- authenticator.create(newUser.id.unwrap).getOrRaise(LoginError)
        o      <- Ok("Successfully signed up!")
      } yield authenticator.embed(o, cookie)

      response
        .handleError { _ =>
          Response(Status.BadRequest)
        }
  }

  val loginRoute: HttpService[IO] = HttpService[IO] {
    case request @ POST -> Root / "api" / "login" =>
      (for {
        login    <- request.as[LoginForm]
        user     <- userStore.exists(EmailAddr(login.email)).getOrRaise(LoginError)
        authInfo <- authStore.get(user.id.unwrap).getOrRaise(LoginError)
        _        <- checkOrRaise(login.password, authInfo.password)
        cookie   <- authenticator.create(user.id.unwrap).getOrRaise(LoginError)
        o        <- Ok()
      } yield authenticator.embed(o, cookie))
        .handleError { _ =>
          Response(Status.BadRequest)
        }
  }

}
