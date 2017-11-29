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
    case request @ POST -> Root / "signup" =>
      println(s"signup: ${request.toString()}")
      // request.decode[SignupForm]
      val response = for {
        // signup    <- request.as[SignupForm]
        signup    <- request.attemptAs[SignupForm].fold(
          decodeFailure => {println(s"decodeFailure: ${decodeFailure}"); throw SignupError},
          signupForm => { signupForm }
        )

        _          = println(s"signup; $signup")
        exists    <- userStore.exists(EmailAddr(signup.email)).fold(())(_ => throw SignupError)
        _          = println(s"exists; $exists")
        password  <- F.pure(signup.password.base64Bytes.toAsciiString.hashPassword[SCrypt])
        _          = println(s"password; $password")
        newUser   <- userStore.put(User(UserID(0), EmailAddr(signup.email)))
        _          = println(s"newUser; $newUser")
        _         <- authStore.put(AuthInfo(newUser.id, Username(signup.username), password))
        cookie    <- authenticator.create(newUser.id.unwrap).getOrRaise(LoginError)
        _          = println(s"cookie; $cookie")
        response  <- Ok("Successfully signed up!")
      } yield authenticator.embed(response, cookie)

      Status.Redirection
      response
        .handleError { _ => Response(Status.BadRequest) }
  }

  val loginRoute: HttpService[IO] = HttpService[IO] {
    case request @ POST -> Root / "login" =>
      println(s"login")
      val response = for {
        login    <- request.as[LoginForm]
        user     <- userStore.exists(EmailAddr(login.email)).getOrRaise(LoginError)
        authInfo <- authStore.get(user.id.unwrap).getOrRaise(LoginError)
        _        <- checkOrRaise(login.password, authInfo.password)
        cookie   <- authenticator.create(user.id.unwrap).getOrRaise(LoginError)
        response <- Ok()
      } yield authenticator.embed(response, cookie)

      response
        .handleError { _ => Response(Status.BadRequest) }
  }

}
