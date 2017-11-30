package edu.umass.cs.iesl.watr
package watrcolors
package services

import cats.effect.{Effect, IO}
import org.http4s.{
  HttpService,
  Response,
  Status
}
import org.http4s.dsl.Http4sDsl
import tsec.passwordhashers._
import tsec.passwordhashers.imports._
import cats.syntax.all._
// import tsec.common._
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
    if (rawFromLogin.checkWithHash(hashed)) IO.unit
    else IO.raiseError[Unit](LoginError)

  // val forbidOnFailure: AuthedService[String, IO] = AuthedService.lift(req => Forbidden(req.authInfo))
  val Auth = SecuredRequestHandler(authenticator)


  val signupRoute: HttpService[IO] = HttpService[IO] {
    case request @ POST -> Root / "signup" =>
      println(s"signup: ${request.toString()}")

      val response = for {

        signup    <- request.attemptAs[SignupForm].fold(
          decodeFailure => {println(s"decodeFailure: ${decodeFailure}"); throw SignupError},
          signupForm => { signupForm }
        )

        // _         <- F.pure( println(s"signup; $signup"))
        exists    <- userStore.exists(EmailAddr(signup.email)).fold(())(_ => throw SignupError)
        // _         <- F.pure(println(s"exists; $exists"))
        password  <- F.pure(signup.password.hashPassword[SCrypt])
        // _         <- F.pure(println(s"password; $password"))
        newUser   <- userStore.put(User(UserID(0), EmailAddr(signup.email)))
        // _         <- F.pure( println(s"newUser; $newUser") )
        _         <- authStore.put(AuthInfo(newUser.id, Username(signup.username), password))
        cookie    <- authenticator.create(newUser.id.unwrap).getOrRaise(LoginError)
        // _         <- F.pure( println(s"cookie; $cookie") )
        response  <- Ok("Successfully signed up!")
      } yield authenticator.embed(response, cookie)

      response
        .handleError { _ => Response(Status.BadRequest) }
  }

  val loginRoute: HttpService[IO] = HttpService[IO] {
    case request @ POST -> Root / "login" =>
      println(s"login")
      val response = for {
        login    <- request.as[LoginForm]
        // _         = println(s"login; $login")
        user     <- userStore.getByEmail(EmailAddr(login.email)).getOrRaise(LoginError)
        // _         = println(s"user; $user")
        authInfo <- authStore.get(user.id.unwrap).getOrRaise(LoginError)
        // _         = println(s"authInfo; $authInfo")
        _        <- checkOrRaise(login.password, authInfo.password)
        cookie   <- authenticator.create(user.id.unwrap).getOrRaise(LoginError)
        // _         = println(s"cookie; $cookie")
        response <- Ok()
      } yield authenticator.embed(response, cookie)

      response
        .handleError { _ => Response(Status.BadRequest) }
  }



  val authedUserRoutes = Auth {
    case GET -> Root / "status" asAuthed user =>
      Ok(s"User ${user.email} is logged in.")

    case r @ GET -> Root / "logout" asAuthed user =>
      // val request: SecuredRequest[IO, User, AuthEncryptedCookie[AES128, Int]] = r

      val response = for {
        deadCookie   <- authenticator.discard(r.authenticator).getOrRaise(LoginError)
        response     <- Ok()
      } yield authenticator.embed(response, deadCookie)

      response
        .handleError { _ => Response(Status.BadRequest) }
  }

}
