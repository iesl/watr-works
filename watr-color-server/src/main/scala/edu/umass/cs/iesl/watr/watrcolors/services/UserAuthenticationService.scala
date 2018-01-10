package edu.umass.cs.iesl.watr
package watrcolors
package services

import cats.effect.IO
import cats.syntax.all._
// import org.http4s.UrlForm
import org.http4s.headers.Location
import org.http4s._
// import org.http4s.{
//   HttpService,
//   Response,
//   Status
// }
import tsec.passwordhashers._
import tsec.passwordhashers.imports._
import tsec.authentication._

import models.users._
import models.formdata._
import LoginForm.LoginError
import SignupForm.SignupError
import TypeTags._

import _root_.io.circe, circe._
import circe.literal._
import org.http4s.circe._

trait UserAuthenticationServices extends AuthenticatedService {

  def userAuthenticationServices = signupRoute <+> loginRoute <+> authedUserRoutes

  private def checkOrRaise(rawFromLogin: String, hashed: PasswordHash[SCrypt]): IO[Unit] = {
    SCrypt.checkpw[IO](rawFromLogin, hashed)
      .map { valid => if (valid) () else IO.raiseError[Unit](LoginError) }
  }



  def userInfoResponse(user: User, authInfo: AuthInfo): Json = {
    json""" { "email": ${user.email}, "username": ${authInfo.username} } """
  }


  val signupRoute: HttpService[IO] = HttpService[IO] {
    case request @ POST -> Root / "signup" =>
      val hdrs = request.headers.toList.map(h => s"${h.name}: ${h.value}").mkString("\n")
      println(s"signup: ${hdrs}")


      val resp = request.decode[UrlForm]{ data =>
        println(s"signup data ${data}")
        val signup = SignupForm(
          data.values("email").head,
          data.values("username").head,
          data.values("password").head
        )
        for {
          // signup    <- decodeOrErr[SignupForm](request)
          exists    <- userStore.exists(EmailAddr(signup.email)).fold(true)(_ => throw SignupError)
          _         <- IO(println(s"exists; $exists"))
          // password  <- IO(signup.password.hashPassword[SCrypt])
          password  <- SCrypt.hashpw[IO](signup.password)
          _         <- IO(println(s"password; $password"))
          newUser   <- userStore.put(User(UserID(0), EmailAddr(signup.email)))
          authInfo  <- authStore.put(AuthInfo(newUser.id, Username(signup.username), password))
          _         <- IO( println(s"newUser; $newUser") )
          cookie    <- authenticator.create(newUser.id.unwrap).getOrRaise(LoginError)
          _         <- IO( println(s"cookie; $cookie") )
          // response  <- Ok(userInfoResponse(newUser, authInfo))
          response  <- TemporaryRedirect(Location(uri("/")))
        } yield authenticator.embed(response, cookie)
      }

      // resp.handleError { _ => Response(Status.BadRequest) }
      orErrorJson(resp)
  }

  val loginRoute: HttpService[IO] = HttpService[IO] {
    case request @ POST -> Root / "login" =>
      println(s"login")
      val resp = request.decode[UrlForm]{ data =>
        println(s"login: data=${data.values}")
        val login = LoginForm(
          data.values("email").head,
          data.values("password").head
        )
        for {
          // login       <- decodeOrErr[LoginForm](request)
          _           <- IO( println(s"login; $login"))
          user        <- userStore.getByEmail(EmailAddr(login.email)).getOrRaise(LoginError)
          _           <- IO(   println(s"user; $user"))
          authInfo    <- authStore.get(user.id.unwrap).getOrRaise(LoginError)
          _           <- IO(    println(s"authInfo; $authInfo"))
          _           <- checkOrRaise(login.password, authInfo.password)
          cookie      <- authenticator.create(user.id.unwrap).getOrRaise(LoginError)
          _           <- IO(    println(s"cookie; $cookie"))
          response    <- TemporaryRedirect(Location(uri("/")))
        } yield authenticator.embed(response, cookie)
      }

      // resp.handleError { _ => Response(Status.BadRequest) }
      orErrorJson(resp)
  }





  val authedUserRoutes = Auth {
    case GET -> Root / "status" asAuthed user =>

      val resp = for {
        authInfo <- authStore.get(user.id.unwrap).getOrRaise(LoginError)
        response  <- Ok(userInfoResponse(user, authInfo))
      } yield response

      orErrorJson(resp)

    case POST -> Root / "status" asAuthed user =>

      val resp = for {
        authInfo <- authStore.get(user.id.unwrap).getOrRaise(LoginError)
        response  <- Ok(userInfoResponse(user, authInfo))
      } yield response

      orErrorJson(resp)

    case r @ GET -> Root / "logout" asAuthed user =>
      // val request: SecuredRequest[IO, User, AuthEncryptedCookie[AES128, Int]] = r

      val resp = for {
        deadCookie   <- authenticator.discard(r.authenticator).getOrRaise(LoginError)
        response     <- Ok(json""" {} """)
      } yield {
        response.removeCookie(deadCookie.toCookie)
      }

      orErrorJson(resp)

  }

}
