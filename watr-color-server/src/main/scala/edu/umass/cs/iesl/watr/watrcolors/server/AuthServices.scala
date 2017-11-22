package edu.umass.cs.iesl.watr
package watrcolors
package server


import org.http4s._
import org.http4s.dsl._
import org.http4s.server._

import org.http4s.dsl.io._
// import fs2._
// import fs2.interop.cats._
import cats.implicits._, cats.data._

import cats.effect._
import org.reactormonk.{CryptoBits, PrivateKey}
import java.time._

case class UserData(
  id: Int@@UserID,
  emailAddr: String@@EmailAddr,
  pass: String,
  session: String
)

trait UserAuthenticationServices extends ServiceCommons with WorkflowCodecs { self =>

  val key = PrivateKey(scala.io.Codec.toUTF8(scala.util.Random.alphanumeric.take(20).mkString("")))

  val crypto = CryptoBits(key)

  val clock = Clock.systemUTC

  // def retrieveUser: Service[String, Either[String, UserData]] = Kleisli(emailAddr => IO.delay{
  def retrieveUser: Service[IO, String, UserData] = Kleisli(emailAddr => IO{
    val userOrErr = for {
      userId <- userbaseApi.getUserByEmail(emailAddr).toRight(left="User not found")
      person <- userbaseApi.getUser(userId).toRight(left="User id not found")
    } yield {
      UserData(person.prKey, person.email, "", "")
    }
    userOrErr.right.get
  })

  val authUser: Service[IO, Request[IO], Either[String, UserData]] = Kleisli({ request =>
    val message = for {
      header    <- headers.Cookie.from(request.headers).toRight(left = "Cookie parsing error")
      cookie    <- header.values.toList.find(_.name == "authcookie").toRight(left = "Couldn't find the authcookie")
      token     <- crypto.validateSignedToken(cookie.content).toRight(left = "Cookie invalid")
      // message   <- Either.catchOnly[NumberFormatException](token).leftMap(_.toString)
    } yield token

    message.traverse(retrieveUser.run)
  })


  // val forbidOnFailure: AuthedService[String, IO] = Kleisli(req => Forbidden(req.authInfo))
  val forbidOnFailure: AuthedService[String, IO] = AuthedService.lift(req => Forbidden(req.authInfo))
  // def redirectOnFailure(urlstr: String): AuthedService[String, IO] = Kleisli(req => SeeOther(Uri.fromString(urlstr)))

  val userStatusAndLogout = AuthedService[UserData, IO] {
    case GET -> Root / "status" as user =>
      Ok(s"User ${user.emailAddr} is logged in.")

    case GET -> Root / "logout" as user =>
      TemporaryRedirect(uri("/"))
        .removeCookie("authcookie")
  }

  val authOrForbid = AuthMiddleware(authUser, forbidOnFailure)

  // val authStatusLogoutService: HttpService = authOrForbid(authedService)

  def verifyLogin(request: Request[IO]): IO[Either[String, UserData]] =  {

    request.as[UrlForm].map{ formData =>
      println(s"verifyLogin: ${formData}")
      val maybeName = for {
        emailAddr <- formData.get("username")
        userId <- userbaseApi.getUserByEmail(emailAddr) orElse {
          Some(userbaseApi.addUser(emailAddr))
        }
        person <- userbaseApi.getUser(userId)
      } yield {
        Right{ UserData(person.prKey, person.email, "", "") }
      }
      maybeName.headOption getOrElse {
        Left{ "Invalid login form" }
      }
    }
  }

  val login: Service[IO, Request[IO], Response[IO]] = Kleisli({ request =>
    verifyLogin(request).flatMap(_ match {
      case Left(error) =>
        Forbidden(error)
      case Right(user) => {
        val message = crypto.signToken(user.emailAddr.unwrap, clock.millis.toString)

        SeeOther(uri("/"))
          .addCookie(Cookie("authcookie", message, path=Some("/")))
      }
    })
  })

  val loginService : HttpService[IO] = HttpService {
    case req @ POST -> Root /  "login" =>
      println(s"POST: login")
      login.run(req)
  }
}


// case class UserLogin(u: String, p: String)

// class AdminUserHttpEndpoint[F[_] : IO](middleware: Middleware[F]) extends Http4sDsl[F] {

  // private val adminUser = UserLogin("gvolpe", "") // Hardcoded admin user

  // private def retrieveUser: Kleisli[F, String, UserLogin] =
  //   Kleisli(username => IO[F].delay(UserLogin(username, "")))

  // private val onFailure: AuthedService[String, F] = Kleisli(req => OptionT.liftF(Forbidden(req.authInfo)))

  // private val authUser: Kleisli[F, Request[F], Either[String, UserLogin]] = Kleisli({ request =>
  //   val message = for {
  //     header  <- headers.Cookie.from(request.headers).toRight("Cookie parsing error")
  //     cookie  <- header.values.toList.find(_.name == "authcookie").toRight("Couldn't find the authcookie")
  //     token   <- crypto.validateSignedToken(cookie.content).toRight("Cookie invalid")
  //     message <- Right(token)
  //   } yield message
  //   message.traverse(retrieveUser.run)
  // })

  // private val authMiddleware: AuthMiddleware[F, UserLogin] = AuthMiddleware(authUser, onFailure)

  // private val key     = PrivateKey(scala.io.Codec.toUTF8(scala.util.Random.alphanumeric.take(20).mkString("")))
  // private val crypto  = CryptoBits(key)
  // private val clock   = Clock.systemUTC

  // // TODO: Retrieve user from real admin db...
  // private def verifyLogin(userLogin: UserLogin): F[String Either UserLogin] =
  //   userLogin match {
  //     case user @ UserLogin(adminUser.username, _)  => IO[F].delay(Right(user))
  //     case _                                        => IO[F].delay(Left("User must be admin"))
  //   }

  // private def login: Kleisli[F, Request[F], Response[F]] = Kleisli({ request =>
  //   request.decode[UserLogin] { userLogin =>
  //     verifyLogin(userLogin).>>= {
  //       case Left(error) =>
  //         Forbidden(error)
  //       case Right(user) => {
  //         val message = crypto.signToken(user.username, clock.millis.toString)
  //         Ok("Logged in!").map(_.addCookie(Cookie("authcookie", message)))
  //       }
  //     }
  //   }
  // })

  // private val authedService: AuthedService[UserLogin, F] = AuthedService {
  //   case GET -> Root / "users" as _ =>
  //     middleware.all().handle
  //   case DELETE -> Root / "users" / id as _ =>
  //     middleware.delete(User.Id(id)).handle
  //   case POST -> Root / "users" / id / "block" as _ =>
  //     middleware.block(User.Id(id)).handle
  //   case POST -> Root /  "users" / id / "unblock" as _ =>
  //     middleware.unblock(User.Id(id)).handle
  // }

  // val loginService : HttpService[F] = HttpService[F] {
  //   case req @ POST -> Root / ApiVersion / "signin" =>
  //     login.run(req)
  // }

  // val service: HttpService[F] = authMiddleware(authedService)

// }
