package edu.umass.cs.iesl.watr
package watrcolors
package server


import org.http4s._
import org.http4s.dsl._
import org.http4s.server._

import fs2._
import fs2.interop.cats._
import cats.implicits._, cats.data._

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

  // def retrieveUser: Service[String, Either[String, UserData]] = Kleisli(emailAddr => Task.delay{
  def retrieveUser: Service[String, UserData] = Kleisli(emailAddr => Task.delay{
    val userOrErr = for {
      userId <- userbaseApi.getUserByEmail(emailAddr).toRight(left="User not found")
      person <- userbaseApi.getUser(userId).toRight(left="User id not found")
    } yield {
      UserData(person.prKey, person.email, "", "")
    }
    userOrErr.right.get
  })

  val authUser: Service[Request, Either[String, UserData]] = Kleisli({ request =>
    val message = for {
      header    <- headers.Cookie.from(request.headers).toRight(left = "Cookie parsing error")
      cookie    <- header.values.toList.find(_.name == "authcookie").toRight(left = "Couldn't find the authcookie")
      token     <- crypto.validateSignedToken(cookie.content).toRight(left = "Cookie invalid")
      // message   <- Either.catchOnly[NumberFormatException](token).leftMap(_.toString)
    } yield token

    message.traverse(retrieveUser.run)
  })


  val forbidOnFailure: AuthedService[String] = Kleisli(req => Forbidden(req.authInfo))
  // def redirectOnFailure(urlstr: String): AuthedService[String] = Kleisli(req => SeeOther(Uri.fromString(urlstr)))

  val userStatusAndLogout: AuthedService[UserData] = AuthedService {
    case GET -> Root / "status" as user =>
      Ok(s"User ${user.emailAddr} is logged in.")

    case GET -> Root / "logout" as user =>
      TemporaryRedirect(uri("/"))
        .removeCookie("authcookie")
  }

  val authOrForbid = AuthMiddleware(authUser, forbidOnFailure)

  // val authStatusLogoutService: HttpService = authOrForbid(authedService)

  def verifyLogin(request: Request): Task[Either[String, UserData]] =  {

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

  val logInService: Service[Request, Response] = Kleisli({ request =>
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

}
