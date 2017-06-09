package edu.umass.cs.iesl.watr
package watrcolors
package server

import scalaz.Kleisli
import scalaz.{
  \/, -\/, \/-
}
import scalaz.concurrent.Task

import org.http4s._
import org.http4s.dsl._
import org.http4s.server._
// import TypeTags._
import workflow.UserbaseApi



import org.reactormonk.{CryptoBits, PrivateKey}
import java.time._


case class UserData(
  id: Int@@UserID,
  emailAddr: String@@EmailAddr,
  pass: String,
  session: String
)


trait AuthServer {
  def userbaseApi: UserbaseApi

  val key = PrivateKey(scala.io.Codec.toUTF8(scala.util.Random.alphanumeric.take(20).mkString("")))

  val crypto = CryptoBits(key)

  val clock = Clock.systemUTC

  def retrieveUser: Service[String, UserData] = Kleisli(emailAddr => Task.delay(
    (for {
      userId <- userbaseApi.getUserByEmail(emailAddr)
      person <- userbaseApi.getUser(userId)
    } yield {
      UserData(person.prKey, person.email, "", "")
    }).get
  ))

  val authUser: Service[Request, String \/ UserData] = Kleisli({ request =>
    val message = for {
      header <- headers.Cookie.from(request.headers).toRightDisjunction("Cookie parsing error")
      cookie <- header.values.list.find(_.name == "authcookie").toRightDisjunction("Couldn't find the authcookie")
      token     <- crypto.validateSignedToken(cookie.content).toRightDisjunction("Cookie invalid")
      message   <- \/.fromTryCatchNonFatal(token).leftMap(_.toString)
    } yield message
    message.traverse(retrieveUser)
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

  def verifyLogin(request: Request): Task[String \/ UserData] =  {

    request.as[UrlForm].map{ formData =>
      println(s"verifyLogin: ${formData}")
      val maybeName = for {
        emailAddr <- formData.get("username")
        userId <- userbaseApi.getUserByEmail(emailAddr) orElse {
          Some(userbaseApi.addUser(emailAddr))
        }
        person <- userbaseApi.getUser(userId)
      } yield {
        \/-{ UserData(person.prKey, person.email, "", "") }
      }
      maybeName.headOption getOrElse {
        -\/{"Invalid login form"}
      }
    }


  }

  val logInService: Service[Request, Response] = Kleisli({ request =>
    verifyLogin(request).flatMap(_ match {
      case -\/(error) =>
        Forbidden(error)
      case \/-(user) => {
        val message = crypto.signToken(user.emailAddr.unwrap, clock.millis.toString)

        SeeOther(uri("/"))
          .addCookie(Cookie("authcookie", message, path=Some("/")))
      }
    })
  })

}
