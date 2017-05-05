package edu.umass.cs.iesl.watr
package watrcolors
package server


import scalaz._
// import Scalaz._
import scalaz.concurrent.Task
// import scalaz.syntax.std.option._
// import scalaz.syntax.all._

import org.http4s._
import org.http4s.dsl._
import org.http4s.server._
// import org.http4s.util.string._


import org.reactormonk.{CryptoBits, PrivateKey}
import java.time._

import org.http4s.headers.Authorization


case class UserData(username: String, pass: String, session: String)


trait AuthServer {

  val key = PrivateKey(scala.io.Codec.toUTF8(scala.util.Random.alphanumeric.take(20).mkString("")))

  val crypto = CryptoBits(key)

  val clock = Clock.systemUTC

  def retrieveUser: Service[Long, UserData] = Kleisli(id => Task.delay(???))

  val authUser: Service[Request, String \/ UserData] = Kleisli({ request =>
    val message = for {
      header    <- request.headers.get(Authorization).toRightDisjunction("Couldn't find an Authorization header")
      token     <- crypto.validateSignedToken(header.value).toRightDisjunction("Cookie invalid")
      message   <- \/.fromTryCatchNonFatal(token.toLong).leftMap(_.toString)
    } yield message
    message.traverse(retrieveUser)
  })


  val onFailure: AuthedService[String] = Kleisli(req => Forbidden(req.authInfo))

  val authedService: AuthedService[UserData] = AuthedService {
    case GET -> Root / "status" as user =>
      Ok(s"User ${user.username} is logged in.")

    case GET -> Root / "logout" as user =>
      Ok(s"Logged out ${user.username}")
        .removeCookie("authcookie")
  }

  val authMiddleware = AuthMiddleware(authUser, onFailure)

  val loginHttpMiddleware: HttpService = authMiddleware(authedService)


  def verifyLogin(request: Request): Task[String \/ UserData] = ??? // gotta figure out how to do the form

  val logInService: Service[Request, Response] = Kleisli({ request =>
    verifyLogin(request).flatMap(_ match {
      case -\/(error) =>
        Forbidden(error)
      case \/-(user) => {
        val message = crypto.signToken(user.username, clock.millis.toString)

        Ok("Logged in!")
          .addCookie(Cookie("authcookie", message))


      }
    })
  })



}
