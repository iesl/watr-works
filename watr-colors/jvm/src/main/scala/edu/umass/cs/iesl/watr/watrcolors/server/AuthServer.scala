package edu.umass.cs.iesl.watr
package watrcolors
package server

import corpora._
import docstore._

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

case class User(id: Long, name: String)

class AuthServer(
  reflowDB: TextReflowDB,
  corpus: Corpus,
  url: String,
  port: Int
) {
  val key = PrivateKey(scala.io.Codec.toUTF8(scala.util.Random.alphanumeric.take(20).mkString("")))

  val crypto = CryptoBits(key)

  val clock = Clock.systemUTC

  def retrieveUser: Service[Long, User] = Kleisli(id => Task.delay(???))


  val authUser: Service[Request, String \/ User] = Kleisli({ request =>
    val message = for {
      header <- request.headers.get(Authorization).toRightDisjunction("Couldn't find an Authorization header")
      token <- crypto.validateSignedToken(header.value).toRightDisjunction("Cookie invalid")
      message <- \/.fromTryCatchNonFatal(token.toLong).leftMap(_.toString)
    } yield message
    message.traverse(retrieveUser)
  })

  // type AuthedService[T] = Service[AuthedRequest[T], Response]
  // case class AuthedRequest[A](authInfo: A, req: Request)

  // val onFailure: AuthedService[String] = Kleisli(req => Task.delay(Forbidden(req.authInfo)))
  val onFailure: AuthedService[String] = Kleisli(req => Forbidden(req.authInfo))

  // def apply[Err, T](authUser: Service[Request, Err \/ T], onFailure: Kleisli[Task, AuthedRequest[Err], Response]): AuthMiddleware[T] = { service =>
  val middleware = AuthMiddleware(authUser, onFailure)

  val authedService: AuthedService[User] =
    AuthedService {
      case GET -> Root / "welcome" as user => Ok(s"Welcome, ${user.name}")
    }


  val service: HttpService = middleware(authedService)


  def verifyLogin(request: Request): Task[String \/ User] = ??? // gotta figure out how to do the form

  val logIn: Service[Request, Response] = Kleisli({ request =>
    verifyLogin(request: Request).flatMap(_ match {
      case -\/(error) =>
        Forbidden(error)
      case \/-(user) => {
        val message = crypto.signToken(user.id.toString, clock.millis.toString)
        Ok("Logged in!").addCookie(Cookie("authcookie", message))
      }
    })
  })



}
