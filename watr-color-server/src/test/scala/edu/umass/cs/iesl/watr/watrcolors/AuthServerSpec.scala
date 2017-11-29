package edu.umass.cs.iesl.watr
package watrcolors
package server


import org.http4s._
import org.http4s.server.middleware.authentication._
import org.http4s.dsl._
import org.http4s.headers._
import org.http4s.parser.HttpHeaderParser
import fs2._

class AuthenticationSpec extends Http4sSpec {

  // def nukeService(launchTheNukes: => Unit) = AuthedService[String, IO] {
  //   case GET -> Root / "launch-the-nukes" as user =>
  //     for {
  //       _ <- Task.delay(launchTheNukes)
  //       r <- Response(Gone).withBody(s"Oops, ${user} launched the nukes.")
  //     } yield r
  // }

  // val realm = "Test Realm"
  // val username = "Test User"
  // val password = "Test Password"

  // def authStore(u: String) = Task.now {
  //   if (u == username) Some(u -> password)
  //   else None
  // }

  // def validatePassword(creds: BasicCredentials) = Task.now {
  //   if (creds.username == username && creds.password == password) Some(creds.username)
  //   else None
  // }

  // val service = AuthedService[String, IO] {
  //   case GET -> Root as user => Ok(user)
  //   case req as _ => Response.notFound(req)
  // }

  // behavior of "Failure to authenticate"

  // it should "not run unauthorized routes" in {
  //   val req = Request(uri = Uri(path = "/launch-the-nukes"))
  //   var isNuked = false
  //   val authedValidateNukeService = BasicAuth(realm, validatePassword _)(nukeService { isNuked = true })
  //   val res = authedValidateNukeService.orNotFound(req).unsafeRunSync()
  //   isNuked shouldEqual false
  //   val response = res.right.get

  //   response.status shouldEqual (Unauthorized)
  // }

  // behavior of "BasicAuthentication"

  // val basicAuthedService = BasicAuth(realm, validatePassword _)(service)

  // it should "Respond to a request without authentication with 401" in {
  //   val req = Request(uri = Uri(path = "/"))
  //   val res = basicAuthedService.orNotFound(req).unsafeRunSync

  //   val response = res.right.get

  //   response.status shouldEqual (Unauthorized)
  //   response.headers.get(`WWW-Authenticate`).map(_.value) shouldEqual (Some(Challenge("Basic", realm, Nil.toMap).toString))
  // }

  // it should "Respond to a request with unknown username with 401" in {
  //   val req = Request(uri = Uri(path = "/"), headers = Headers(Authorization(BasicCredentials("Wrong User", password))))
  //   val res = basicAuthedService.orNotFound(req).unsafeRunSync

  //   val response = res.right.get

  //   response.status shouldEqual (Unauthorized)
  //   response.headers.get(`WWW-Authenticate`).map(_.value) shouldEqual (Some(Challenge("Basic", realm, Nil.toMap).toString))
  // }

  // it should "Respond to a request with wrong password with 401" in {
  //   val req = Request(uri = Uri(path = "/"), headers = Headers(Authorization(BasicCredentials(username, "Wrong Password"))))
  //   val res = basicAuthedService.orNotFound(req).unsafeRunSync

  //   val response = res.right.get

  //   response.status shouldEqual (Unauthorized)
  //   response.headers.get(`WWW-Authenticate`).map(_.value) shouldEqual (Some(Challenge("Basic", realm, Nil.toMap).toString))
  // }

  // it should "Respond to a request with correct credentials" in {
  //   val req = Request(uri = Uri(path = "/"), headers = Headers(Authorization(BasicCredentials(username, password))))
  //   val res = basicAuthedService.orNotFound(req).unsafeRunSync

  //   val response = res.right.get

  //   response.status shouldEqual (Ok)
  // }

  // private def parse(value: String) = HttpHeaderParser.WWW_AUTHENTICATE(value).fold(err => sys.error(s"Couldn't parse: $value"), identity)

  // behavior of "DigestAuthentication"

  // it should "Respond to a request without authentication with 401" in {
  //   val authedService = DigestAuth(realm, authStore)(service)
  //   val req = Request(uri = Uri(path = "/"))
  //   val res = authedService.orNotFound(req).unsafeRunSync

  //   val response = res.right.get
  //   response.status shouldEqual (Status.Unauthorized)
  //   val opt = response.headers.get(`WWW-Authenticate`).map(_.value)
  //   opt.isDefined shouldEqual true
  //   val challenge = parse(opt.get).values.head
  //   (challenge match {
  //     case Challenge("Digest", realm, _) => true
  //     case _ => false
  //   }) shouldEqual true

  // }


}
