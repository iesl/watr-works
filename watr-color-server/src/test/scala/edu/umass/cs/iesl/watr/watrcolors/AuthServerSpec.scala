package edu.umass.cs.iesl.watr
package watrcolors
package server


// import org.http4s._
// import org.http4s.server.middleware.authentication._
// import org.http4s.dsl._
// import org.http4s.headers._
// import org.http4s.parser.HttpHeaderParser
// import org.http4s.circe._
// import _root_.io.circe
// import circe._
// import corpora._
// import corpora.database._
// import workflow._


class AuthenticationSpec extends Http4sSpec {

  // def nukeService(launchTheNukes: => Unit) = AuthedService[String] {
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

  // val service = AuthedService[String] {
  //   case GET -> Root as user => Ok(user)
  //   case req as _ => Response.notFound(req)
  // }

  // behavior of "Failure to authenticate"

  // it should "not run unauthorized routes" in {
  //   val req = Request(uri = Uri(path = "/launch-the-nukes"))
  //   var isNuked = false
  //   val authedValidateNukeService = BasicAuth(realm, validatePassword _)(nukeService { isNuked = true })
  //   val res = authedValidateNukeService.orNotFound(req).unsafePerformSync
  //   isNuked shouldEqual false

  //   res.status shouldEqual (Unauthorized)
  // }

  // behavior of "BasicAuthentication"

  // val basicAuthedService = BasicAuth(realm, validatePassword _)(service)

  // it should "Respond to a request without authentication with 401" in {
  //   val req = Request(uri = Uri(path = "/"))
  //   val res = basicAuthedService.orNotFound(req).unsafePerformSync

  //   res.status shouldEqual (Unauthorized)
  //   res.headers.get(`WWW-Authenticate`).map(_.value) shouldEqual (Some(Challenge("Basic", realm, Nil.toMap).toString))
  // }

  // it should "Respond to a request with unknown username with 401" in {
  //   val req = Request(uri = Uri(path = "/"), headers = Headers(Authorization(BasicCredentials("Wrong User", password))))
  //   val res = basicAuthedService.orNotFound(req).unsafePerformSync

  //   res.status shouldEqual (Unauthorized)
  //   res.headers.get(`WWW-Authenticate`).map(_.value) shouldEqual (Some(Challenge("Basic", realm, Nil.toMap).toString))
  // }

  // it should "Respond to a request with wrong password with 401" in {
  //   val req = Request(uri = Uri(path = "/"), headers = Headers(Authorization(BasicCredentials(username, "Wrong Password"))))
  //   val res = basicAuthedService.orNotFound(req).unsafePerformSync

  //   res.status shouldEqual (Unauthorized)
  //   res.headers.get(`WWW-Authenticate`).map(_.value) shouldEqual (Some(Challenge("Basic", realm, Nil.toMap).toString))
  // }

  // it should "Respond to a request with correct credentials" in {
  //   val req = Request(uri = Uri(path = "/"), headers = Headers(Authorization(BasicCredentials(username, password))))
  //   val res = basicAuthedService.orNotFound(req).unsafePerformSync

  //   res.status shouldEqual (Ok)
  // }


  // private def parse(value: String) = HttpHeaderParser.WWW_AUTHENTICATE(value).fold(err => sys.error(s"Couldn't parse: $value"), identity)

  // behavior of "DigestAuthentication"

  // it should "Respond to a request without authentication with 401" in {
  //   val authedService = DigestAuth(realm, authStore)(service)
  //   val req = Request(uri = Uri(path = "/"))
  //   val res = authedService.orNotFound(req).unsafePerformSync

  //   res.status shouldEqual (Status.Unauthorized)
  //   val opt = res.headers.get(`WWW-Authenticate`).map(_.value)
  //   opt.isDefined shouldEqual true
  //   val challenge = parse(opt.get).values.head
  //   (challenge match {
  //     case Challenge("Digest", realm, _) => true
  //     case _ => false
  //   }) shouldEqual true

  // }


}
