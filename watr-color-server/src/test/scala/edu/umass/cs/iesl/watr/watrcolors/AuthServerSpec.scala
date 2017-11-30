package edu.umass.cs.iesl.watr
package watrcolors
package services


import org.http4s._
// import org.http4s.syntax._
import org.http4s.syntax.string._
import persistence._

import _root_.io.circe
import circe.literal._
import models.users._
import cats.effect._
import cats.data._
import corpora._
import tsec.authentication._

import tsec.cipher.symmetric.imports.AES128
import scala.concurrent.duration._

import org.scalatest._

class AuthenticationSpec extends DatabaseFreeSpec  {


  val authenticatorSettings = TSecCookieSettings("tsec-auth", secure = false, httpOnly = true,
    expiryDuration = 1.hour, //   scala.concurrent.duration.FiniteDuration,
    maxIdle = Some(1.hour) //  Option[scala.concurrent.duration.FiniteDuration]
  )

  def corpusAccessApi: CorpusAccessApi = CorpusAccessApi(reflowDB, null)

  def userAuthService(): UserAuthenticationService = {
    val io = for {
      userStore     <- UserStore.fromDb(corpusAccessApi.corpusAccessDB)
      tokenStore    <- TokenStore.fromDb(corpusAccessApi.corpusAccessDB)
      passwordStore <- PasswordStore.fromDb(corpusAccessApi.corpusAccessDB)
      symmetricKey  <- AES128.generateLift[IO]
    } yield {

      val authenticator = EncryptedCookieAuthenticator.withBackingStore[IO, Int, User, AES128](
        authenticatorSettings,
        tokenStore,
        userStore,
        symmetricKey
      )

      UserAuthenticationService(userStore, passwordStore, authenticator)
    }

    io.unsafeRunSync()
  }


  def loginFormJson(name: String) = { json""" { "email": ${name+"@x.com"}, "password": ${name+"-psswd"} } """ }
  def loginForm(name: String) = { loginFormJson(name).noSpaces }
  def signupForm(name: String) = {  loginFormJson(name).deepMerge(json""" { "username": ${name} } """).noSpaces }

  def doPost(url: String) = Request[IO](Method.POST, uri = Uri(path = url))
  def doGet(url: String) = Request[IO](Method.GET, uri = Uri(path = url))

  def signupReq(name: String): Request[IO] = doPost("/signup").withBody(signupForm(name)).unsafeRunSync()
  def loginReq(name: String): Request[IO] = doPost("/login").withBody(loginForm(name)).unsafeRunSync()

  def logoutReq(): Request[IO] = doGet("/logout")

  def checkResponse(r: OptionT[IO, Response[IO]], f: Response[IO] => Unit) = {
    r.fold(fail("no response"))(f(_))
      .unsafeRunSync()
  }

  "Behavior of Authorization" - {

    "When Not Logged In" - {

      "Should redirect to login/signup" in {

      }

      "When Not Registered" - {

        val authService = userAuthService()

        // info("info is recorded")
        // markup("markup is *also* recorded")
        // note("notes are sent immediately")
        // alert("alerts are also sent immediately")

        "Should Permit Signup" in new CleanDocstore {

          val req = signupReq("Morgan")

          checkResponse(authService.signupRoute(req), {resp =>
            resp.status shouldEqual Status.Ok

            val cookieHeader = resp.headers.get("Set-Cookie".ci)
            assert(cookieHeader.isDefined)
          })

        }

        "Should Block Unregistered Login" in {
          val req = loginReq("Oliver")

          checkResponse(authService.loginRoute(req), {resp =>
            resp.status shouldEqual Status.BadRequest
          })

        }
      }

      // import org.http4s.headers.{Authorization, `Content-Type`, `X-Forwarded-For`}
      // import org.http4s.headers

      "When Registered" - {

        val authService = userAuthService()
        // var authCookie: Option[Cookie] = None
        var authCookieHdr: Option[Header] = None
        var authCookieVal: String = ""

        "After Signup" in new CleanDocstore {

          checkResponse(authService.signupRoute(signupReq("Morgan")), { resp =>
            info("should be logged in")
            authCookieHdr = resp.headers.get("Set-Cookie".ci)
            authCookieVal = resp.headers.get("Set-Cookie".ci).get.value
            info(s"authCookieHdr ${authCookieHdr}")
            assert(resp.headers.get("Set-Cookie".ci).isDefined)
          })
        }

        "Should Block Duplicate Signup" in {
          checkResponse(authService.signupRoute(signupReq("Morgan")), { resp =>
            resp.status shouldEqual Status.BadRequest
          })
        }

        "After Logout" - {
          logoutReq().addCookie()()
          checkResponse(authService.authedUserRoutes(logoutReq()), { resp =>
            info(s"After Logout: ${resp}")
          })
        }

        "Should Permit Login" in {
          checkResponse(authService.loginRoute(loginReq("Morgan")), { resp =>

            println(s"login resp: ${resp}")

            // resp.status shouldEqual Status.Ok

          })
        }
      }
    }

    "When Logged In" - {

    }


  }





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
