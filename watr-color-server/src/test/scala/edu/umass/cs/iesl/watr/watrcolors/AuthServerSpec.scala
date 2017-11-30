package edu.umass.cs.iesl.watr
package watrcolors
package services


import org.http4s
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
// import org.http4s.headers.{Authorization, `Content-Type`, `X-Forwarded-For`}
// import org.http4s.headers
import scala.collection.JavaConverters._

import java.net.HttpCookie

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

  case class MockCookieJar(
    var cookies: List[HttpCookie] = List()
  ) {

    // var authCookieHdr: Option[Header] = None
    // var authCookie: List[HttpCookie] = List()
    // authCookie = authCookieHdr.toList.flatMap{ h =>
    //   HttpCookie.parse(h.value).asScala
    // }
    // authCookie.foreach({c =>
    //   println(s"> ${c.getName}, ${c.getValue}")
    //   println(s">== ${c} ")
    //   println()
    // })

    def httpCookies(): List[Cookie] = {
      cookies.map{httpCookie =>
        Cookie(
          httpCookie.getName,
          httpCookie.getValue,
          None
        )
      }
    }
  }


  def loginFormJson(name: String) = { json""" { "email": ${name+"@x.com"}, "password": ${name+"-psswd"} } """ }
  def loginForm(name: String) = { loginFormJson(name).noSpaces }
  def signupForm(name: String) = {  loginFormJson(name).deepMerge(json""" { "username": ${name} } """).noSpaces }

  def doPost(url: String)(implicit cj: MockCookieJar) = {
    addCookies(Request[IO](Method.POST, uri = Uri(path = url)), cj)
  }

  def doGet(url: String)(implicit cj: MockCookieJar) = {
    addCookies(Request[IO](Method.GET, uri = Uri(path = url)), cj)
  }

  def addCookies(req: Request[IO], cj: MockCookieJar): Request[IO] = {
    cj.httpCookies().foldLeft(req)({case (racc, celem) =>
      req.addCookie(celem.name, celem.content, celem.expires)
    })
  }

  def signupReq(name: String)(implicit cj: MockCookieJar): Request[IO] = doPost("/signup").withBody(signupForm(name)).unsafeRunSync()
  def loginReq(name: String)(implicit cj: MockCookieJar): Request[IO] = doPost("/login").withBody(loginForm(name)).unsafeRunSync()
  def logoutReq()(implicit cj: MockCookieJar): Request[IO] = doGet("/logout")

  def checkResponse(r: OptionT[IO, Response[IO]], checks: (Response[IO] => Unit)*)(implicit cj: MockCookieJar) = {
    r.fold(fail("no response")){ resp =>
      checks.foreach { check => check(resp) }
    }.unsafeRunSync()
  }

  def hasStatus(s: http4s.Status): Response[IO] => Unit =
    r => r.status shouldEqual s

  def hasHeader(hdr: String): Response[IO] => Unit =
    r => r.headers.get(hdr.ci).isDefined

  def tapWith(f: Response[IO] => Unit): Response[IO] => Unit = r => f(r)
  def tapWith(f: => Unit): Response[IO] => Unit = _ => f

  // doGet("").addCookie(name: String, content: String, expires: Option[HttpDate])
  "Behavior of Authorization" - {

    "When Not Registered" - {

      val authService = userAuthService()


      "Should Permit Signup" in new CleanDocstore {

        implicit val cookieJar  = MockCookieJar()

        val req = signupReq("Morgan")

        checkResponse(authService.signupRoute(req), {resp =>
          resp.status shouldEqual Status.Ok

          val cookieHeader = resp.headers.get("Set-Cookie".ci)
          assert(cookieHeader.isDefined)
        })

      }

      "Should Block Unregistered Login" in {
        implicit val cookieJar  = MockCookieJar()
        val req = loginReq("Oliver")

        checkResponse(authService.loginRoute(req), {resp =>
          resp.status shouldEqual Status.BadRequest
        })

      }
    }



    "When Registered" - {

      "When Not Logged In" - {

        "Should redirect to login/signup" in {

        }

        implicit val cookieJar  = MockCookieJar()

        val authService = userAuthService()

        "After Signup" in new CleanDocstore {

          checkResponse(authService.signupRoute(signupReq("Morgan")),
            hasHeader("Set-Cookie"),
            tapWith(info("should be logged in"))
          )

        }

        "Should Block Duplicate Signup" in {
          checkResponse(authService.signupRoute(signupReq("Morgan")),
            hasStatus(Status.BadRequest)
          )
        }

        "After Logout" - {
          checkResponse(authService.authedUserRoutes(logoutReq()), { resp =>
            info(s"After Logout: ${resp}")
          })
        }

        "Should Permit Login" in {

          checkResponse(authService.loginRoute(loginReq("Morgan")),
            hasStatus(Status.Ok)
          )
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
