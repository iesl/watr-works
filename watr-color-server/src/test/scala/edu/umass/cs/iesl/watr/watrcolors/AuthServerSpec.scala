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
// import models.users._
import cats.effect._
import cats.data._
import corpora._
import tsec.authentication._

// import tsec.cipher.symmetric.imports.AES128
import scala.concurrent.duration._

import org.scalatest._

import java.net.HttpCookie

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class MockCookieJar {

  var responseCookies: List[Cookie] = Nil

  def httpCookiesToHttp4s(cs: List[HttpCookie]): List[Cookie] = {
    cs.map{httpCookie =>

      Cookie(
        httpCookie.getName,
        httpCookie.getValue,
        expires=Some(HttpDate.now),
        // maxAge=Some(httpCookie.getMaxAge),
        // domain=Some(httpCookie.getDomain),
        // path=Some(httpCookie.getPath),
        httpOnly=httpCookie.isHttpOnly(),
        secure=httpCookie.getSecure
      )
    }
  }

  def updateCookies(r: Response[IO]): Unit = {
    responseCookies = r.cookies

    // val setCookieHdrs = r.headers.get("Set-Cookie".ci)
    // val authCookie = setCookieHdrs.toList.flatMap{ h =>
    //   HttpCookie.parse(h.value).asScala
    // }
    // responseCookies = responseCookies ++ httpCookiesToHttp4s(authCookie)
  }

  def addCookies(req: Request[IO]): Request[IO] = {
    responseCookies.foldLeft(req)({case (racc, celem) =>
      req.addCookie(celem.name, celem.content, celem.expires)
    })
  }

  def debugPrint(hdr: String): String = {
    val cookieStr = responseCookies.map { c =>
      c.toString()
    }.mkString("{\n  ", "\n  ", "\n}")

    s"${hdr}: Cookie jar state==========\n" + cookieStr
  }

}


trait MockAuthentication {

  def loginFormJson(name: String) = { json""" { "email": ${name+"@x.com"}, "password": ${name+"-psswd"} } """ }
  def loginForm(name: String) = { loginFormJson(name).noSpaces }
  def signupForm(name: String) = {  loginFormJson(name).deepMerge(json""" { "username": ${name} } """).noSpaces }

  def doPost(url: String)(implicit cj: MockCookieJar) = {
    cj.addCookies(Request[IO](Method.POST, uri = Uri(path = url)))
  }

  def doGet(url: String)(implicit cj: MockCookieJar) = {
    cj.addCookies(Request[IO](Method.GET, uri = Uri(path = url)))
  }

  def signupReq(name: String)(implicit cj: MockCookieJar): Request[IO] = doPost("/signup").withBody(signupForm(name)).unsafeRunSync()
  def loginReq(name: String)(implicit cj: MockCookieJar): Request[IO] = doPost("/login").withBody(loginForm(name)).unsafeRunSync()
  def logoutReq()(implicit cj: MockCookieJar): Request[IO] = doGet("/logout")

  def checkResponse(r: OptionT[IO, Response[IO]], checks: (Response[IO] => Unit)*)(implicit cj: MockCookieJar) = {
    r.fold(sys.error("no response")){ resp =>
      checks.foreach { check => check(resp) }
      // Put any headers from response into cookie jar
      cj.updateCookies(resp)
    }.unsafeRunSync()
  }


  def hasHeader(hdr: String): Response[IO] => Unit =
    r => assert(r.headers.get(hdr.ci).isDefined)

  def tapWith(f: Response[IO] => Unit): Response[IO] => Unit = r => f(r)
  def tapWith(f: => Unit): Response[IO] => Unit = _ => f

}

class AuthenticationSpec extends DatabaseFreeSpec with MockAuthentication {


  val authenticatorSettings = TSecCookieSettings(
    "tsec-auth",
    secure = false,
    httpOnly = true,
    expiryDuration = 1.hour, //   scala.concurrent.duration.FiniteDuration,
    maxIdle = Some(1.hour) //  Option[scala.concurrent.duration.FiniteDuration]
  )


  def userAuthService() = new UserAuthenticationServices {

    def corpusAccessApi: CorpusAccessApi = CorpusAccessApi(reflowDB, null)
    lazy val corpusAccessDB  = corpusAccessApi.corpusAccessDB

    lazy val userStore = UserStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
    lazy val authStore = PasswordStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
    lazy val tokenStore = MemTokenStore.apply.unsafeRunSync()

  }


  def statusReq()(implicit cj: MockCookieJar): Request[IO] = doGet("/status")

  def hasStatus(s: http4s.Status): Response[IO] => Unit =
    r => r.status shouldEqual s




  ///////////////////////////////
  ///////////////////////////


  "Behavior of Authorization" - {

    "When Not Registered" - {

      val authService = userAuthService()


      "Should Permit Signup" in new EmptyDatabase {
        implicit val cookieJar  = new MockCookieJar()

        checkResponse(authService.signupRoute(signupReq("Morgan")),
          hasStatus(Status.TemporaryRedirect),
          hasHeader("Set-Cookie"))

        info("..And then allow Login after Signup")
        checkResponse(authService.loginRoute(loginReq("Morgan")),
          hasStatus(Status.TemporaryRedirect)
        )
      }

      "Should Block Unregistered Login" in new EmptyDatabase {
        implicit val cookieJar  = new MockCookieJar()

        checkResponse(
          authService.loginRoute(loginReq("Oliver")),
          hasStatus(Status.BadRequest)
        )

      }

      "Should respond with Unauthorized header + Json payload" in new EmptyDatabase {

      }
    }



    "When Registered" - {

      "Should Block Duplicate Registration" in new EmptyDatabase {
        implicit val cookieJar = new MockCookieJar()
        val authService = userAuthService()

        checkResponse(authService.signupRoute(signupReq("Morgan")),
          hasStatus(Status.TemporaryRedirect))

        checkResponse(authService.signupRoute(signupReq("Morgan")),
          hasStatus(Status.BadRequest))

        info("Still block dup. reg. after logout")
        checkResponse(authService.authedUserRoutes(logoutReq()))

        checkResponse(authService.signupRoute(signupReq("Morgan")),
          hasStatus(Status.BadRequest))
      }

      "When Logged In" - {
        implicit val cookieJar = new MockCookieJar()

        val authService = userAuthService()


        "Allow logout, re-login, and authorized/unauthorized status codes as appropriate" in new EmptyDatabase {
          // info(cookieJar.debugPrint("New Cookie Jar"))

          checkResponse(authService.signupRoute(signupReq("Morgan")))

          checkResponse(authService.authedUserRoutes(logoutReq()),
            hasStatus(Status.Ok))

          checkResponse(authService.authedUserRoutes(statusReq()),
            hasStatus(Status.Unauthorized))

          checkResponse(authService.loginRoute(loginReq("Morgan")),
            hasStatus(Status.TemporaryRedirect))

          checkResponse(authService.authedUserRoutes(statusReq()),
            tapWith( r => info(s"Status (login): ${r.as[String]}") ),
            hasStatus(Status.Ok))
        }

      }

      "When Not Logged In" - {

        implicit val cookieJar = new MockCookieJar()
        val authService = userAuthService()

        "Should return Unauthorized status" in {

          checkResponse(authService.authedUserRoutes(statusReq()),
            hasStatus(Status.Unauthorized))

        }



      }
    }
  }


  // "Should return Forbidden status" in {}
  // it should "Respond to a request without authentication with 401" in {
  // it should "Respond to a request with unknown username with 401" in {
  // it should "not run unauthorized routes" in {
  // it should "Respond to a request with wrong password with 401" in {

}
