package edu.umass.cs.iesl.watr
package watrcolors

import java.util.concurrent.Executors

import cats.effect.IO
import fs2.Stream
// import io.circe._
// import org.http4s._
// import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.staticcontent._
import org.http4s.server.middleware.{CORS, CORSConfig}
import org.http4s.util.{ ExitCode, StreamApp }
import persistence.{PasswordStore, TokenStore, UserStore}
import services._
import tsec.authentication._
import tsec.cipher.symmetric.imports.{AES128, SecretKey}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import utils.{PathUtils => P}

object WiredServerMain extends StreamApp[IO] with Http4sDsl[IO] with utils.AppMainBasics {
  val assetService = resourceService(ResourceService.Config[IO](
    basePath = "",
    pathPrefix = "/assets"
  ))



  val corsConfig = CORSConfig(
    anyOrigin = true,
    allowCredentials = true,
    maxAge = 100000
  )

  val authenticatorSettings = TSecCookieSettings("tsec-auth", secure = false, httpOnly = true,
    expiryDuration = 1.hour, //   scala.concurrent.duration.FiniteDuration,
    maxIdle = Some(1.hour) //  Option[scala.concurrent.duration.FiniteDuration]
  )


  def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, ExitCode] = {
    implicit val refEc = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
    val argMap = argsToMap(args.toArray)

    val port = argMap.get("port").flatMap(_.headOption)
      .getOrElse(sys.error("no port supplied (--port ...)"))

    val distRoot = argMap.get("dist").flatMap(_.headOption)
      .getOrElse(sys.error("no dist dir specified (--dist ...); "))

    val portNum = port.toInt

    val distDir = P.strToAmmPath(distRoot)

    val jslibDistService = fileService(FileService.Config[IO](
      systemPath = distDir.toString(),
      pathPrefix = "/dist"
    ))


    val wiring = for {
      userStore     <- UserStore.fromUserbaseApi(userbaseApi)
      tokenStore    <- TokenStore.apply
      passwordStore <- PasswordStore.apply
      symmetricKey  <- AES128.generateLift[IO]
    } yield {
      val authenticator = EncryptedCookieAuthenticator.withBackingStore[IO, Int, models.User, AES128](
        authenticatorSettings,
        tokenStore,
        userStore,
        symmetricKey
      )

      val userAuthService = UserAuthenticationService(userStore, passwordStore, authenticator)

      BlazeBuilder[IO]
        .bindHttp(portNum, "localhost")
        .mountService(CORS(userAuthService.signupRoute))
        .mountService(CORS(userAuthService.loginRoute))
        .mountService(CORS(assetService))
        .mountService(CORS(jslibDistService))
      // .mountService(CORS(authedService.helloFromAuthentication))
    }

    Stream.eval(wiring).flatMap(_.serve)
  }

}
