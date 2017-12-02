package edu.umass.cs.iesl.watr
package watrcolors


import cats.effect.IO

import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.{headers => H}
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.staticcontent._
import org.http4s.server.middleware.{CORS, CORSConfig}
import org.http4s.util.{ ExitCode, StreamApp }
import persistence._
import services._
import scala.concurrent.ExecutionContext.Implicits.global
// import tsec.authentication._

// import tsec.cipher.symmetric.imports.AES128
// import models._

// import scala.concurrent.ExecutionContext
// import java.util.concurrent.Executors

import utils.{PathUtils => P}
import edu.umass.cs.iesl.watr.table._
import ammonite.{ops => fs}

import corpora._
import services._

trait TheServices extends LabelingServices
    with CurationWorkflowServices
    with CorpusListingServices
    with CorpusArtifactServices
    with UserAuthenticationServices

class AllServices(
  override val corpusAccessApi: CorpusAccessApi,
  distDir: fs.Path,
  portNum: Int
)(
) extends Http4sDsl[IO] with TheServices {

  val assetService = resourceService(ResourceService.Config[IO](
    basePath = "",
    pathPrefix = "/assets"
  ))



  val corsConfig = CORSConfig(
    anyOrigin = true,
    allowCredentials = true,
    maxAge = 100000
  )


  def htmlPage(bundleName: String, user: Option[String]): IO[Response[IO]]= {
    Ok(html.Frame(bundleName).toString())
      .map { _.putHeaders(H.`Content-Type`(MediaType.`text/html`)) }
  }


  // Pages
  val htmlPageService = HttpService[IO] {
    case req @ GET -> Root =>
      htmlPage("browse", None)

    case req @ GET -> Root / "document" / stableId =>
      htmlPage("document", None)

    case req @ GET -> Root / "register" =>
      htmlPage("Registration", None)

      // case req @ POST -> Root / "login" =>
      //   logInService(req)
  }

  lazy val userStore = UserStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
  lazy val authStore = PasswordStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
  lazy val tokenStore = MemTokenStore.apply[IO].unsafeRunSync()


  val jslibDistService = fileService(FileService.Config[IO](
    systemPath = distDir.toString(),
    pathPrefix = "/dist"
  ))

  def buildServer() = {
    BlazeBuilder[IO]
      .bindHttp(portNum, "localhost")
      .mountService(CORS(jslibDistService))
      .mountService(CORS(assetService))
      .mountService(CORS(jslibDistService))
      .mountService(htmlPageService)
      .mountService(userAuthenticationServices        , "/api/v1/auth")
      .mountService(labelingServiceEndpoints          , "/api/v1/labeling")
      .mountService(curationWorkflowEndpoints         , "/api/v1/workflow")
      .mountService(corpusArtifactEndpoints           , "/api/v1/corpus/artifacts")
      .mountService(corpusListingEndpoints            , "/api/v1/corpus/entries")

  }
}

object WiredServerMain extends StreamApp[IO] with Http4sDsl[IO] with utils.AppMainBasics {


  def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, ExitCode] = {
    // implicit val refEc = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
    val argMap = argsToMap(args.toArray)

    val port = argMap.get("port").flatMap(_.headOption)
      .getOrElse(sys.error("no port supplied (--port ...)"))

    val distRoot = argMap.get("dist").flatMap(_.headOption)
      .getOrElse(sys.error("no dist dir specified (--dist ...); "))

    val portNum = port.toInt

    val distDir = P.strToAmmPath(distRoot)

    val corpusAccessApi = SharedInit.initCorpusAccessApi(args.toArray)

    // _         <- F.pure(println(s"exists; $exists"))
    // _         <- F.pure(println(s"password; $password"))
    // _         <- F.pure( println(s"newUser; $newUser") )
    // _         <- F.pure( println(s"cookie; $cookie") )
    val allServices = new AllServices(corpusAccessApi, distDir, portNum)()

    val server = allServices.buildServer()

    server.serve

    // Stream.eval(server).flatMap(_.serve)
  }

}
