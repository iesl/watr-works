package edu.umass.cs.iesl.watr
package watrcolors


import cats.effect._
import cats.implicits._

import org.http4s.dsl.Http4sDsl
import org.http4s.{ HttpApp, HttpRoutes }
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.server.staticcontent._
import org.http4s.syntax.kleisli._


import persistence._
import services._

import utils.{PathUtils => P}
import edu.umass.cs.iesl.watr.table._
import ammonite.{ops => fs}

import corpora._
import services._
import utils.Threading.BlockingFileIO

import java.util.concurrent.Executors
import scala.concurrent._


trait AllTheServices extends ZoningServices
    with CurationWorkflowServices
    with CorpusListingServices
    with CorpusArtifactServices
    with UserAuthenticationServices
    with HtmlPageService

class AllServices(
  override val corpusAccessApi: CorpusAccessApi,
  distDir: fs.Path,
  portNum: Int
)(implicit
  override val cs: ContextShift[IO],
  ce: ConcurrentEffect[IO],
  timer: Timer[IO],
  override val blockingEc: ExecutionContext
) extends AllTheServices {

  // 34: scalac: not enough arguments for method apply: (basePath: String, blockingExecutionContext: scala.concurrent.ExecutionContext, pathPrefix: String, bufferSize: Int, cacheStrategy: org.http4s.server.staticcontent.CacheStrategy[cats.effect.IO], preferGzipped: Boolean)org.http4s.server.staticcontent.ResourceService.Config[cats.effect.IO] in object Config.
  val assetService = resourceService(new ResourceService.Config[IO](
    basePath = "",
    BlockingFileIO,
    pathPrefix = "/"
  ))
  val jslibDistService = fileService(FileService.Config[IO](
    systemPath = distDir.toString(),
    pathPrefix = "/"
  ))

  lazy val corpusAccessDB  = corpusAccessApi.corpusAccessDB


  lazy val userStore = UserStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
  lazy val authStore = PasswordStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
  // lazy val tokenStore = MemTokenStore.apply().unsafeRunSync()
  lazy val tokenStore = new TokenStore(corpusAccessDB)


  def httpApp(): HttpApp[IO] = Router(
    // "/assets" -> assetService
    // "/api/v1/labeling" -> labelingServiceEndpoints
    "/" -> htmlPageServices
  ).orNotFound

  def buildServer(): BlazeServerBuilder[IO] = {
    BlazeServerBuilder[IO]
      .bindHttp(portNum, "localhost")
      .withHttpApp(httpApp)


      // .mountService(assetService                      , "/assets")
      // .mountService(jslibDistService                  , "/dist")
      // .mountService(userAuthenticationServices        , "/api/v1/auth")
      // .mountService(labelingServiceEndpoints          , "/api/v1/labeling")
      // .mountService(curationServices                  , "/api/v1/workflow")
      // .mountService(corpusArtifactEndpoints           , "/api/v1/corpus/artifacts")
      // .mountService(corpusListingEndpoints            , "/api/v1/corpus/entries")
      // .mountService(htmlPageServices                  , "/")

  }
}

object WiredServerMain extends IOApp with Http4sDsl[IO] with utils.AppMainBasics {
  import java.util.concurrent.Executors
  import scala.concurrent._
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  // def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, ExitCode] = {
  def run(args: List[String]): IO[ExitCode] = {
    val argMap = argsToMap(args)

    val port = argMap.get("port").flatMap(_.headOption)
      .getOrElse(sys.error("no port supplied (--port ...)"))

    val distRoot = argMap.get("dist").flatMap(_.headOption)
      .getOrElse(sys.error("no dist dir specified (--dist ...); "))

    val portNum = port.toInt

    val distDir = P.strToAmmPath(distRoot)

    val sharedInit = new SharedInit()
    val corpusAccessApi = sharedInit.initCorpusAccessApi(args.toArray)

    val allServices = new AllServices(corpusAccessApi, distDir, portNum)

    val server = allServices.buildServer()

    server
      .serve
      .compile
      .drain
      .as(ExitCode.Success)


  }

}
