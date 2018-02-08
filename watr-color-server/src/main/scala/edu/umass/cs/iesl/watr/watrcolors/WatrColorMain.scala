package edu.umass.cs.iesl.watr
package watrcolors


import cats.effect.IO

// import org.http4s._
import org.http4s.dsl.Http4sDsl
// import org.http4s.{headers => H}
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.staticcontent._
// import org.http4s.server.middleware.{CORS, CORSConfig}

// import org.http4s.util.{ ExitCode, StreamApp }
import fs2.StreamApp.ExitCode

import persistence._
import services._

import utils.{PathUtils => P}
import edu.umass.cs.iesl.watr.table._
import ammonite.{ops => fs}

import corpora._
import services._

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
) extends AllTheServices {

  val assetService = resourceService(ResourceService.Config[IO](
    basePath = "",
    pathPrefix = "/"
  ))
  val jslibDistService = fileService(FileService.Config[IO](
    systemPath = distDir.toString(),
    pathPrefix = "/"
  ))

  lazy val corpusAccessDB  = corpusAccessApi.corpusAccessDB


  lazy val userStore = UserStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
  lazy val authStore = PasswordStore.fromDb(corpusAccessApi.corpusAccessDB).unsafeRunSync()
  lazy val tokenStore = MemTokenStore.apply().unsafeRunSync()



  def buildServer() = {
    BlazeBuilder[IO]
      .bindHttp(portNum, "localhost")
      .mountService(assetService                      , "/assets")
      .mountService(jslibDistService                  , "/dist")
      .mountService(userAuthenticationServices        , "/api/v1/auth")
      .mountService(labelingServiceEndpoints          , "/api/v1/labeling")
      .mountService(curationServices                  , "/api/v1/workflow")
      .mountService(corpusArtifactEndpoints           , "/api/v1/corpus/artifacts")
      .mountService(corpusListingEndpoints            , "/api/v1/corpus/entries")
      .mountService(htmlPageServices                  , "/")

  }
}

object WiredServerMain extends fs2.StreamApp[IO] with Http4sDsl[IO] with utils.AppMainBasics {
  import java.util.concurrent.Executors
  import scala.concurrent._
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  // implicit val refEc = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))


  def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, ExitCode] = {
    val argMap = argsToMap(args.toArray)

    val port = argMap.get("port").flatMap(_.headOption)
      .getOrElse(sys.error("no port supplied (--port ...)"))

    val distRoot = argMap.get("dist").flatMap(_.headOption)
      .getOrElse(sys.error("no dist dir specified (--dist ...); "))

    val portNum = port.toInt

    val distDir = P.strToAmmPath(distRoot)

    val corpusAccessApi = SharedInit.initCorpusAccessApi(args.toArray)

    val allServices = new AllServices(corpusAccessApi, distDir, portNum)

    val server = allServices.buildServer()

    server.serve

  }

}
