package edu.umass.cs.iesl.watr
package watrcolors
package server

import cats.effect._

import corpora._

import org.http4s._
import org.http4s.util.ExitCode
import org.http4s.{headers => H}
import org.http4s.server.staticcontent._
import org.http4s.server.blaze._

import org.http4s.util.StreamApp
import org.http4s.util.ExitCode

import ammonite.{ops => fs}

import cats.effect._

import utils.{PathUtils => P}
import edu.umass.cs.iesl.watr.table._

trait AllServices extends LabelingServices
    with CurationWorkflowServices
    with CorpusListingServices
    with CorpusArtifactServices
    with UserAuthenticationServices


class Http4sService(
  override val corpusAccessApi: CorpusAccessApi,
  url: String,
  port: Int,
  distDir: fs.Path
) extends AllServices {

  println(s"Current Dir: ${ fs.pwd }")

  val assetService = resourceService(ResourceService.Config[IO](
    basePath = "",
    pathPrefix = "/assets"
  ))

  val jslibDistService = fileService(FileService.Config[IO](
    systemPath = distDir.toString(),
    pathPrefix = "/dist"
  ))


  def htmlPage(bundleName: String, user: Option[String]): IO[Response[IO]]= {
    Ok().flatMap { resp =>
      resp
        .withBody(html.Frame(bundleName).toString())
        .putHeaders(H.`Content-Type`(MediaType.`text/html`))
    }
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



  // val redirectOnFailure: AuthedService[String, IO] = Kleisli(req => SeeOther(uri("/user/register")))
  // val redirectOnFailure: AuthedService[String, IO] = AuthedService.lift{ req => SeeOther(uri("/user/register")) }

  val builder = BlazeBuilder[IO].bindHttp(port, url)
    .mountService(jslibDistService)
    .mountService(assetService)
    .mountService(htmlPageService)
    .mountService(labelingServiceEndpoints, "/api/v1/labeling")
    .mountService(curationWorkflowEndpoints, "/api/v1/workflow")
    .mountService(corpusArtifactEndpoints, "/api/v1/corpus/artifacts")
    .mountService(corpusListingEndpoints, "/api/v1/corpus/entries")
    // .mountService(loginService, "/api/v1/auth")


  def serve(): fs2.Stream[IO, ExitCode] = {
    builder.serve
  }


  def shutdown(): Unit = {}


}

object WatrColorMain extends StreamApp[IO] with utils.AppMainBasics {
  override def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, ExitCode] = {

    val corpusAccessApi = SharedInit.initCorpusAccessApi(args.toArray)

    val argMap = argsToMap(args.toArray)

    val port = argMap.get("port").flatMap(_.headOption)
      .getOrElse(sys.error("no port supplied (--port ...)"))

    val distRoot = argMap.get("dist").flatMap(_.headOption)
      .getOrElse(sys.error("no dist dir specified (--dist ...); "))

    val portNum = port.toInt

    val distDir = P.strToAmmPath(distRoot)

    val httpService =  new Http4sService(corpusAccessApi, "localhost", portNum, distDir)
    httpService.serve()

  }
}
