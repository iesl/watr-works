package edu.umass.cs.iesl.watr
package watrcolors
package server

// import fs2._
// import cats.data.Kleisli
import cats.effect._

import corpora._

import org.http4s._
import org.http4s.util.ExitCode
import org.http4s.{headers => H}
// import org.http4s.dsl._
import org.http4s.server._
import org.http4s.server.staticcontent._
import org.http4s.server.blaze._

import ammonite.{ops => fs}

import cats.effect._

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

  // val pageImageService = HttpService {

  //   case req @ GET -> Root / "menu" =>
  //     val entries = for {
  //       stableId <- docStore.getDocuments(40, 0, Seq())
  //       docId <- docStore.getDocument(stableId)
  //     } yield {
  //       json"""{ "entry": ${stableId.unwrap}, "logfiles": "" }"""
  //     }

  //     val all = circe.Json.arr(
  //       entries:_*
  //     )

  //     Ok(all)
  //       .putHeaders(
  //         H.`Content-Type`(MediaType.`application/json`)
  //       )
  // }

  def htmlPage(bundleName: String, user: Option[String]): IO[Response[IO]]= {
    Ok(html.Frame(bundleName).toString())
      .putHeaders(
        H.`Content-Type`(MediaType.`text/html`)
      )
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
  val redirectOnFailure: AuthedService[String, IO] = AuthedService.lift{ req => SeeOther(uri("/user/register")) }

  val builder = BlazeBuilder[IO].bindHttp(port, url)
    .mountService(jslibDistService)
    .mountService(assetService)
    .mountService(htmlPageService)
    .mountService(labelingServiceEndpoints, "/api/v1/labeling")
    .mountService(curationWorkflowEndpoints, "/api/v1/workflow")
    .mountService(corpusArtifactEndpoints, "/api/v1/corpus/artifacts")
    .mountService(corpusListingEndpoints, "/api/v1/corpus/entries")
    .mountService(loginService, "/api/v1/auth")

  def run(): Server[IO]= {
    builder.start
      .unsafeRunSync()
  }


  def serve(): fs2.Stream[IO, ExitCode] = {
    builder.serve
  }


  def shutdown(): Unit = {}

}
