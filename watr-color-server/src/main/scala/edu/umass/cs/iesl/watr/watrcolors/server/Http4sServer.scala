package edu.umass.cs.iesl.watr
package watrcolors
package server

import fs2._
import cats.data.Kleisli

import corpora._

import org.http4s._
import org.http4s.{headers => H}
import org.http4s.dsl._
import org.http4s.server._
import org.http4s.server.staticcontent._
import org.http4s.server.blaze._

import ammonite.{ops => fs}


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

  val assetService = resourceService(ResourceService.Config(
    basePath = "",
    pathPrefix = "/assets"
  ))

  val jslibDistService = fileService(FileService.Config(
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

  def htmlPage(bundleName: String, user: Option[String]): Task[Response]= {
    Ok(html.Frame(bundleName).toString())
      .putHeaders(
        H.`Content-Type`(MediaType.`text/html`)
      )
  }

  // Pages
  val htmlPageService = HttpService {
    case req @ GET -> Root =>
      htmlPage("menu", None)

    case req @ GET -> Root / "document" / stableId =>
      htmlPage("document", None)

    case req @ GET -> Root / "register" =>
      htmlPage("Registration", None)

    // case req @ POST -> Root / "login" =>
    //   logInService(req)
  }



  val redirectOnFailure: AuthedService[String] = Kleisli(req => SeeOther(uri("/user/register")))

  val builder = BlazeBuilder.bindHttp(port, url)
    .mountService(jslibDistService)
    .mountService(assetService)
    .mountService(htmlPageService)
    .mountService(labelingServiceEndpoints, "/api/v1/labeling")
    .mountService(curationWorkflowEndpoints, "/api/v1/workflow")
    .mountService(corpusArtifactEndpoints, "/api/v1/corpus/artifacts")
    .mountService(corpusListingEndpoints, "/api/v1/corpus/entries")

  def run(): Server = {
    builder.run
  }

  def serve(): Stream[Task, Nothing] = {
    builder.serve
  }

  def shutdown(): Unit = {}

}
