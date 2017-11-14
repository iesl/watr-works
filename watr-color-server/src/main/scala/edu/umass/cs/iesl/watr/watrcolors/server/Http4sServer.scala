package edu.umass.cs.iesl.watr
package watrcolors
package server

import fs2._
import cats.data.Kleisli
import fs2.interop.cats._

import corpora._
import corpora.filesys._
import workflow._

import org.http4s
import org.http4s._
import org.http4s.{headers => H}
import org.http4s.dsl._
import org.http4s.server._
// import org.http4s.server.syntax._
import org.http4s.server.staticcontent._
import org.http4s.server.blaze._
import org.http4s.circe._
import _root_.io.circe
// import circe._
// import circe.generic._
// import circe.generic.auto._
// import circe.syntax._
import circe.literal._


import ammonite.{ops => fs}

class Http4sService(
  override val corpusAccessApi: CorpusAccessApi,
  url: String,
  port: Int,
  distDir: fs.Path
) extends LabelingServices {

  println(s"Current Dir: ${ fs.pwd }")

  private val docStore: DocumentZoningApi = corpusAccessApi.docStore
  val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  // override val userbaseApi: UserbaseApi = corpusAccessApi.userbaseApi
  val corpus: Corpus = corpusAccessApi.corpus


  val assetService = resourceService(ResourceService.Config(
    basePath = "",
    pathPrefix = "/assets"
  ))

  val jslibDistService = fileService(FileService.Config(
    systemPath = distDir.toString(),
    pathPrefix = "/dist"
  ))

  val pageImageService = HttpService {
    case req @ GET -> Root / "entry" / entryId / "image" / "page" / IntVar(pageNum) =>
      val artifactName = s"page-${pageNum}.opt.png"
      val maybeImage = for {
        entry <- corpus.entry(entryId)
        pageImages <- entry.getArtifactGroup("page-images")
        pageImage <- pageImages.getArtifact(artifactName)
        imagePath <- pageImage.asPath.toOption
      } yield {
        println(s"pageImageService: serving page image ${entryId} from ${imagePath}")
        StaticFile.fromFile(imagePath.toIO, Some(req))
          .getOrElse {
            Response(http4s.Status(404)(s"could not serve image ${entryId} page ${pageNum}"))
          }
      }
      maybeImage.getOrElse {
        Task.now{
          Response(http4s.Status(500)(s"could not serve image ${entryId} page ${pageNum}"))
        }
      }

    case req @ GET -> Root / "entry" / entryId / "image" / "thumb" / IntVar(pageNum) =>

      val artifactName = s"page-${pageNum}.png"
      val maybeImage = for {
        entry <- corpus.entry(entryId)
        pageImages <- entry.getArtifactGroup("page-thumbs")

        pageImage <- pageImages.getArtifact(artifactName)
        imagePath <- pageImage.asPath.toOption
      } yield {
        StaticFile.fromFile(imagePath.toIO, Some(req))
          .getOrElse {
            Response(http4s.Status(404)(s"could not serve image ${entryId} page ${pageNum}"))
          }

      }
      maybeImage.getOrElse {
        Task.now{
          Response(http4s.Status(500)(s"could not serve image ${entryId} page ${pageNum}"))
        }
      }

    case req @ GET -> Root / "vtrace" /  "json" / entryId / jsonArtifact =>

      val maybeResp = for {
        entry <- corpus.entry(entryId)
        traceLogs <- entry.getArtifactGroup("tracelogs")
        artifact <- traceLogs.getArtifact(jsonArtifact)
        artifactPath <- artifact.asPath.toOption
      } yield {
        StaticFile
          .fromFile(artifactPath.toIO, Some(req))
          .getOrElse { Response(http4s.Status(404)(s"could not serve ${entryId} artifact ${jsonArtifact}")) }
      }
      maybeResp.getOrElse {
        Task.now{
          Response(http4s.Status(500)(s"could not serve ${entryId} artifact ${jsonArtifact}"))
        }
      }

    case req @ GET -> Root / "menu" =>
      val entries = for {
        stableId <- docStore.getDocuments(40, 0, Seq())
        docId <- docStore.getDocument(stableId)
      } yield {
        json"""{ "entry": ${stableId.unwrap}, "logfiles": "" }"""
      }

      val all = circe.Json.arr(
        entries:_*
      )

      Ok(all)
        .putHeaders(
          H.`Content-Type`(MediaType.`application/json`)
        )


  }

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
    .mountService(pageImageService)

  def run(): Server = {
    builder.run
  }

  def serve(): Stream[Task, Nothing] = {
    builder.serve
  }

  def shutdown(): Unit = {}

}

