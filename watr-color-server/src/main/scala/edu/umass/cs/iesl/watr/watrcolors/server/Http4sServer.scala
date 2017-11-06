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
// import TypeTags._
// import scala.concurrent._
import play.api.libs.json, json._

import ammonite.{ops => fs}
class Http4sService(
  corpusAccessApi: CorpusAccessApi,
  url: String,
  port: Int,
  distDir: fs.Path
// ) extends AuthServer {
) {

  println(s"Current Dir: ${ fs.pwd }")

  val docStore: DocumentZoningApi = corpusAccessApi.docStore
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

    case req @ GET -> Root / "menu" =>
      val entries = for {
        stableId <- docStore.getDocuments(40, 0, Seq())
        docId <- docStore.getDocument(stableId)
      } yield {
        Json.obj(
          ("entry", stableId.unwrap),
          ("logfiles", ""),
        )
      }

      val jsstr = Json.stringify(
        JsArray(entries)
      )

      Ok(jsstr)
        .putHeaders(
          H.`Content-Type`(MediaType.`application/json`)
        )


  }

  def htmlPage(pageName: String, user: Option[String]): Task[Response]= {
    Ok(html.ShellHtml(pageName, user).toString())
      .putHeaders(
        H.`Content-Type`(MediaType.`text/html`)
      )
  }

  // Pages
  val htmlPageService = HttpService {
    case req @ GET -> Root =>
      htmlPage("Menu", None)

    case req @ GET -> Root / "register" =>
      htmlPage("Registration", None)

    // case req @ POST -> Root / "login" =>
    //   logInService(req)
  }


  // val authedPages = AuthedService[UserData] {

  //   case request @ GET -> Root / pageName as user =>
  //     pageName match {
  //       case "browse"    => htmlPage("BrowseCorpus", Some(user.emailAddr.unwrap))
  //       case "label"     => htmlPage("WatrColors", Some(user.emailAddr.unwrap))
  //     }

  //   case request @ GET -> Root  as user =>
  //     SeeOther(uri("/browse"))
  // }

  val redirectOnFailure: AuthedService[String] = Kleisli(req => SeeOther(uri("/user/register")))

  // val serveAuthorizedPages = AuthMiddleware(
  //   authUser,
  //   redirectOnFailure
  // )( userStatusAndLogout orElse authedPages )


  // val actorSystem = ActorSystem()
  // import actorSystem.dispatcher

  // lazy val browseCorpusServer = new BrowseCorpusApiListeners(corpusAccessApi)

  // val userSessions = actorSystem.actorOf(SessionsActor.props(corpusAccessApi), "sessionsActor")

  // val authedAutowire = AuthedService[UserData] {
  //   case req @ POST -> "api" /: path as user =>
  //     path.toList.headOption match {
  //       case Some("browse") =>

  //         val router = ShellsideServer.route[BrowseCorpusApi](browseCorpusServer)
  //         Ok {
  //           req.req.bodyAsText().map{ body =>
  //             router(
  //               autowire.Core.Request(
  //                 path.toList.tail,
  //                 UPickle.read[Map[String, String]](body)
  //               )
  //             ).map{ responseData =>
  //               responseData.getBytes
  //             }
  //           }
  //         }

  //       case Some("shell") =>

  //         implicit val timeout = Timeout(20.seconds)

  //         Ok {
  //           req.req.bodyAsText().map{ body =>
  //             for {
  //               resp <- ask(userSessions, RoutingRequest(
  //                 user,
  //                 path.toList.tail,
  //                 body
  //               )).mapTo[RoutingResponse]
  //               respData <- resp.response

  //             } yield respData.getBytes()
  //           }
  //         }

  //       case Some(path) =>
  //         sys.error(s"autowire request to unknown url ${path}")

  //       case None =>
  //         sys.error(s"autowire request to empty path")
  //     }
  // }


  // val autowireService = authOrForbid(authedAutowire)


  val builder = BlazeBuilder.bindHttp(port, url)
    .mountService(jslibDistService)
    .mountService(assetService)
    .mountService(htmlPageService)
    .mountService(pageImageService)
    // .mountService(userRegistration, "/user")

  def run(): Server = {
    builder.run
  }

  def serve(): Stream[Task, Nothing] = {
    builder.serve
  }

  def shutdown(): Unit = {}

}
