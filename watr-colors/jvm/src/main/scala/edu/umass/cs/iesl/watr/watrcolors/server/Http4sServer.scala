package edu.umass.cs.iesl.watr
package watrcolors
package server

// import scalaz.Kleisli
import scalaz.concurrent.Task

import akka.actor._
import akka.util.Timeout
import akka.pattern.{ ask }
// import akka.event.Logging
import concurrent.duration._

import corpora._
import docstore._

import org.http4s._
import org.http4s.{headers => H}
import org.http4s.dsl._
import org.http4s.server._
import org.http4s.server.syntax._
import org.http4s.server.staticcontent._
import org.http4s.server.blaze._
import TypeTags._
import upickle.{default => UPickle}

import scala.concurrent._


class Http4sService(
  reflowDB: TextReflowDB,
  corpus: Corpus,
  url: String,
  port: Int
) extends AuthServer {

  val assetService = resourceService(ResourceService.Config(
    basePath = "",
    pathPrefix = "/assets"
  ))

  val webJarService = resourceService(ResourceService.Config(
    basePath = "/META-INF/resources/webjars",
    pathPrefix = "/webjars"
  ))


  val regionImageService = HttpService {
    case request @ GET -> Root / "region" / regionId =>
      val bytes = reflowDB.serveTargetRegionImage(RegionID(regionId.toInt))
      Ok(bytes).putHeaders(
        H.`Content-Type`(MediaType.`image/png`)
      )
  }

  def htmlPage(pageName: String): Task[Response]= {
    Ok(html.ShellHtml(pageName).toString())
      .putHeaders(
        H.`Content-Type`(MediaType.`text/html`)
      )
  }

  // Pages
  val userRegistration = HttpService {
    case req @ GET -> Root / "register" =>
      htmlPage("Registration")

    case req @ POST -> Root / "login" =>
      logInService(req)
  }


  val authedPages = AuthedService[UserData] {

    case request @ GET -> Root / pageName as user =>
      pageName match {
        case "browse"    => htmlPage("BrowseCorpus")
        case "label"     => htmlPage("WatrColors")
      }

    case request @ GET -> Root  as user =>
      SeeOther(uri("/browse"))
  }


  val serveAuthorizedPages = AuthMiddleware(
    authUser,
    forbidOnFailure
  )( userStatusAndLogout orElse authedPages )
  // val serveAuthorizedPages = AuthMiddleware(authUser)(authedPages)


  val actorSystem = ActorSystem()
  import actorSystem.dispatcher

  lazy val browseCorpusServer = new BrowseCorpusApiListeners(reflowDB, corpus)

  val userSessions = actorSystem.actorOf(SessionsActor.props(reflowDB, corpus), "sessionsActor")

  val authedAutowire = AuthedService[UserData] {
    case req @ POST -> "api" /: path as user =>
      path.toList.headOption match {
        case Some("browse") =>

          import UPicklers._
          val router = ShellsideServer.route[BrowseCorpusApi](browseCorpusServer)
          Ok {
            req.req.bodyAsText().map{ body =>
              router(
                autowire.Core.Request(
                  path.toList.tail,
                  UPickle.read[Map[String, String]](body)
                )
              ).map{ responseData =>
                responseData.getBytes
              }
            }
          }

        case Some("shell") =>


          implicit val timeout = Timeout(20.seconds)

          Ok {
            req.req.bodyAsText().map{ body =>
              for {
                resp <- ask(userSessions, RoutingRequest(
                  user,
                  path.toList.tail,
                  body
                )).mapTo[RoutingResponse]
                respData <- resp.response

              } yield respData.getBytes()
            }
          }

        case None =>
          ???
      }
  }

  val autowireService = authOrForbid(authedAutowire)


  // val aggregateService = (
  //   serveAuthorizedPages
  // ) // orElse authStatusLogoutService

  val builder = BlazeBuilder.bindHttp(9999, "localhost")
    .mountService(serveAuthorizedPages)
    .mountService(webJarService)
    .mountService(assetService)
    .mountService(regionImageService, "/img")
    .mountService(autowireService, "/autowire")
    .mountService(userRegistration, "/user")

  def run(): Server = {
    builder.run
  }

  def shutdown(): Unit = {
    Await.result(
      actorSystem.terminate(),
      Duration.Inf
    )
  }
}
