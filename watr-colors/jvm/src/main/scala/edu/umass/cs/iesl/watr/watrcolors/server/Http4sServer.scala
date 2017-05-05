package edu.umass.cs.iesl.watr
package watrcolors
package server


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
import org.http4s.server.staticcontent._
import org.http4s.server.blaze._
import TypeTags._
import upickle.{default => UPickle}

import scala.concurrent._
// import scala.concurrent.ExecutionContext.Implicits.global
// import scala.collection.mutable


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
  val service1 = HttpService {
    case request @ GET -> Root / pageName =>
      pageName match {
        case "register"  => htmlPage("Registration")
        case _           => TemporaryRedirect(uri("/register/"))
      }
  }

  val webPageService = AuthedService[UserData] {
    case request @ GET -> Root / pageName as user =>
      pageName match {
        case "browse"    => htmlPage("BrowseCorpus")
        case "label"     => htmlPage("WatrColors")
        case _           => TemporaryRedirect(uri("/"))
      }
  }

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

  val autowireService  = authMiddleware(authedAutowire)



  val builder = BlazeBuilder.bindHttp(9999, "localhost")
    .mountService(webJarService)
    .mountService(assetService)
    .mountService(regionImageService, "/img")
    .mountService(autowireService, "/autowire")
    .mountService(loginHttpMiddleware, "/login")
    .mountService(webPageService, "/")

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

  // val autowireRoutes = HttpService {
  //   case req @ POST -> "api" /: path =>
  //     path.toList.headOption match {
  //       case Some("browse") =>

  //         import UPicklers._
  //         val router = ShellsideServer.route[BrowseCorpusApi](browseCorpusServer)
  //         Ok {
  //           req.bodyAsText().map{ body =>
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
  //           req.bodyAsText().map{ body =>
  //             for {
  //               resp <- ask(userSessions, RoutingRequest(
  //                 UserData("anon", "", ""),
  //                 path.toList.tail,
  //                 body
  //               )).mapTo[RoutingResponse]
  //               respData <- resp.response

  //             } yield respData.getBytes()
  //           }
  //         }

  //       case None =>
  //         ???
  //     }
  // }
