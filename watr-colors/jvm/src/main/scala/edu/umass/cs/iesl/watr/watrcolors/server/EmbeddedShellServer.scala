package edu.umass.cs.iesl.watr
package watrcolors
package server

import akka.actor.{ActorRef, Actor, ActorSystem, Props}
import akka.util.ByteString
import spray.routing.{ Route, SimpleRoutingApp }

import spray.http._
import HttpHeaders._
import HttpMethods._

import concurrent.duration._
// import scala.concurrent.Future
import scala.collection.mutable

import corpora._
import docstore._

import upickle.{default => UPickle}

case class UserData(user: String, pass: String, session: String)

class EmbeddedServer(
  reflowDB: TextReflowDB,
  corpus: Corpus,
  url: String,
  port: Int
) extends SimpleRoutingApp {

  implicit val system = ActorSystem()
  import system.dispatcher

  object actors {
    val corsHeaders: List[ModeledHeader] =
      List(
        `Access-Control-Allow-Methods`(OPTIONS, GET, POST),
        `Access-Control-Allow-Origin`(AllOrigins),
        `Access-Control-Allow-Headers`("Origin, X-Requested-With, Content-Type, Accept, Accept-Encoding, Accept-Language, Host, Referer, User-Agent"),
        `Access-Control-Max-Age`(1728000)
      )


    /**
      * Actor meant to handle long polling, buffering messages or waiting actors
      */
    val longPoll = system.actorOf(Props[LongPollActor])

    class LongPollActor extends Actor {
      var waitingActor: Option[ActorRef] = None
      var queuedMessages = List[RemoteCall]()

      /**
        * Flushes returns nothing to any waiting actor every so often,
        * to prevent the connection from living too long.
        */
      case object Clear

      system.scheduler.schedule(0.seconds, 10.seconds, self, Clear)

      def respond(a: ActorRef, s: String) = {
        val respEntity = HttpEntity(
          HttpData(s)
        )
        a ! HttpResponse(
          entity = respEntity,
          headers = corsHeaders
        )
      }
      def receive = (x: Any) => (x, waitingActor, queuedMessages) match {
        case (a: ActorRef, _, Nil) =>
          waitingActor = Some(a)

        case (a: ActorRef, None, msgs) =>
          println(s"""receive: Actor ! msgs waiting""")
          val wr = UPickle.write[List[RemoteCall]](queuedMessages)
          respond(a, wr)
          queuedMessages = Nil

        case (msg: RemoteCall, None, msgs) =>
          println(s"""receive: msg enqueue""")
          queuedMessages = msg :: msgs

        case (msg: RemoteCall, Some(a), Nil) =>
          println(s"""receive: waiting actor gets msg""")
          val wr = UPickle.write(List(msg))
          respond(a, wr)
          waitingActor = None

        case (Clear, waitingOpt, msgs) =>
          val wr = UPickle.write(msgs)
          waitingOpt.foreach(respond(_, wr))
          waitingActor = None
      }
    }

  }

  object httpserver {


    def webjarResources = pathPrefix("webjars") {
      getFromResourceDirectory("META-INF/resources/webjars")
    }

    def assets = pathPrefix("assets") {
      getFromResourceDirectory("")
    }

    def httpResponse(resp: String) = {
      HttpEntity(MediaTypes.`text/html`, resp)
    }

    import TypeTags._

    def produceRegionImage(regionId: String): Array[Byte] = {
      val id = RegionID(regionId.toInt)
      reflowDB.serveTargetRegionImage(id)
    }

    def regionImageServer =
      pathPrefix("img") {
        pathPrefix("region") {
          path(Segment) { regionId =>
            complete(
              HttpResponse(entity =
                HttpEntity(MediaTypes.`image/png`, HttpData(produceRegionImage(regionId)))
              )
            )
          }
        }
      }


    def login() =
      pathPrefix("login")

    def webPage(pageName: String) =
      extract(_.request.entity.data) ( requestData => ctx =>
        ctx.complete { httpResponse(html.ShellHtml(pageName).toString()) }
      )

    def webPages =
      ( pathPrefix("browse")(webPage("BrowseCorpus")) ~
        pathPrefix("label") (webPage("WatrColors")) ~
        pathPrefix("") (redirect("/browse", StatusCodes.SeeOther))
      )


    def apiRoute(
      prefix: String,
      router: autowire.Core.Router[String]
    ) = {
      pathPrefix("api")(
        path(prefix / Segments) { segs =>
          extract(_.request.entity.data) { requestData => ctx =>
            ctx.complete(
              router(autowire.Core.Request(
                segs,
                UPickle.read[Map[String, String]](
                  requestData.asString
                )
              )).map(responseData =>
                HttpEntity(HttpData(ByteString(responseData)))
              )
            )}})

    }

    import UPicklers._

    val userServers = mutable.HashMap[String, BioArxivServer]()

    // lazy val bioArxivServer = new BioArxivServer(reflowDB, corpus)

    // def autowireWatrShell = apiRoute("shell",
    //   ShellsideServer.route[WatrShellApi](bioArxivServer)
    // )

    lazy val browseCorpusServer = new BrowseCorpusApiListeners(reflowDB, corpus)

    def autowireBrowseCorpus = apiRoute("browse",
      ShellsideServer.route[BrowseCorpusApi](browseCorpusServer)
    )

    def ensureLogin(f: UserData => Route): Route = {
      optionalCookie("user") {
        case Some(user) =>
          f(UserData(user.value, "", ""))

        case None =>
          post(
            path("/login"){ctx =>
              formFields('username) { user =>
                println(s"ensureLogin:POST:/login -> ")
                // setCookie(HttpCookie(name = "user", content = user, maxAge = Some(6000))) & redirect("/", StatusCodes.SeeOther)
                setCookie(HttpCookie(name = "user", content = user, maxAge = None, domain=Some("/"))) & redirect("/", StatusCodes.SeeOther)
              }
            }
          )
      }
    }


    val loginRoute = {
      path("register")(
        (get|post)(
          extract(_.request.entity.data) ( requestData => ctx =>
            ctx.complete{
              HttpEntity(MediaTypes.`text/html`,
                html.ShellHtml.register().toString
              )
            }

          )
        )
      )

    }

    val stdRoutes = {
      val noAuthRoutes =
        get (
          webjarResources
            ~ assets
            ~ regionImageServer
            ~ webPages
        )


      ( loginRoute ~
        noAuthRoutes ~
        ensureLogin { userData =>
          get(
            path("logout"){ctx =>
              deleteCookie("user") & redirect("/register", StatusCodes.SeeOther)
            }
          ) ~ post (
            apiRoute("shell", {

              val server = userServers.getOrElseUpdate(userData.user, {
                println(s"Creating new server for ${userData}")
                new BioArxivServer(userData, reflowDB, corpus)
              })
              println(s"Fetched server for ${userData}")

              ShellsideServer.route[WatrShellApi](server)
            }) ~ autowireBrowseCorpus
          )
        }
      )
    }

    def run(): Unit = {
      startServer(url, port)(stdRoutes)
    }

    def kill() = {
      system.terminate()
    }
  }

}
