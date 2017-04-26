package edu.umass.cs.iesl.watr
package watrcolors
package server

import akka.actor.{ActorRef, Actor, ActorSystem, Props}
import akka.util.ByteString

import spray.routing.SimpleRoutingApp
import spray.http._
import HttpHeaders._
import HttpMethods._

import concurrent.duration._

import corpora._
import docstore._

// import autowire._
import upickle.{default => UPickle}
import UPickle._
import TypeTagPicklers._


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


    def webPage(pageName: String) =
      extract(_.request.entity.data) ( requestData => ctx =>
        ctx.complete { httpResponse(html.ShellHtml(pageName).toString()) }
      )

    def webPages =
      ( pathPrefix("browse")(webPage("BrowseCorpus")) ~
        pathPrefix("label") (webPage("WatrColors")) ~
        pathPrefix("") (redirect("/browse", StatusCodes.PermanentRedirect))
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


    def autowireWatrShell = apiRoute("shell",
      ShellsideServer.route[WatrShellApi](
        new BioArxivServer(reflowDB, corpus)
      ))

    def autowireBrowseCorpus = apiRoute("browse",
      ShellsideServer.route[BrowseCorpusApi](
        new BrowseCorpusApiListeners(reflowDB, corpus)
      ))

    def run(): Unit = {
      startServer(url, port)(
        get(
          webjarResources
          ~  assets
          ~  regionImageServer
          ~  webPages
        ) ~
        post(
          path("notifications") (ctx => actors.longPoll ! ctx.responder)
          ~ autowireWatrShell
          ~ autowireBrowseCorpus
        )
      )

    }

    def kill() = system.terminate()
  }




}
