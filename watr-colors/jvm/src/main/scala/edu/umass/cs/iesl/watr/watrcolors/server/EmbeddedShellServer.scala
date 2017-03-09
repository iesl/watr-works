package edu.umass.cs.iesl.watr
package watrcolors
package server

import akka.actor.{ActorRef, Actor, ActorSystem}
import akka.util.ByteString
import akka.actor.ActorDSL._

import spray.routing.SimpleRoutingApp
import spray.http._
import HttpHeaders._
import HttpMethods._

import scala.concurrent.Future
import concurrent.duration._

import corpora._
// import textreflow.data._
import geometry._
import labeling._
import docstore._

import autowire._
import upickle.{default => UPickle}
import UPickle._
import TypeTagPicklers._


class EmbeddedServer(
  reflowDB: TextReflowDB,
  corpus: Corpus,
  url: String,
  port: Int
) extends SimpleRoutingApp {

  import system.dispatcher
  implicit val system = ActorSystem()

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
    val longPoll = actor(new Actor{
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
    })
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

    def regionImageServer = pathPrefix("img") {
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


    def mainFrame = pathPrefix("") (
      extract(_.request.entity.data) ( requestData => ctx =>
        ctx.complete { httpResponse(html.ShellHtml().toString()) }
      )
    )


    def apiRoute(
      prefix: String,
      router: autowire.Core.Router[String]) =
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


    def autowireRoute = apiRoute("autowire", ShellsideServer.route[WatrShellApi](WatrShellApiListeners))

    def run(): Unit = {
      startServer(url, port)(
        get( webjarResources
          ~  assets
          ~  regionImageServer
          ~  mainFrame
        ) ~
        post( path("notifications") (ctx => actors.longPoll ! ctx.responder)
            ~ autowireRoute
        )
      )

    }

    def kill() = system.terminate()
  }

  var activeLabelWidgetIndex: Option[LabelWidgetIndex] = None

  object WatrShellApiListeners extends WatrShellApi {

    // Handle incoming messages from WatrColors:
    def helloShell(msg: String): Unit = {
      println(s"helloShell: $msg")
    }

    def onDrawPath(artifactId: String, path: Seq[Point]): Unit = {
      println(s"onDrawPath: ")
    }

    def uiRequest(r: UIRequest): Future[UIResponse] = {
      activeLabelWidgetIndex.map { lwIndex =>
        val UIRequest(UIState(constraint, maybeLabel, action), gesture) = r
        println(s"got UIRequest ${r}")

        gesture match {
          case SelectRegion(bbox) =>
            maybeLabel.map {label =>
              println("adding label")
              lwIndex.addLabel(bbox, constraint, label)
            }

          case Click(point) =>

        }

        Future{ UIResponse(List()) }
      } getOrElse {
        Future{ UIResponse(List()) }
      }
    }
  }



  object colors {

    val ClientSite  = new ShellsideClient(actors.longPoll)
    val api = ClientSite[WatrColorsApi]

    val labeler = new LabelingServer(reflowDB, corpus)

    def clear(): Unit = {
      api.clear().call()
    }

    def print(level: String, msg: String): Unit = {
      api.print(level, msg).call()
    }


    def echoLabeler(lwidget: LabelingPanel): Unit = {
      val lwIndex = LabelWidgetIndex.create(reflowDB.docstorage, lwidget.content)
      activeLabelWidgetIndex = Some(lwIndex)

      val layout = lwIndex.layout.positioning // .map(p => WidgetPositioning(p.widget, p.widgetBounds, p.id))

      api.echoLabeler(layout, lwidget.options).call()
    }

  }

}
