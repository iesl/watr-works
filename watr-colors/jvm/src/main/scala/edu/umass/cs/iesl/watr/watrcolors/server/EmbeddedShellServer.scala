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
        WatrShellApiListeners
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

  var activeLabelWidgetIndex: Option[LabelWidgetIndex] = None


  object WatrShellApiListeners extends WatrShellApi {

    def onDrawPath(artifactId: String, path: Seq[Point]): Unit = {
      println(s"onDrawPath: ")
    }

    import bioarxiv._

    def createDocumentLabeler(
      stableId: String@@DocumentID,
      labelerType: String
    ): Future[(Seq[WidgetPositioning], LabelOptions)] = {

      val hasEntry = corpus.hasEntry(stableId.unwrap)
      println(s"createDocumentLabeler($stableId, ${labelerType}): hasEntry=$hasEntry")
      val docStore = reflowDB.docStore

      val maybeLabeler = for {
        entry <- corpus.entry(stableId.unwrap)
        rec   <- BioArxivOps.getBioarxivJsonArtifact(entry)
      } yield {


        try {
          val labelingPanel = TitleAuthorsLabelers.bioArxivLabeler(stableId, rec, docStore)

          val withIndicators = labelingPanel.options
            .labels
            .foldLeft(labelingPanel.content){
              case (acc, elemLabel) =>
                LabelWidgetTransforms.addZoneIndicators(elemLabel, acc, docStore)
            }

          val lwIndex = LabelWidgetIndex.create(docStore, withIndicators)

          activeLabelWidgetIndex = Some(lwIndex)
          val layout = lwIndex.layout.positioning
          Future { (layout, labelingPanel.options) }

        } catch {
          case t: Throwable =>
            println(s"error ${t}, ${t.getCause}")
            t.printStackTrace()
            throw t
        }

      }

      maybeLabeler.getOrElse {
        sys.error("createDocumentLabeler: error")
      }

    }

    def uiRequest(r: UIRequest): Future[UIResponse] = {
      val UIRequest(uiState@ UIState(constraint, maybeLabel, selections), gesture) = r
      activeLabelWidgetIndex.map { lwIndex =>
        println(s"got UIRequest ${r}")

        val (uiResponse, modifiedWidget) = lwIndex.userInteraction(uiState, gesture)

        val changes: Option[UIChange] =
          gesture match {
            case SelectRegion(bbox) =>
              maybeLabel.map {label =>
                println(s"adding label to bbox ${bbox}")
                val maybeGeometricGroup = lwIndex.addLabel(bbox, constraint, label)
                // UIAdd()
              }
              None

            case gesture@ Click(point) =>
              println(s"Click ${point}")
              // select the top (selectable) item
              lwIndex.userInteraction(uiState, gesture)

              None
            case DblClick(point) =>
              println(s"DblClick ${point}")
              None
          }

        Future{ uiResponse }
      } getOrElse {
        Future{ UIResponse(uiState, List()) }
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


    def echoLabeler(labelingPanel: LabelingPanel): Unit = {
      val docStore = reflowDB.docStore

      val withIndicators =
        labelingPanel.options
          .labels
          .foldLeft(labelingPanel.content){
            case (acc, elemLabel) =>
              LabelWidgetTransforms.addZoneIndicators(elemLabel, acc, docStore)
          }

      val lwIndex = LabelWidgetIndex.create(docStore, withIndicators)
      activeLabelWidgetIndex = Some(lwIndex)

      val layout = lwIndex.layout.positioning

      api.echoLabeler(layout, labelingPanel.options).call()
    }

  }

}
