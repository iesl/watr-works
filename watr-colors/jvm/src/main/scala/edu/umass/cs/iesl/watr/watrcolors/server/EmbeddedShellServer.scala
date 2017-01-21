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

import concurrent.duration._
import scala.concurrent.Future

import corpora._
import textreflow._
import geometry._
import display._

import autowire._
import upickle.{default => UPickle}
import UPickle._
import TypeTagPicklers._



object ClientSiteConn {
  type Conn[T] = ClientProxy[T, String, Reader, Writer]
}

class ClientSiteConn(pollActor: ActorRef)
    extends autowire.Client[String, UPickle.Reader, UPickle.Writer] {

  override def doCall(req: Request)= {
    val reqArgs = req.args.toList.map({case (param, arg) =>
      (param -> arg)
    })

    val rc = RemoteCall(req.path.toList, reqArgs)

    pollActor ! rc

    Future.successful("")
  }

  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
}
class EmbeddedServer(
  reflowDB: TextReflowDB, corpus: Corpus, url: String, port: Int
) extends SimpleRoutingApp
    with WatrTableApi {


  implicit val system = ActorSystem()
  import system.dispatcher
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
  private val longPoll = actor(new Actor{
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

  def webjarResources = pathPrefix("webjars") {
    getFromResourceDirectory("META-INF/resources/webjars")
  }

  def assets = pathPrefix("assets") {
    getFromResourceDirectory("")
  }

  def httpResponse(resp: String) = {
    HttpEntity(MediaTypes.`text/html`, resp)
  }

  // import geometry._

  def producePageImage(path: List[String]): Array[Byte] = {
    println(s"producePageImage: ${path}")

    val uriPath = path.headOption.getOrElse { sys.error("producePageImage: no path specified") }

    reflowDB
      .serveImageWithURI(TargetRegion.fromUri(uriPath))
      .bytes
      // .map(_.bytes)
      // .getOrElse { sys.error("producePageImage: no images found")}
  }

  def pageImageServer = pathPrefix("img")(
    path(Segments)(pathSegments =>
      complete(
        HttpResponse(entity =
          HttpEntity(MediaTypes.`image/png`, HttpData(producePageImage(pathSegments)))
        )
      )
    )
  )

  def mainFrame = pathPrefix("") (
    extract(_.request.entity.data) ( requestData => ctx =>
      ctx.complete { httpResponse(html.ShellHtml().toString()) }
    )
  )

  // Helper function for constructing Autowire routes
  def apiRoute(
    prefix: String,
    router: autowire.Core.Router[String] // (which expands to)  PartialFunction[autowire.Core.Request[java.nio.ByteBuffer],scala.concurrent.Future[java.nio.ByteBuffer]]
  ) = pathPrefix("api")(
    path(prefix / Segments) { segs =>
      extract(_.request.entity.data) { requestData => ctx =>
        ctx.complete(
          router({
            val reqStr = requestData.asString
            val argmap = UPickle.read[Map[String, String]](reqStr)
            autowire.Core.Request(segs, argmap)
          }).map(responseData =>
            HttpEntity(HttpData(ByteString(responseData)))
          )
        )
      }
    }
  )


  startServer(url, port)(
    get( webjarResources
      ~  assets
      ~  pageImageServer
      ~  mainFrame
    ) ~ post(
      path("notifications") ( ctx =>
        longPoll ! ctx.responder
      )
    )
  )

  def kill() = system.terminate()

  val ClientSite  = new ClientSiteConn(longPoll)

  val labeler = new LabelingServer(reflowDB, corpus)

  val api = ClientSite[WatrTableApi]

  def clear(): Unit = {
    api.clear().call()
  }

  def print(level: String, msg: String): Unit = {
    api.print(level, msg).call()
  }

  def echoTextReflows(textReflows: List[TextReflow]): Unit = {
    api.echoTextReflows(textReflows).call()
  }

  def echoLabeler(lwidget: LabelWidget): Unit = {
    import matryoshka._
    import matryoshka.data._
    import matryoshka.implicits._

    import LabelWidgetF._

    /// pre-create target region images w/embossings
    def visit(t: LabelWidgetF[Unit]): Unit = t match {
      case Target(tr, emboss) =>
        // pre-create the images w/labels embossed as color overlays and put them in database
        labeler.embossTargetRegion(tr, emboss)

      case _ => ()
    }

    // side-effecting...
    lwidget.cata(visit)

    api.echoLabeler(lwidget).call()
  }

}
