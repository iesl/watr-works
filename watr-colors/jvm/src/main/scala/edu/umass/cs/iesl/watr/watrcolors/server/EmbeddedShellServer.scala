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
import autowire._
import boopickle.DefaultBasic._
import java.nio.ByteBuffer
import java.nio.ByteOrder

import extract.images._
import corpora._
import textreflow._
import geometry._

class EmbeddedServer(corpus: Corpus, url: String, port: Int)
    extends SimpleRoutingApp
    with WatrTableApi
    with TextReflowBoopicklers
{

  implicit val system = ActorSystem()
  import system.dispatcher
  val corsHeaders: List[ModeledHeader] =
    List(
      `Access-Control-Allow-Methods`(OPTIONS, GET, POST),
      `Access-Control-Allow-Origin`(AllOrigins),
      `Access-Control-Allow-Headers`("Origin, X-Requested-With, Content-Type, Accept, Accept-Encoding, Accept-Language, Host, Referer, User-Agent"),
      `Access-Control-Max-Age`(1728000)
    )

  object ClientSite extends autowire.Client[ByteBuffer, Pickler, Pickler] {
    val pUnit = Pickle.intoBytes(())

    def doCall(req: Request): Future[ByteBuffer] = {
      val reqArgs = req.args.toList.map({case (param, arg) =>
        // val iarr = value.array.map(_.toInt).mkString(", ")
        // println(s"  [$iarr]")
        val value = arg.order(ByteOrder.LITTLE_ENDIAN)

        println(s"  Client byte order: ${arg.order}/${value.order}")

        val data = Array.ofDim[Byte](value.remaining())
        val _ = value.get(data)
        // val array = new Array[Byte](len)
        // buf.get(array)
        (param -> data)
      })

      val rc = RemoteCall(req.path.toList, reqArgs)

      // val rc = RemoteCall(
      //   req.path.toList,
      //   req.args.toList.map(p => (p._1, p._2.array()))
      // )

      longPoll ! rc

      Future.successful(pUnit)
    }

    override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
    override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)
  }

  val api = ClientSite[WatrTableApi]

  def clear(): Unit = {
    api.clear().call()
  }

  def print(level: String, msg: String): Unit = {
    api.print(level, msg).call()
  }

  def echo(textReflow: TextReflow): Unit = {
    api.echo(textReflow).call()
  }

  def echo2(textReflows: List[TextReflow]): Unit = {
    api.echo2(textReflows).call()
  }

  def echoTargetRegion(tr: TargetRegion): Unit = {
    api.echoTargetRegion(tr).call()
  }

  def echoDouble(d: Double): Unit = {
    api.echoDouble(d).call()
  }

  def echoLTBounds(bbox: LTBounds): Unit = {
    api.echoLTBounds(bbox).call()
  }

  def echoCharAtom(charAtom: CharAtom): Unit = {
    api.echoCharAtom(charAtom).call()
  }

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

    def respond(a: ActorRef, s: ByteBuffer) = {
      val respEntity = HttpEntity(
        MediaTypes.`application/octet-stream`,
        HttpData(ByteString(s))
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
        val bs = Pickle.intoBytes(msgs)
        respond(a, bs)
        queuedMessages = Nil

      case (msg: RemoteCall, None, msgs) =>
        println(s"""receive: msg enqueue""")
        queuedMessages = msg :: msgs

      case (msg: RemoteCall, Some(a), Nil) =>
        println(s"""receive: waiting actor gets msg""")
        val bs = Pickle.intoBytes(List(msg))
        respond(a, bs)
        waitingActor = None

      case (Clear, waitingOpt, msgs) =>
        // println(s"""receive: Clear""")
        val bs = Pickle.intoBytes(msgs)
        waitingOpt.foreach(respond(_, bs))
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

  import geometry._

  def producePageImage(path: List[String]): Array[Byte] = {
    println(s"producePageImage: ${path}")

    val uriPath = path.headOption.getOrElse { sys.error("producePageImage: no path specified") }
    // s"${doc}+${pg}+${bbox}"
    val TargetRegion(id, docId, pageId, bbox) = TargetRegion.fromUri(uriPath)

    val imagesOpt = for {
      entry <- corpus.entry(docId.unwrap)
      group <- entry.getArtifactGroup("page-images")
    } yield {
      ExtractImages.load(group.rootPath)
    }

    val images = imagesOpt.getOrElse { sys.error("producePageImage: no images found")}

    val bytes = images.pageBytes(pageId)

    bytes
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
    router: autowire.Core.Router[java.nio.ByteBuffer] // (which expands to)  PartialFunction[autowire.Core.Request[java.nio.ByteBuffer],scala.concurrent.Future[java.nio.ByteBuffer]]
  ) = pathPrefix("api") {
    path(prefix / Segments) { segs =>
      extract(_.request.entity.data) { requestData => ctx =>
        ctx.complete(
          router(
            autowire.Core.Request(segs,
              Unpickle[Map[String, ByteBuffer]]
                .fromBytes(requestData.toByteString.asByteBuffer.order(ByteOrder.LITTLE_ENDIAN))
            )
          ).map(responseData =>
            HttpEntity(HttpData(ByteString(responseData)))
          ))}}
  }


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

}
