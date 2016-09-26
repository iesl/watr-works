package edu.umass.cs.iesl.watr
package watrcolors
package server


import akka.actor.{ActorRef, Actor, ActorSystem}
import akka.util.ByteString
import spray.httpx.encoding.Gzip
import spray.routing.SimpleRoutingApp
import spray.http.HttpData
import spray.http.{MediaTypes, HttpEntity}
import akka.actor.ActorDSL._

import spray.http.{HttpEntity, AllOrigins, HttpResponse}
import spray.http.HttpHeaders.`Access-Control-Allow-Origin`
import spray.http.ContentType
import concurrent.duration._
import scala.concurrent.Future
import scala.io.Source
import scala.tools.nsc
import scala.tools.nsc.Settings

import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.collection.mutable
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.util.{JavaClassPath, DirectoryClassPath}
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import autowire._
import boopickle.DefaultBasic._
import java.nio.ByteBuffer


class Server(url: String, port: Int) extends SimpleRoutingApp with WatrTableApi with RemoteCallPicklers {
  val corsHeaders: List[ModeledHeader] =
    List(
      `Access-Control-Allow-Methods`(OPTIONS, GET, POST),
      `Access-Control-Allow-Origin`(AllOrigins),
      `Access-Control-Allow-Headers`("Origin, X-Requested-With, Content-Type, Accept, Accept-Encoding, Accept-Language, Host, Referer, User-Agent"),
      `Access-Control-Max-Age`(1728000)
    )

  implicit val system = ActorSystem()
  import system.dispatcher

  val api = Wire[WatrTableApi]

  def clear(): Unit = {
    api.clear().call()
  }

  def print(level: String, msg: String): Unit = {
    api.print(level, msg).call()
  }


  object Wire extends autowire.Client[ByteBuffer, Pickler, Pickler] {
    def doCall(req: Request): Future[ByteBuffer] = {

      val rc = RemoteCall(
        req.path,
        req.args.toSeq.map(p => (p._1, p._2.array()))
      )

      longPoll ! rc

      val pUnit = Pickle.intoBytes(())
      Future.successful(pUnit)
    }

    override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)
    override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)
  }
  /**
   * Actor meant to handle long polling, buffering messages or waiting actors
   */
  private val longPoll = actor(new Actor{
    var waitingActor: Option[ActorRef] = None
    // var queuedMessages = List[ByteBuffer]()
    var queuedMessages = List[RemoteCall]()

    /**
     * Flushes returns nothing to any waiting actor every so often,
     * to prevent the connection from living too long.
     */
    case object Clear

    system.scheduler.schedule(0.seconds, 10.seconds, self, Clear)

    def respond(a: ActorRef, s: ByteBuffer) = {
      val respEntity = HttpEntity(HttpData(ByteString(s)))
      a ! HttpResponse(
        entity =respEntity,
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

      // case (msg: ByteBuffer, None, msgs) =>
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
              Unpickle[Map[String, ByteBuffer]].fromBytes(requestData.toByteString.asByteBuffer)
            )
          ).map(responseData =>
            HttpEntity(HttpData(ByteString(responseData)))
          ))}}
  }

  /**
   * Spray server:
   * - POSTs to /notifications get routed to the longPoll actor
   */
  startServer(url, port) {
    get {
      webjarResources ~
      assets ~
      pathPrefix("") {
        extract(_.request.entity.data) { requestData => ctx =>
            ctx.complete { httpResponse(html.ShellHtml().toString()) }
        }
      }
    } ~
    post {
      path("notifications") { ctx =>
        longPoll ! ctx.responder
      }
    }
  }

  def kill() = system.terminate()

}

