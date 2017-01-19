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

import autowire._
import upickle.{default => UPickle}
import UPickle._
import TypeTagPicklers._


class EmbeddedServer(reflowDB: TextReflowDB, corpus: Corpus, url: String, port: Int)
    extends SimpleRoutingApp
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

  object ClientSite extends autowire.Client[String, UPickle.Reader, UPickle.Writer] {

    override def doCall(req: Request)= {
      val reqArgs = req.args.toList.map({case (param, arg) =>
        (param -> arg)
      })

      val rc = RemoteCall(req.path.toList, reqArgs)

      longPoll ! rc

      Future.successful("")
    }

    override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
    override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)

  }

  val api = ClientSite[WatrTableApi]

  def clear(): Unit = {
    api.clear().call()
  }

  def print(level: String, msg: String): Unit = {
    api.print(level, msg).call()
  }

  def echoTextReflow(textReflow: TextReflow): Unit = {
    api.echoTextReflow(textReflow).call()
  }

  def echoTextReflows(textReflows: List[TextReflow]): Unit = {
    api.echoTextReflows(textReflows).call()
  }

  def echoTargetRegion(tr: TargetRegion): Unit = {
    api.echoTargetRegion(tr).call()
  }

  def echoDouble(d: Double): Unit = {
    api.echoDouble(d).call()
  }

  def imageScratch(): Unit = {

    // import java.awt.{AlphaComposite, Graphics2D}
    // val maskImage = Image(w, h)
    //Rect(x: Int, y: Int, width: Int, height: Int)
    // val g2: Graphics2D => Unit = { g2 =>
    //   g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC))
    //   g2.setColor(Color.Black)
    // }
    //   Line(100, 100, 120, 120)
    // import scrimage.canvas

    // import scrimage.composite.AlphaComposite
    // import java.awt.Graphics2D
    // import java.awt
    // import scrimage.composite.OverlayComposite

    // val g2: Graphics2D => Unit = { g2 =>
    //   g2.setComposite(awt.AlphaComposite.getInstance(awt.AlphaComposite.SRC))
    //   g2.setColor(Color.Black)
    // }

  }

  def rescale(bbox: LTBounds, page1: LTBounds, page2: LTBounds): LTBounds = {
    val LTBounds(l, t, w, h) = bbox


    val scaleX = page2.width/page1.width
    val scaleY = page2.height/page1.height
    val l2 = l * scaleX
    val t2 = t * scaleY
    val w2 = w * scaleX
    val h2 = h * scaleY
    val res = LTBounds(l2, t2, w2, h2)
    // println(s"rescaling ${bbox}/[$page1] => x/ [$page2] ==> scalex=${scaleX}, scaley=${scaleY} ")
    // println(s"   $res")
    res
  }

  def showPageImage(docId: String@@DocumentID, pagenum: Int): Unit = {
    val pageId = PageID(pagenum)
    val (pageImage, pageGeometry) = reflowDB.getPageImageAndGeometry(docId, pageId)
    val pageTargetRegion = TargetRegion(RegionID(0), docId, pageId, pageGeometry.bounds)

    showTargetRegion(pageTargetRegion, LB.VisualLine)
  }

  def showTargetRegion(targetRegion: TargetRegion, label: watrmarks.Label): Unit = {

    import com.sksamuel.scrimage
    import scrimage._
    import scrimage.canvas._

    val docId = targetRegion.docId
    val pageId = targetRegion.pageId

    val (pageImage, pageGeometry) = reflowDB.getPageImageAndGeometry(docId, pageId)

    // select all zones w/label on given page
    val zones = reflowDB.selectZones(docId, pageId, label)
    val (w, h) = pageImage.dimensions
    val pageImageBounds = LTBounds(0, 0, w.toDouble, h.toDouble)
    println(s"showTargetRegion: pageImage has dims ${w}, $h = ${pageImageBounds}")

    val blank = Image.filled(w, h, Color(0, 0, 0, 20))
    val maskCanvas = new Canvas(blank)

    val zoneRects = for { zone <- zones } yield {
      zone.regions.map({
        case TargetRegion(id, docId, pageId, bbox @ LTBounds(l, t, w, h)) =>
          val re @ LTBounds(rl, rt, rw, rh) = rescale(bbox, pageGeometry.bounds, pageImageBounds)
          val ctx = Context.painter(Color(10, 10, 220, 40))
          val r = Rect(rl.toInt, rt.toInt, rw.toInt, rh.toInt, ctx)
          // println(s"adding rescaled rect ${re}")
          r.fill
      })
    }

    val rectsCanvas = maskCanvas.draw(zoneRects.flatten)

    val maskImage = rectsCanvas.image

    val overlay = pageImage.overlay(maskImage, 0, 0)

    val pageTargetRegion = TargetRegion(RegionID(0), docId, pageId, pageGeometry.bounds)
    // val labelUri = pageUri + "?l=" + label.fqn

    reflowDB.putTargetRegionImage(pageTargetRegion, overlay)

    api.showTargetRegion(pageTargetRegion, label).call()
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

  import geometry._

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

}
