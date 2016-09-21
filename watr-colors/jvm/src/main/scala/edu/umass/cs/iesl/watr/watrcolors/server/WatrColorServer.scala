package edu.umass.cs.iesl.watr
package watrcolors
package server

import akka.util.ByteString
import java.nio.ByteBuffer
import spray.http.HttpData
import spray.routing.SimpleRoutingApp
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext.Implicits.global
import spray.http.{MediaTypes, HttpEntity}

import boopickle.DefaultBasic._
import Picklers._
import ammonite.ops.{Path => FPath, _}

object AutowireServer extends autowire.Server[ByteBuffer, Pickler, Pickler] {
  override def read[R: Pickler](p: ByteBuffer) = Unpickle[R].fromBytes(p)
  override def write[R: Pickler](r: R) = Pickle.intoBytes(r)
}

object WatrColorServer {
  def main(args: Array[String]): Unit = {
    // val conf = configuration.getPdfCorpusConfig(args(0))
    val corpusRoot = pwd/RelPath(args(0))
    println(s"WatrColorServer: root = ${corpusRoot}")
    val server = new WatrColorServer(corpusRoot)
    server.run()
  }
}


class WatrColorServer(
  rootDirectory: FPath
) extends SimpleRoutingApp {

  val svgRepoPath = rootDirectory

  def svgResponse(resp: String) = {
    HttpEntity(MediaTypes.`image/svg+xml`, resp)
  }

  def jsonResponse(resp: String) = {
    HttpEntity(MediaTypes.`application/json`, resp)
  }

  def httpResponse(resp: String) = {
    HttpEntity(MediaTypes.`text/html`, resp)
  }

  def webjarResources = pathPrefix("webjars") {
    getFromResourceDirectory("META-INF/resources/webjars")
  }

  def assets = pathPrefix("assets") {
    getFromResourceDirectory("")
  }

  def svgRepo = pathPrefix("repo") {
    getFromDirectory(svgRepoPath.toIO.getPath)
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


  def run(): Unit = {

    // val corpusExplorerServer = new CorpusExplorerServer(rootDirectory)
    val svgOverviewServer = new SvgOverviewServer(rootDirectory)
    // val tableServer = new WatrTableServer(rootDirectory)

    implicit val system = ActorSystem()

    val _ = startServer("0.0.0.0", port = 8080) {
      get {
        webjarResources ~
        svgRepo ~
        assets ~
        pathPrefix("") {
          extract(_.request.entity.data) { requestData => ctx =>
            // val uri = ctx.request.uri
            // val q = uri.query
            // val frag = uri.fragment
            ctx.complete { httpResponse(html.Frame().toString()) }
          }
        }
      } ~
      post {
        apiRoute("svg", AutowireServer.route[SvgOverviewApi](svgOverviewServer))
        // apiRoute("table", AutowireServer.route[WatrTableApi](tableServer)) ~
        // apiRoute("explorer", AutowireServer.route[CorpusExplorerApi](corpusExplorerServer)) ~
      }
    }
  }



}
