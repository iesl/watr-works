package edu.umass.cs.iesl.watr
package watrcolors
package server

import corpora._
import docstore._

// import scalaz._
// import Scalaz._
// import scalaz.concurrent.Task

import org.http4s._
import org.http4s.{headers => H}
import org.http4s.dsl._
// import org.http4s.util.task._
import org.http4s.server._
import org.http4s.server.staticcontent._
// import org.http4s.server.staticcontent.WebjarService.{WebjarAsset, Config}
import org.http4s.server.blaze._
// import org.http4s.server.syntax._
import TypeTags._
import upickle.{default => UPickle}
// import upickle.Js

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable

// import org.reactormonk.{CryptoBits, PrivateKey}
// import java.time._
// import java.io.File

class Http4sService(
  reflowDB: TextReflowDB,
  corpus: Corpus,
  url: String,
  port: Int
) {

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

  // Pages
  val webPageService = HttpService {
    case request @ GET -> Root / pageName =>
      val clientName = pageName match {
        case "browse" => "BrowseCorpus"
        case "label"  => "WatrColors"
      }
      Ok(html.ShellHtml(clientName).toString())
        .putHeaders(
          H.`Content-Type`(MediaType.`text/html`)
        )
  }

  lazy val browseCorpusServer = new BrowseCorpusApiListeners(reflowDB, corpus)
  lazy val bioArxivServer = new BioArxivServer(UserData("TODO", "", ""), reflowDB, corpus)

  import UPicklers._
  // .putHeaders(//   H.`Content-Type`(MediaType.`application/octet-stream`) // )

  val userServers = mutable.HashMap[String, BioArxivServer]()

  val autowireRoutes = HttpService {
    case req @ POST -> "api" /: path =>
      println(s"inside autowire ${path}")
      path.toList.headOption match {
        case Some("browse") =>

          val router = ShellsideServer.route[BrowseCorpusApi](browseCorpusServer)
          Ok {
            req.bodyAsText().map{ body =>
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

          val router = ShellsideServer.route[WatrShellApi](bioArxivServer)
          Ok {
            req.bodyAsText().map{ body =>
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

        case None =>
          ???
      }
  }


  val builder = BlazeBuilder.bindHttp(9999, "localhost")
    .mountService(webJarService)
    .mountService(assetService)
    .mountService(regionImageService, "/img")
    .mountService(autowireRoutes, "/autowire")
    .mountService(webPageService, "/")

  def run(): Server = {
    builder.run
  }
}
