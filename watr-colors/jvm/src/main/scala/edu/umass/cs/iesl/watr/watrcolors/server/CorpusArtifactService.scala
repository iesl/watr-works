package edu.umass.cs.iesl.watr
package watrcolors
package server

// import scalaz.Kleisli
// import corpora._
// import docstore._

import org.http4s._
import org.http4s.util._
// import org.http4s.{headers => H}
// import org.http4s.dsl._
// import org.http4s.server._
// import org.http4s.server.syntax._
import org.http4s.server.staticcontent._
// import org.http4s.server.blaze._

// import scala.concurrent._
import java.io.File
import java.util.concurrent.ExecutorService

import org.http4s.headers.{`Content-Range`, Range}
import org.http4s.headers.Range.SubRange

import scalaz.concurrent.Task
import org.http4s.util._
// import org.http4s.util.threads.DefaultPool
import org.http4s.headers.`Accept-Ranges`
import scalaz.{Kleisli}



object CorpusArtifactService {

  private val AcceptRangeHeader = `Accept-Ranges`(RangeUnit.Bytes)
  // Will strip the pathPrefix from the first part of the Uri, returning the remainder without a leading '/'
  private def getSubPath(uriPath: String, pathPrefix: String): String = {
    val index = pathPrefix.length + {
      if (uriPath.length > pathPrefix.length &&
        uriPath.charAt(pathPrefix.length) == '/') 1
      else 0
    }

    uriPath.substring(index)
  }
  private val sanitize = "\\.\\.".r.replaceAllIn(_: String, ".")
  /** [[org.http4s.server.staticcontent.FileService]] configuration
    *
    * @param systemPath path prefix to the folder from which content will be served
    * @param pathPrefix prefix of Uri from which content will be served
    * @param pathCollector function that performs the work of collecting the file or rendering the directory into a response.
    * @param bufferSize buffer size to use for internal read buffers
    * @param executor `ExecutorService` to use when collecting content
    * @param cacheStrategy strategy to use for caching purposes. Default to no caching.
    */
  final case class Config(systemPath: String,
                          pathPrefix: String = "",
                          pathCollector: (File, Config, Request) => Task[Option[Response]] = filesOnly,
                          bufferSize: Int = 50*1024,
                          executor: ExecutorService,
                          cacheStrategy: CacheStrategy = NoopCacheStrategy)


  private def apply(config: Config): HttpService = {

    Kleisli.kleisli(
      (req: Request) =>{  val uriPath = req.pathInfo
        if (!uriPath.startsWith(config.pathPrefix))
          Pass.now
        else
          getFile(config.systemPath + '/' + getSubPath(uriPath, config.pathPrefix))
            .map { f => config.pathCollector(f, config, req) }
            .getOrElse(Task.now(None))
            .flatMap(_.fold(Pass.now)(config.cacheStrategy.cache(uriPath, _)))
      }
    )


  }
  /* Returns responses for static files.
   * Directories are forbidden.
   */
  private def filesOnly(file: File, config: Config, req: Request): Task[Option[Response]] = Task.now {
    if (file.isDirectory()) Some(Response(Status.Unauthorized))
    else if (!file.isFile) None
    else getPartialContentFile(file, config, req) orElse
      StaticFile.fromFile(file, config.bufferSize, Some(req))(config.executor)
                .map(_.putHeaders(AcceptRangeHeader))
  }

  private def validRange(start: Long, end: Option[Long], fileLength: Long): Boolean = {
    start < fileLength && (end match {
      case Some(end) => start >= 0 && start <= end
      case None      => start >= 0 || fileLength + start - 1 >= 0
    })
  }

  // Attempt to find a Range header and collect only the subrange of content requested
  private def getPartialContentFile(file: File, config: Config, req: Request): Option[Response] = req.headers.get(Range).flatMap {
    case Range(RangeUnit.Bytes, NonEmptyList(SubRange(s, e))) if validRange(s, e, file.length) =>
      val size = file.length()
      val start = if (s >= 0) s else math.max(0, size + s)
      val end = math.min(size - 1, e getOrElse (size - 1))  // end is inclusive

      StaticFile .fromFile(file, start, end + 1, config.bufferSize, Some(req))(config.executor)
                  .map { resp =>
                    val hs = resp.headers.put(AcceptRangeHeader, `Content-Range`(SubRange(start, end), Some(size)))
                    resp.copy(status = Status.PartialContent, headers = hs)
                  }

    case _ => None
  }

  // Attempts to sanitize the file location and retrieve the file. Returns None if the file doesn't exist.
  private def getFile(unsafePath: String): Option[File] = {
    val f = new File(sanitize(unsafePath))
    if (f.exists()) Some(f)
    else None
  }
}
