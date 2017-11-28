package edu.umass.cs.iesl.watr
package watrcolors
package server


import org.http4s

import org.http4s._
// import org.http4s.dsl._
// import fs2._
// import fs2.interop.cats._
import org.http4s
import org.http4s._
import org.http4s.dsl._
import cats.effect._
import org.http4s.dsl.io._
import org.http4s.headers._
// import cats.implicits._

import models._

// import ammonite.{ops => fs}

trait CorpusArtifactServices extends ServiceCommons with WorkflowCodecs { self =>
  // http4s.Status.x

  // Mounted at /api/v1/corpus/artifacts
  val corpusArtifactEndpoints = HttpService[IO] {
    case req @ GET -> Root / "entry" / entryId / "image" / "page" / IntVar(pageNum) =>
      val artifactName = s"page-${pageNum}.opt.png"
      val maybeImage = for {
        entry <- corpus.entry(entryId)
        pageImages <- entry.getArtifactGroup("page-images")
        pageImage <- pageImages.getArtifact(artifactName)
        imagePath <- pageImage.asPath.toOption
      } yield {
        println(s"pageImageService: serving page image ${entryId} from ${imagePath}")
        StaticFile.fromFile(imagePath.toIO, Some(req))
          .getOrElse {
            Response(http4s.Status(404, s"could not serve image ${entryId} page ${pageNum}"))
          }
      }
      maybeImage.getOrElse {
        IO.pure{
          Response(http4s.Status(500, s"could not serve image ${entryId} page ${pageNum}"))
        }
      }

    case req @ GET -> Root / "entry" / entryId / "image" / "thumb" / IntVar(pageNum) =>

      val artifactName = s"page-${pageNum}.png"
      val maybeImage = for {
        entry <- corpus.entry(entryId)
        pageImages <- entry.getArtifactGroup("page-thumbs")

        pageImage <- pageImages.getArtifact(artifactName)
        imagePath <- pageImage.asPath.toOption
      } yield {
        StaticFile.fromFile(imagePath.toIO, Some(req))
          .getOrElse {
            Response(http4s.Status(404, s"could not serve image ${entryId} page ${pageNum}"))
          }

      }
      maybeImage.getOrElse {
        IO.pure{
          Response(http4s.Status(500, s"could not serve image ${entryId} page ${pageNum}"))
        }
      }

    case req @ GET -> Root / "vtrace" /  "json" / entryId / jsonArtifact =>

      val maybeResp = for {
        entry <- corpus.entry(entryId)
        traceLogs <- entry.getArtifactGroup("tracelogs")
        artifact <- traceLogs.getArtifact(jsonArtifact)
        artifactPath <- artifact.asPath.toOption
      } yield {
        StaticFile
          .fromFile(artifactPath.toIO, Some(req))
          .getOrElse { Response(http4s.Status(404, s"could not serve ${entryId} artifact ${jsonArtifact}")) }
      }
      maybeResp.getOrElse {
        IO.pure{
          Response(http4s.Status(500, s"could not serve ${entryId} artifact ${jsonArtifact}"))
        }
      }
  }
}
