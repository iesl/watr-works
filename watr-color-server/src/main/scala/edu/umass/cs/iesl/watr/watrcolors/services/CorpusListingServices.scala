package edu.umass.cs.iesl.watr
package watrcolors
package services

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl._
import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._
import cats.effect._

import models._

trait CorpusListingServices extends Http4sDsl[IO] with ServiceCommons with WorkflowCodecs { self =>
  // Mounted at /api/v1xx/corpus/entries/..


  val corpusListingEndpoints = HttpService[IO] {

    case req @ GET -> Root :? StartQP(start) +& LengthQP(len) =>

      val skip = start.getOrElse(0)
      val get = len.getOrElse(100)

      val docCount = docStore.getDocumentCount()

      val entries = (for {
        (stableId, i) <- docStore.getDocuments(get, skip).zipWithIndex
        docId <- docStore.getDocument(stableId)
      } yield {
        val annots = annotApi.listDocumentAnnotations(docId)
        val annotCount = annots.length

        Json.obj(
          "num" := skip+i,
          "stableId" := stableId.unwrap,
          "labelCount" := annotCount
        )

      }).asJson

      Ok(
        Json.obj(
          "corpusSize" := Json.fromInt(docCount),
          "entries" := entries,
          "start" := Json.fromInt(skip)
        )
      )
  }
}
