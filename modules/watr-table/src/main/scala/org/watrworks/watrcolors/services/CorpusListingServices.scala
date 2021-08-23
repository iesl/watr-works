package org.watrworks
package watrcolors
package services

import org.http4s
import http4s._
import http4s.dsl._
import http4s.circe._
import _root_.io.circe
import circe._
// import circe.syntax._
// import circe.literal._
// import cats.effect._

// import _root_.io.circe
import cats.effect._




trait CorpusListingServices extends Http4sDsl[IO] with ServiceCommons  { self =>
  // Mounted at /api/v1xx/corpus/entries/..


  val corpusListingEndpoints = ???
  //       HttpService[IO] {

  //   case req @ GET -> Root :? StartQP(start) +& LengthQP(len) =>

  //     val skip = start.getOrElse(0)
  //     val get = len.getOrElse(100)

  //     // val docCount = docStore.getDocumentCount()

  //     // val entries = (for {
  //     //   (stableId, i) <- docStore.getDocuments(get, skip).zipWithIndex
  //     // } yield {

  //     //   val docLabels = (for {
  //     //     docId <- docStore.getDocument(stableId).toList
  //     //   } yield for {
  //     //     labelId <- docStore.getZoneLabelsForDocument(docId)
  //     //   } yield {
  //     //     docStore.getLabel(labelId).asJson
  //     //   }).asJson

  //     //   Json.obj(
  //     //     ("num", Json.fromInt(skip+i)),
  //     //     ("stableId", Json.fromString(stableId.unwrap)),
  //     //     ("labels", docLabels)
  //     //   )

  //     // }).asJson

  //     Ok(
  //       Json.obj(
  //         // ("corpusSize", Json.fromInt(docCount)),
  //         // ("entries", entries),
  //         ("start", Json.fromInt(skip))
  //       )
  //     )
  // }
}
