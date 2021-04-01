package org.watrworks
package watrcolors
package services

import corpora._

import org.http4s._
import org.http4s.dsl._
import org.http4s.headers.Location

import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._

import cats.effect._

import corpora.filesys._
import scala.concurrent.duration._

import cats.effect.IO

    // with CirceJsonCodecs with HttpPayloads
trait ServiceCommons extends Http4sDsl[IO] { self =>

  object StatusQP extends QueryParamDecoderMatcher[String]("status")

  object StartQP extends OptionalQueryParamDecoderMatcher[Int]("start")
  object LengthQP extends OptionalQueryParamDecoderMatcher[Int]("len")

  // def decodeOrErr[T: Decoder](req: Request[IO]): IO[T] = {for {
  //     js   <- req.as[Json]
  //     decoded <-  IO { Decoder[T].decodeJson(js).fold(fail => {
  //       println(s"Error decoding: ${js}: ${fail}")
  //       throw new Throwable(s"error decoding ${js} ${fail}")
  //     }, mod => mod) }
  //   } yield decoded
  // }


  // def orErrorJson(response: IO[Response[IO]]): IO[Response[IO]] = {
  //   response.attempt.flatMap { _ match {
  //     case Left(t: Throwable) => t match {
  //       case LoginError =>
  //         TemporaryRedirect(Location(uri("/")))

  //       case other =>
  //         println(s"server error: ${t}: ${t.getMessage}: ${t.getCause}")
  //         Ok(Json.obj(
  //           "server error" := s""
  //         ))
  //     }
  //     case Right(r: Response[IO]) => IO(r)

  //   }}
  // }

}
