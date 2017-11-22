package edu.umass.cs.iesl.watr
package watrcolors
package server

import corpora._
import workflow._

import org.http4s._
import org.http4s.{headers => H}
import org.http4s.dsl._
import org.http4s.circe._

import _root_.io.circe
import circe._

import cats.implicits._, cats.data._
import cats.effect._

import corpora.{RelationModel => R}
import corpora.filesys._

import geometry._
import watrmarks.Label

trait ServiceCommons extends Http4sDsl[IO] with CirceJsonCodecs { self =>

  def corpusAccessApi: CorpusAccessApi

  lazy val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  lazy val userbaseApi = corpusAccessApi.userbaseApi
  lazy val docStore = corpusAccessApi.docStore
  lazy val corpus: Corpus = corpusAccessApi.corpus


  object UserQP extends QueryParamDecoderMatcher[String]("user")
  object ZoneQP extends QueryParamDecoderMatcher[Int]("zone")
  object StatusQP extends QueryParamDecoderMatcher[String]("status")

  object StartQP extends OptionalQueryParamDecoderMatcher[Int]("start")
  object LengthQP extends OptionalQueryParamDecoderMatcher[Int]("len")

  def okJson(resp: Json): IO[Response[IO]] = {
    for {
      resp <- Ok(resp).putHeaders(H.`Content-Type`(MediaType.`application/json`))
    } yield resp
  }

  //   jsonOf[IO, T].decode(req, strict=true)
  //     .attempt.fold(t => {
  //       t match {
  //         case Left(x) =>
  //           println(s"Error: ${t}")
  //           println(s"Error: ${t.getCause}")
  //           println(s"Error: ${t.getMessage}")
  //         // sys.error(s"${t}")
  //         case Right(y) =>
  //       }
  //     }, ss => ss)

  def decodeOrErr[T: Decoder](req: Request[IO]): IO[T] = {
    jsonOf[IO, T].decode(req, strict=true)
      .attempt.fold(t => {
        println(s"Error: ${t}")
        println(s"Error: ${t.getCause}")
        println(s"Error: ${t.getMessage}")
        sys.error(s"${t}")
      }, ss => {
        sys.error(s"${ss}")
      })
  }

}

trait TypeTagCodecs {
  import circe.generic.semiauto._

  implicit def Enc_IntTypeTags[T]: Encoder[Int@@T] = Encoder.encodeInt.contramap(_.unwrap)
  implicit def Enc_StringTypeTags[T]: Encoder[String@@T] = Encoder.encodeString.contramap(_.unwrap)

}

trait CirceJsonCodecs extends TypeTagCodecs {
  import circe.generic.semiauto._

  implicit val Enc_LTBounds: Encoder[LTBounds] = deriveEncoder

  implicit val Enc_StablePage: Encoder[StablePage] = deriveEncoder
  implicit val Enc_PageRegion: Encoder[PageRegion] = deriveEncoder


  // implicit val Enc_XX: Encoder[XX] = deriveEncoder
  implicit val Enc_Label: Encoder[Label] = Encoder.encodeString.contramap(_.fqn)
  implicit val Dec_Label: Decoder[Label] = Decoder.decodeString.map(Label(_))

  implicit lazy val Enc_Zone: Encoder[Zone] = deriveEncoder

}


trait WorkflowCodecs extends CirceJsonCodecs {
  import circe.generic.semiauto._

  //   implicit val Enc_XX: Encoder[XX] = deriveEncoder
  //   implicit val Dec_XX: Decoder[XX] = deriveDecoder

  lazy implicit val Enc_WorkflowDef: Encoder[R.WorkflowDef] = deriveEncoder

}
