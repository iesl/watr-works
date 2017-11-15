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

import corpora.{RelationModel => R}
import corpora.filesys._

import geometry._
import watrmarks.Label

trait ServiceCommons extends CirceJsonCodecs { self =>

  def corpusAccessApi: CorpusAccessApi

  lazy val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  lazy val userApi = corpusAccessApi.userbaseApi
  lazy val docStore = corpusAccessApi.docStore
  lazy val corpus: Corpus = corpusAccessApi.corpus


  object UserQP extends QueryParamDecoderMatcher[String]("user")
  object ZoneQP extends QueryParamDecoderMatcher[Int]("zone")
  object StatusQP extends QueryParamDecoderMatcher[String]("status")

  object StartQP extends OptionalQueryParamDecoderMatcher[Int]("start")
  object LengthQP extends OptionalQueryParamDecoderMatcher[Int]("len")

  def okJson(resp: Json): fs2.Task[Response] = {
    for {
      resp <- Ok(resp).putHeaders(H.`Content-Type`(MediaType.`application/json`))
    } yield resp
  }

  def decodeOrErr[T: Decoder](req: Request): fs2.Task[T] = {
    req.as(jsonOf[T]).attemptFold(t => {
      println(s"Error: ${t}")
      println(s"Error: ${t.getCause}")
      println(s"Error: ${t.getMessage}")
      sys.error(s"${t}")
    }, ss => ss)
  }

}

trait TypeTagCodecs {
  // import circe.generic.semiauto._

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
