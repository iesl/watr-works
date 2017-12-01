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

import cats.implicits._
import cats.effect._

import corpora.filesys._


import models._
import persistence._
import tsec.authentication._
import tsec.cipher.symmetric.imports.AES128
import models.users._
import scala.concurrent.duration._

trait AuthenticationHandlers extends Http4sDsl[IO] {
  def userStore: UserStore
  def authStore: PasswordStore
  def tokenStore: TokenStore.StoreType

  val authenticatorSettings = TSecCookieSettings(
    "tsec-auth",
    secure         = false,
    httpOnly       = false,
    expiryDuration = 1.day,
    maxIdle        = Some(1.hour),
    domain         = None, // Some("localhost") , // : Option[String] = None,
    path           = Some("/"), // : Option[String] = None,
    extension      = None // : Option[String] = None,
  )

  def symmetricKey = AES128.generateKeyUnsafe()

  lazy val authenticator: EncryptedCookieAuthenticator[IO, Int, User, AES128] =
    EncryptedCookieAuthenticator.stateless[IO, Int, User, AES128](
      authenticatorSettings,
      // tokenStore,
      userStore,
      symmetricKey
    )

  lazy val Auth = SecuredRequestHandler(authenticator)
}

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
    Ok(resp)
      .map { _.putHeaders(H.`Content-Type`(MediaType.`application/json`)) }
  }

  def decodeOrErr[T: Decoder](req: Request[IO]): IO[T] = {
    val res = jsonOf[IO, T].decode(req, strict=true)
      .attempt.fold(t => {
        println(s"Error: ${t}")
        println(s"Error: ${t.getCause}")
        println(s"Error: ${t.getMessage}")
        sys.error(s"${t}")
      }, ss => {
        sys.error(s"${ss}")
      })
    res
  }

}


trait AuthenticatedService extends ServiceCommons with AuthenticationHandlers
