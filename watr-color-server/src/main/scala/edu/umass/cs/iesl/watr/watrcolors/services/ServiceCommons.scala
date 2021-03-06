package edu.umass.cs.iesl.watr
package watrcolors
package services

import corpora._
import workflow._

import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._
import org.http4s.headers.Location

import _root_.io.circe
import circe._
import circe.syntax._
import circe.literal._

import cats.effect._

import corpora.filesys._


import models._
import persistence._
import tsec.authentication._
// import tsec.cipher.symmetric.imports.AES128
import tsec.cipher.symmetric.imports.{AES128, AES128GCM, SecretKey}
import models.users._
import scala.concurrent.duration._
import corpora.database.CorpusAccessDB

trait AuthenticationHandlers extends Http4sDsl[IO] {

  def corpusAccessDB: CorpusAccessDB
  def userStore: UserStore
  def authStore: PasswordStore
  def tokenStore: TokenStore.StoreType

  val authenticatorSettings = TSecCookieSettings(
    "tsec-auth",
    secure         = false,
    httpOnly       = false,
    expiryDuration = 1.day,
    maxIdle        = Some(8.hour),
    domain         = Option.empty[String],
    path           = Some("/"), // : Option[String] = None,
    extension      = None // : Option[String] = None,
  )

  def symmetricKey = AES128.generateKeyUnsafe()
  implicit val encryptor = AES128GCM.genEncryptor[IO].unsafeRunSync()
  implicit val gcmstrategy = AES128GCM.defaultIvStrategy

  lazy val authenticator: EncryptedCookieAuthenticator[IO, Int, User, AES128] =
    EncryptedCookieAuthenticator.withBackingStore[IO, Int, User, AES128](
      authenticatorSettings,
      tokenStore,
      userStore,
      symmetricKey
    )

  lazy val Auth = WSecureRequestHandler(authenticator)
  // lazy val Auth = FakeAuthHandler


  // lazy val UserAwareService = WSecureRequestHandler(authenticator)
}


trait ServiceCommons extends Http4sDsl[IO] with HttpPayloads { self =>

  def corpusAccessApi: CorpusAccessApi

  lazy val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  lazy val userbaseApi = corpusAccessApi.userbaseApi
  lazy val docStore = corpusAccessApi.docStore
  lazy val corpus: Corpus = corpusAccessApi.corpus
  lazy val annotApi: DocumentAnnotationApi = corpusAccessApi.annotApi
  lazy val corpusLockApi: CorpusLockingApi = corpusAccessApi.corpusLockApi


  object StatusQP extends QueryParamDecoderMatcher[String]("status")

  object StartQP extends OptionalQueryParamDecoderMatcher[Int]("start")
  object LengthQP extends OptionalQueryParamDecoderMatcher[Int]("len")
  object LabelFilterQP extends OptionalQueryParamDecoderMatcher[String]("lbl")
  object ArtifactnameFilterQP extends OptionalQueryParamDecoderMatcher[String]("name")

  def decodeOrErr[T: Decoder](req: Request[IO]): IO[T] = {
    for {
      js   <- req.as[Json]
      decoded <-  IO { Decoder[T].decodeJson(js).fold(fail => {
        println(s"Error decoding: ${js}: ${fail}")
        throw new Throwable(s"error decoding ${js} ${fail}")
      }, mod => mod) }
    } yield decoded
  }

  import cats.effect.IO
  import formdata.LoginForm.LoginError

  def orErrorJson(response: IO[Response[IO]]): IO[Response[IO]] = {
    response.attempt.flatMap { _ match {
      case Left(t: Throwable) => t match {
        case LoginError =>
          TemporaryRedirect(Location(Uri.uri("/")))
          // Uri.fromString("/").fold(
          //   err => throw new Throwable(s"error redirecgin ${err}"),
          //   v => TemporaryRedirect(Location(v)),
          // )

        case other =>
          println(s"server error: ${t}: ${t.getMessage}: ${t.getCause}")
          Ok(Json.obj(
            "server error" := s""
          ))
      }
      case Right(r: Response[IO]) => IO(r)

    }}
  }

}


trait AuthenticatedService extends ServiceCommons with AuthenticationHandlers
