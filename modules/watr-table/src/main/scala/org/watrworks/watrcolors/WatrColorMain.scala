package org.watrworks
package watrcolors

import services._
import cats.effect._
import org.http4s.HttpApp
import org.http4s.server.{Router, Server}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.syntax.kleisli._
import scala.concurrent.ExecutionContext.global

object WatrTableApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    resource[IO].use(_ => IO.never).as(ExitCode.Success)

  def httpApp[F[_]: Effect: ContextShift: Timer](blocker: Blocker): HttpApp[F] =
    Router(
      "/http4s" -> TableService[F](blocker).routes
    ).orNotFound

  def resource[F[_]: ConcurrentEffect: ContextShift: Timer]: Resource[F, Server[F]] =
    for {
      blocker <- Blocker[F]
      app = httpApp[F](blocker)
      server <- BlazeServerBuilder[F](global)
                  .bindHttp(8080)
                  .withHttpApp(app)
                  .resource
    } yield server
}


// trait AllTheServices extends HtmlPageService
//   with CorpusListingServices
//   with CorpusArtifactServices

//   implicit val cs: ContextShift[IO] = IO.contextShift(global)
//   implicit val timer: Timer[IO]     = IO.timer(global)

//   //   BlazeBuilder[IO]
//   //     .bindHttp(portNum, "localhost")
//   //     .mountService(assetService, "/assets")
//   //     .mountService(jslibDistService, "/dist")
//   //     .mountService(corpusArtifactEndpoints, "/api/v1/corpus/artifacts")
//   //     .mountService(corpusListingEndpoints, "/api/v1/corpus/entries")
//   //     .mountService(htmlPageServices, "/")
//   // }
// }
