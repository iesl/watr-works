package org.watrworks
package watrcolors

// import services._
import cats.effect._
// import org.http4s._
// import org.http4s.server.{ Router, Server }
// import org.http4s.server.blaze._
// import org.http4s.syntax.all._
// import scala.concurrent.ExecutionContext.global

object WatrTableApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = ???
    // WatrTableServer.resource[IO].use(_ => IO.never).as(ExitCode.Success)
}

object WatrTableServer {

  // def httpApp[F[_]: Effect: ContextShift: Timer](blocker: Blocker): HttpApp[F] =
  //   Router(
  //     "" -> TableService[F](blocker).routes
  //   ).orNotFound

  // def resource[F[_]: ConcurrentEffect: ContextShift: Timer]: Resource[F, Server[F]] =
  //   for {
  //     blocker <- Blocker[F]
  //     app = httpApp[F](blocker)
  //     server <- BlazeServerBuilder[F](global)
  //                 .bindHttp(8080)
  //                 .withHttpApp(app)
  //                 .withBanner(List("WatrTable Background Server"))
  //                 .resource
  //   } yield server
}
