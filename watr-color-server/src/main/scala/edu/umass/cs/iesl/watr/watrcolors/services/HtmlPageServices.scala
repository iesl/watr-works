package edu.umass.cs.iesl.watr
package watrcolors
package services



import cats._
import cats.data._
import cats.effect._
import org.http4s.headers.Location
// import cats.implicits._
import org.http4s._
import org.http4s.{headers => H}
import html._
import tsec.authentication._
import cats.syntax.all._

// class HtmlPageService[F[_]: Effect: ContextShift] extends AuthenticatedService {
// class HtmlPageService[F[_]](implicit F: Effect[F], cs: ContextShift[F]) extends AuthenticatedService {
trait HtmlPageService extends AuthenticatedService {

  implicit def scalatagsEncoder[SE[_]: EntityEncoder[?[_], String]: Applicative]: EntityEncoder[SE, TextTag] = {
    EntityEncoder.stringEncoder[SE]
      .contramap[TextTag] { _.toString() }
      .withContentType(H.`Content-Type`(MediaType.text.html, DefaultCharset))
  }

  private val unauthedPages: HttpRoutes[IO] = HttpRoutes.of {
    case req @ GET -> Root / "login" =>
      // Ok(Authentication.loginForm())
      Ok()
  }

  private val authedPages = Auth {
    case req @ (GET | POST) -> Root asAuthed user =>
      Ok(Frame.empty())

    case req @ GET -> Root / "document" / stableId asAuthed user =>
      Ok(Frame.empty())

    case req @ GET -> Root / "curate"  asAuthed user =>
      Ok(Frame.empty())
  }

  val authedOrLogin = authedPages.andThen( resp => {
    // TODO overriding login pages
    OptionT.liftF{ IO(resp) }
    // if (resp.status == Status.Unauthorized) {
    //   OptionT.liftF{ TemporaryRedirect(Location(uri("/login"))) }
    // } else {
    //   OptionT.liftF{ IO(resp) }
    // }
  })

  def htmlPageServices: HttpRoutes[IO] = unauthedPages <+> authedOrLogin


}
