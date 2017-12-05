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


trait HtmlPageService extends AuthenticatedService {

  implicit def scalatagsEncoder[F[_]: EntityEncoder[?[_], String]: Applicative]: EntityEncoder[F, TextTag] = {
    EntityEncoder.stringEncoder[F]
      .contramap[TextTag] { _.toString() }
      .withContentType(H.`Content-Type`(MediaType.`text/html`, DefaultCharset))
  }

  private val unauthedPages = HttpService[IO] {
    case req @ GET -> Root / "login" =>
      Ok(Authentication.loginForm())
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
    if (resp.status == Status.Unauthorized) {
      OptionT.liftF{ TemporaryRedirect(Location(uri("/login"))) }
    } else {
      OptionT.liftF{ IO(resp) }
    }
  })

  def htmlPageServices = unauthedPages <+> authedOrLogin


}
