package edu.umass.cs.iesl.watr
package watrcolors
package services

import cats.MonadError
import cats.data.Kleisli
import org.http4s._
import cats._
import cats.data._
import cats.syntax.all._
import org.http4s.server.Middleware
import tsec.authorization._
import tsec.authentication.{
  AuthenticatorService,
  TSecAuthService,
  SecuredRequest,

}

// From TSec
//   type TSecAuthService[Ident, A, F] = Kleisli[OptionT[F, ?], SecuredRequest[F, Ident, A], Response[F]]

final case class UserAwareRequest[F[_], Identity, Auth](request: Request[F], identity: Option[Identity], authenticator: Auth)

// object FakeAuthHandler {
//   def apply(pf: PartialFunction[SecuredRequest[User], F[Response[F]]]): HttpService[IO] =
//   defaultMiddleware(TSecAuthService(pf, authenticator.afterBlock))
//     .handleError(_ => Response[F](Status.Unauthorized))
//   def authorized(
//     authorization: Authorization[F, User, Auth]
//   )(pf: PartialFunction[SecuredRequest[F, User, Auth], F[Response[F]]]): HttpService[F] =
//     authorizedMiddleware(authorization)(TSecAuthService(pf, authenticator.afterBlock))
//       .handleError(_ => Response[F](Status.Unauthorized))

// }

object SecurityMiddleware {
  // From http4s:
  //   type Middleware[F[_], A, B, C, D] = Kleisli[F, A, B] => Kleisli[F, C, D]
  type Type[F[_], I, A] =
    Middleware[OptionT[F, ?], SecuredRequest[F, I, A], Response[F], Request[F], Response[F]]


  def apply[F[_]: Monad, Ident, Auth](
    authedStuff: Kleisli[OptionT[F, ?], Request[F], SecuredRequest[F, Ident, Auth]]
  ): Type[F, Ident, Auth] =
    service => {
      authedStuff
        .andThen(service.mapF(o => OptionT.liftF(o.fold(Response[F](Status.NotFound))(identity))))
        .mapF(o => OptionT.liftF(o.fold(Response[F](Status.Unauthorized))(identity)))
    }
}


sealed abstract class WSecureRequestHandler[F[_], Identity, User, Auth](
    val authenticator: AuthenticatorService[F, Identity, User, Auth]
)(implicit F: MonadError[F, Throwable]) {


  /**Our default middleware **/
  protected val defaultMiddleware = SecurityMiddleware(Kleisli(authenticator.extractAndValidate))

  /** Create an Authorized middleware from an Authorization **/
  protected def authorizedMiddleware(authorization: Authorization[F, User, Auth]): SecurityMiddleware.Type[F, User, Auth] = {
    val authed = Kleisli(authenticator.extractAndValidate)
      .andThen(e => authorization.isAuthorized(e))
    SecurityMiddleware(authed)
  }

  /** Compose Requests **/
  def apply(pf: PartialFunction[SecuredRequest[F, User, Auth], F[Response[F]]]): HttpRoutes[F]

  /** Lift an Authenticated Service into an HttpService **/
  def liftService(service: TSecAuthService[User, Auth, F]): HttpRoutes[F] =
    defaultMiddleware(service)
      .handleError(_ => Response[F](Status.Unauthorized))

  /** Create an Authorized Service **/
  def authorized(authorization: Authorization[F, User, Auth])(
      pf: PartialFunction[SecuredRequest[F, User, Auth], F[Response[F]]]
  ): HttpRoutes[F]

  /** Create an Authorized service from a TSecAuthService **/
  def liftService(
      authorization: Authorization[F, User, Auth],
      service: TSecAuthService[User, Auth, F]
  ): HttpRoutes[F] =
    authorizedMiddleware(authorization)(service)
      .handleError(_ => Response[F](Status.Unauthorized))

}

object WSecureRequestHandler {


  /** Build our WSecureRequestHandler detecting whether it is rolling window or not **/
  def apply[F[_], Identity, User, Auth](
      authenticator: AuthenticatorService[F, Identity, User, Auth]
  )(implicit F: MonadError[F, Throwable]): WSecureRequestHandler[F, Identity, User, Auth] =
    if (authenticator.maxIdle.isDefined) {
      rollingWindow[F, Identity, User, Auth](authenticator)
    } else {
      default[F, Identity, User, Auth](authenticator)
    }

  /** Default Construction **/
  private def default[F[_], Identity, User, Auth](
      authenticator: AuthenticatorService[F, Identity, User, Auth]
  )(implicit F: MonadError[F, Throwable]): WSecureRequestHandler[F, Identity, User, Auth] =
    new WSecureRequestHandler[F, Identity, User, Auth](authenticator) {
      def apply(pf: PartialFunction[SecuredRequest[F, User, Auth], F[Response[F]]]): HttpRoutes[F] =

        defaultMiddleware(TSecAuthService(pf, authenticator.afterBlock))
          .handleError(_ => Response[F](Status.Unauthorized))

      def authorized(
          authorization: Authorization[F, User, Auth]
      )(pf: PartialFunction[SecuredRequest[F, User, Auth], F[Response[F]]]): HttpRoutes[F] =
        authorizedMiddleware(authorization)(TSecAuthService(pf, authenticator.afterBlock))
          .handleError(_ => Response[F](Status.Unauthorized))
    }

  /** Sliding/Rolling Window expiry Construction **/
  private def rollingWindow[F[_], Identity, User, Auth](
      authenticator: AuthenticatorService[F, Identity, User, Auth]
  )(implicit F: MonadError[F, Throwable]): WSecureRequestHandler[F, Identity, User, Auth] =
    new WSecureRequestHandler[F, Identity, User, Auth](authenticator) {
      def apply(pf: PartialFunction[SecuredRequest[F, User, Auth], F[Response[F]]]): HttpRoutes[F] =

        defaultMiddleware(TSecAuthService(pf))
          .handleError(_ => Response[F](Status.Unauthorized))

      def authorized(authorization: Authorization[F, User, Auth])(
          pf: PartialFunction[SecuredRequest[F, User, Auth], F[Response[F]]]
      ): HttpRoutes[F] =
        authorizedMiddleware(authorization)(TSecAuthService(pf))
          .handleError(_ => Response[F](Status.Unauthorized))
    }

}
