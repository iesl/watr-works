package edu.umass.cs.iesl.watr
package watrcolors
package server



// import cats.kernel.Eq
// import org.http4s._
// import org.http4s.circe._
// import tsec.authorization._
// // import org.http4s.server._
// import cats._
// // import cats.implicits._, cats.data._

// import cats.effect._
// // import org.reactormonk.{CryptoBits, PrivateKey}
// // import java.time._
// import _root_.io.circe // , circe._, circe.syntax._

// import circe.generic.auto._
// // import tsec.authentication._

// case class UserData(
//   id: Int@@UserID,
//   emailAddr: String@@EmailAddr,
//   pass: String,
//   session: String
// )

// case class LoginForm(username: String, password: String)

// object LoginForm {
//   object LoginError extends Exception {
//     override def getMessage: String = "Login Error"

//     override def fillInStackTrace(): Throwable = this
//   }

//   implicit def decoder: EntityDecoder[IO, LoginForm] = jsonOf[IO, LoginForm]

// }

// sealed abstract case class Role(roleRepr: String)

// object Role extends SimpleAuthEnum[Role, String] {
//   implicit object Administrator  extends Role("Administrator")
//   implicit object Curator        extends Role("Curator")
//   implicit object Labeler        extends Role("Labeler")

//   implicit object CorruptedData  extends Role("CorruptedData")

//   implicit val E: Eq[Role]      = Eq.fromUniversalEquals[Role]
//   val getRepr: (Role) => String = _.roleRepr

//   protected val values: AuthGroup[Role] = AuthGroup(Administrator, Curator, Labeler)
//   val orElse: Role                      = CorruptedData
// }


// case class User(
//   id: Int,
//   // id: Int@@UserID,
//   emailAddr: String@@EmailAddr,
//   name: String,
//   role: Role = Role.Labeler
// )

// object User {
//   implicit def authRole[F[_]](implicit F: MonadError[F, Throwable]): AuthorizationInfo[F, Role, User] =
//     new AuthorizationInfo[F, Role, User] {
//       def fetchInfo(u: User): F[Role] = F.pure(u.role)
//     }
// }
