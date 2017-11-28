package edu.umass.cs.iesl.watr
package watrcolors
package server


// // import cats.data.OptionT
// import org.http4s.dsl.io._
// import tsec.authentication._
// // import tsec.authorization._
// import tsec.common.SecureRandomId
// import tsec.mac.imports.{HMACSHA256, MacSigningKey}
// import scala.concurrent.duration._
// import org.http4s._
// // import cats._

// import cats.effect._

// trait JWTAuthHelpers {

//   def userStore: BackingStore[IO, Int, User]
//   def jwtStore: BackingStore[IO, SecureRandomId, AugmentedJWT[HMACSHA256, Int]]



//   val signingKey: MacSigningKey[HMACSHA256] =
//     HMACSHA256.generateKeyUnsafe() //Our signing key. Instantiate in a safe way using GenerateLift

//   val jwtStatefulAuth = JWTAuthenticator.withBackingStore(
//     expiryDuration = 10.minutes, //Absolute expiration time
//     maxIdle        = None,
//     tokenStore     = jwtStore,
//     identityStore  = userStore,
//     signingKey     = signingKey
//   )



//   // val authservice: TSecAuthService[IO, User, TSecBearerToken[Int]] = TSecAuthService {
//   //   case GET -> Root asAuthed user =>
//   //     Ok()
//   // }

//   val Auth = SecuredRequestHandler(jwtStatefulAuth)

//   /*
//    Now from here, if want want to create services, we simply use the following
//    (Note: Since the type of the service is HttpService[IO], we can mount it like any other endpoint!):
//    */
//   val sampleService: HttpService[IO] = Auth {
//     //Where user is the case class User above
//     case request@GET -> Root / "api" asAuthed user =>
//       /*
//        Note: The request is of type: SecuredRequest, which carries:
//        1. The request
//        2. The Authenticator (i.e token)
//        3. The identity (i.e in this case, User)
//        */
//       // val r: SecuredRequest[IO, User, TSecBearerToken[Int]] = request
//       val r: SecuredRequest[IO, User, AugmentedJWT[HMACSHA256, Int]] = request
//       Ok()
//   }
// }


