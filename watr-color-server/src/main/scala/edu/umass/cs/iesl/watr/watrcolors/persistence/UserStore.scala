package edu.umass.cs.iesl.watr
package watrcolors
package persistence

// import java.util.UUID

import cats.effect.IO
import cats.data.OptionT
import edu.umass.cs.iesl.watr.workflow.UserbaseApi
import tsec.authentication._
import models.users._

sealed class UserStore(
  userbaseApi: UserbaseApi
) extends BackingStore[IO, Int, User] {
  import UserStore._


  def put(elem: ValueType): IO[ValueType] = {
    // userbaseApi.addUser(email: String)
    ???
  }

  def get(id: IDType): OptionT[IO, ValueType] = {
    // OptionT(ref.get.map(_.get(id)))
    ???
  }

  def update(v: ValueType): IO[ValueType] = {
    // ref
    //   .modify(_.updated(v.id, v))
    //   .map(_ => 1)
    ???

  }

  def delete(id: IDType): IO[Unit] = {
    // ref
    //   .modify(_ - id)
    //   .map(modified => modified.previous.size - modified.now.size)

   ???
  }

  def exists(username: String): OptionT[IO, User] = {
    ???

  }
    // OptionT(ref.get.map(_.values.find(_.username == username)))
}

object UserStore {
  type IDType = Int
  type ValueType = User
  // def apply(implicit ec: ExecutionContext, F: Effect[IO]): IO[UserStore] =
  //   Ref.initialized(HashMap.empty[IDType, ValueType]).map { m =>
  //     new UserStore {
  //       protected val ref: Ref[IO, HashMap[IDType, ValueType]] = m
  //     }
  //   }
  def fromUserbaseApi(userbaseApi: UserbaseApi): IO[UserStore] = IO{new UserStore(userbaseApi)}
}


// class JWTBackingStore(
// ) extends BackingStore[IO, SecureRandomId, AugmentedJWT[HMACSHA256, Int]] {

//   type I =  SecureRandomId
//   type V =  AugmentedJWT[HMACSHA256, Int]

//   def put(elem: V): IO[V] = {
//     ???
//   }

//   def get(id: I): OptionT[IO, V] = ???

//   def update(v: V): IO[V] = {
//     IO(v)
//   }

//   def delete(id: I): IO[Unit] = {
//     ???
//   }
// }

// class UserBackingStore(
//   userbaseApi: UserbaseApi
// ) extends BackingStore[IO, Int, User] {

//   // private val storageMap = mutable.HashMap.empty[Int, User]
//   // userId     <- userbaseApi.getUserByEmail(userEmail).toSeq

//   def put(elem: User): IO[User] = {
//     val userId = userbaseApi.addUser(elem.name)

//     userbaseApi.getUser(userId).map{p =>
//       // User(p)
//     }
//     // get(userId.unwrap)
//     ???
//       // .getOrElse {
//       //     IO.raiseError(new IllegalArgumentException)
//       // }
//     // val map = storageMap.put(elem.id, elem)
//     // if (map.isEmpty)
//     //   IO(elem)
//     // else
//     //   IO.raiseError(new IllegalArgumentException)
//   }

//   def get(id: Int): OptionT[IO, User] = {
//    ???
//      // OptionT.fromOption[IO](storageMap.get(id))
//   }

//   def update(v: User): IO[User] = {
//     // storageMap.update(v.id, v)
//     IO(v)
//   }

//   def delete(id: Int): IO[Unit] = {
//     ???
//       // OptionT(ref.get.map(_.values.find(_.username == username)))
//       // storageMap.remove(id) match {
//       //   case Some(_) => IO.unit
//       //   case None    => IO.raiseError(new IllegalArgumentException)
//       // }
//   }

//   def exists(username: String): OptionT[IO, User] = ???

// }




// abstract class PasswordStore[F[_], I, A](getId: A => I)(implicit F: Sync[F])
//     extends SCryptPasswordStore[F, I] {

//   import scala.collection.mutable

//   private val otherTable = mutable.HashMap[I, SCrypt]()

//   // type I =  Int
//   // type V =  User

//   def retrievePass(id: I): F[SCrypt] = {
//     // ???
//       otherTable.get(id) match {
//         case Some(s) =>
//           F.pure(s)
//         case None =>
//           F.raiseError(new IllegalArgumentException)
//       }
//   }

//   def putCredentials(credentials: RawCredentials[I]): F[Unit] = F.delay {
//     otherTable.put(credentials.identity, credentials.rawPassword.hashPassword[SCrypt])
//   }

//   def updateCredentials(credentials: RawCredentials[I]): F[Unit] = F.delay {
//     otherTable.update(credentials.identity, credentials.rawPassword.hashPassword[SCrypt])
//   }

//   def removeCredentials(credentials: RawCredentials[I]): F[Unit] =
//     F.ensure(retrievePass(credentials.identity))(new IllegalArgumentException)(
//       credentials.rawPassword.checkWithHash(_)) *> F.delay(
//       otherTable.remove(credentials.identity)) *> F.unit
// }














// // def dummyBackingStore[F[_], I, V](getId: V => I)(implicit F: Sync[F]) = new BackingStore[F, I, V] {
// //   private val storageMap = mutable.HashMap.empty[I, V]

// //   def put(elem: V): F[V] = {
// //     val map = storageMap.put(getId(elem), elem)
// //     if (map.isEmpty)
// //       F.pure(elem)
// //     else
// //       F.raiseError(new IllegalArgumentException)
// //   }

// //   def get(id: I): OptionT[F, V] =
// //     OptionT.fromOption[F](storageMap.get(id))

// //   def update(v: V): F[V] = {
// //     storageMap.update(getId(v), v)
// //     F.pure(v)
// //   }

// //   def delete(id: I): F[Unit] =
// //     storageMap.remove(id) match {
// //       case Some(_) => F.unit
// //       case None    => F.raiseError(new IllegalArgumentException)
// //     }
// // }



// // val bearerTokenStore =
// //   dummyBackingStore[IO, SecureRandomId, TSecBearerToken[Int]](s => SecureRandomId.coerce(s.id))
// // val bearerTokenAuth =
// //   BearerTokenAuthenticator(
// //     bearerTokenStore,
// //     userStore,
// //     settings
// //   )


// // val jwtStore = backingStore // dummyBackingStore[IO, SecureRandomId, AugmentedJWT[HMACSHA256, Int]](s => SecureRandomId.coerce(s.id))

// // val settings: TSecTokenSettings = TSecTokenSettings(
// //   expiryDuration = 1.day, //Absolute expiration time
// //   maxIdle = None
// // )
// //We create a way to store our users. You can attach this to say, your doobie accessor
// // val userStore: BackingStore[IO, Int, User] = dummyBackingStore[IO, Int, User](_.id)
