package edu.umass.cs.iesl.watr
package watrcolors
package persistence


import cats.data.OptionT
import cats.effect._
import tsec.authentication._
import models.users._
import corpora.database.CorpusAccessDB
import doobie.imports._

import TypeTags._

import tsec.passwordhashers.imports.SCrypt
sealed class PasswordStore(
  corpusAccessDB: CorpusAccessDB
) extends BackingStore[IO, PasswordStore.IDType, AuthInfo] {
  import PasswordStore._

  def runq[A](query: ConnectionIO[A]): A = {
    corpusAccessDB.runq(query)
  }


  def put(elem: ValueType): IO[ValueType] = {
    val AuthInfo(
      userId,
      username,
      password
    ) = elem

    runq{
      sql"""
           WITH upsert AS (
             UPDATE person_auth
             SET username=${username.unwrap},
                 password=${password.toString()}
             WHERE person = ${userId.unwrap}
             RETURNING *
           )
           INSERT INTO person_auth (person, username, password)
           SELECT ${userId.unwrap},
                  ${username.unwrap},
                  ${password.toString()}
           WHERE NOT EXISTS (SELECT * FROM upsert)
      """.update.run
    }
    // runq{
    //   sql"""
    //      insert into person_auth (person, username, password)
    //      values (${userId.unwrap}, ${username.unwrap}, ${password.toString()})
    //   """.update.run
    // }
    get(userId.unwrap).getOrElse {
      sys.error("TokenStore:put; just-inserted UUID not found")
    }
    // IO { elem }
  }

  def get(id: IDType): OptionT[IO, ValueType] = {
    val res = runq{
      sql""" select person, username, password from person_auth """
        .query[(Int, String, String)]
        .map{ case (userId, username, password) =>
          AuthInfo(UserID(userId), Username(username), SCrypt.fromString(password))
        }
        .option
    }
    OptionT{ IO{ res } }
  }

  def update(v: ValueType): IO[ValueType] = {
    ???
  }

  def delete(id: IDType): IO[Unit] = {
   ???
  }
}

object PasswordStore {
  type IDType = Int
  type ValueType = AuthInfo

  def fromDb(corpusAccessDB: CorpusAccessDB): IO[PasswordStore] = {
    IO(new PasswordStore(corpusAccessDB))
  }
}

// class JWTBackingStore() extends BackingStore[IO, SecureRandomId, AugmentedJWT[HMACSHA256, Int]] {

// abstract class PasswordStore[F[_], I, A](getId: A => I)(implicit F: Sync[F])
//     extends SCryptPasswordStore[F, I] {

//   def retrievePass(id: I): F[SCrypt] = {
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

// // val settings: TSecTokenSettings = TSecTokenSettings(
// //   expiryDuration = 1.day, //Absolute expiration time
// //   maxIdle = None
// // )
// //We create a way to store our users. You can attach this to say, your doobie accessor
// // val userStore: BackingStore[IO, Int, User] = dummyBackingStore[IO, Int, User](_.id)
