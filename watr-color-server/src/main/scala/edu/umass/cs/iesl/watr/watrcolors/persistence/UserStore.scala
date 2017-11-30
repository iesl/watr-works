package edu.umass.cs.iesl.watr
package watrcolors
package persistence

// import java.util.UUID

import cats.effect.IO
import cats.data._
import tsec.authentication._
import models.users._
import TypeTags._

import workflow.UserbaseApi
import corpora.database.CorpusAccessDB

sealed class UserStore(
  corpusAccessDB: CorpusAccessDB
) extends BackingStore[IO, Int, User] {
  import UserStore._
  lazy val userbaseApi: UserbaseApi = corpusAccessDB.userbaseApi


  def put(elem: ValueType): IO[ValueType] = {
    val userId = userbaseApi.getUserByEmail(elem.email)
      .fold (userbaseApi.addUser(elem.email)) (id => id)

    val u = for {
      person <- userbaseApi.getUser(userId)
    } yield User(person.prKey, person.email)

    IO(u.getOrElse { sys.error(s"could not add user ${elem.email}") })
  }

  def get(id: IDType): OptionT[IO, ValueType] = {
    getById(UserID(id))
  }

  def update(v: ValueType): IO[ValueType] = {
    ???
  }

  def delete(id: IDType): IO[Unit] = {
   ???
  }

  def exists(email: String@@EmailAddr): OptionT[IO, User] = {
    getByEmail(email)
  }


  // def create(email: String@@EmailAddr): EitherT[IO, Unit, User] = {
  //   val asdf: Either[IO[Unit], IO[User]] = getByEmail(email).fold(
  //     Right[Unit, User](put(User(UserID(0), email)))
  //   )(u => Left[Unit, User](IO((): Unit)))
  //   // getByEmail(email).toLeft(right=())
  //   ???
  // }

  def getByEmail(email: String@@EmailAddr): OptionT[IO, User] = {
    val u: Option[User] = for {
      userId <- userbaseApi.getUserByEmail(email)
      person <- userbaseApi.getUser(userId)
    } yield {
      User(person.prKey, person.email)
    }
    OptionT(IO(u))
  }

  def getById(userId: Int@@UserID): OptionT[IO, User] = {
    val u: Option[User] = for {
      person <- userbaseApi.getUser(userId)
    } yield {
      User(person.prKey, person.email)
    }
    OptionT(IO(u))
  }

}

object UserStore {
  type IDType = Int
  type ValueType = User

  def fromDb(corpusAccessDB: CorpusAccessDB): IO[UserStore] = {
    IO(new UserStore(corpusAccessDB))
  }
}
