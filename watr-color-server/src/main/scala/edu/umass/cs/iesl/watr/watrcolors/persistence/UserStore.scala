package edu.umass.cs.iesl.watr
package watrcolors
package persistence

// import java.util.UUID

import cats.effect.IO
import cats.data.OptionT
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

  def getByEmail(email: String@@EmailAddr): OptionT[IO, User] = {
    val userByEmail: Option[User] = for {
      userId <- userbaseApi.getUserByEmail(email)
      person <- userbaseApi.getUser(userId)
    } yield {
      User(person.prKey, person.email)
    }
    OptionT(IO(userByEmail))
  }
  def getById(userId: Int@@UserID): OptionT[IO, User] = {
    val userByEmail: Option[User] = for {
      person <- userbaseApi.getUser(userId)
    } yield {
      User(person.prKey, person.email)
    }
    OptionT(IO(userByEmail))
  }

}

object UserStore {
  type IDType = Int
  type ValueType = User

  def fromDb(corpusAccessDB: CorpusAccessDB): IO[UserStore] = {
    IO(new UserStore(corpusAccessDB))
  }
}
