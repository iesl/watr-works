package edu.umass.cs.iesl.watr
package watrcolors
package persistence


import cats.data.OptionT
import cats.effect._
import fs2.async.Ref
import models._
import tsec.authentication._

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext
import models.users._

sealed abstract class PasswordStore extends BackingStore[IO, PasswordStore.IDType, AuthInfo] {
  import PasswordStore._

  protected val ref: Ref[IO, HashMap[IDType, AuthInfo]]

  def put(elem: ValueType): IO[ValueType] = {
    ???
    // ref
    //   .modify(_ + (elem.parentId -> elem))
    //   .map(modified => modified.now.size - modified.previous.size)
  }

  def get(id: IDType): OptionT[IO, ValueType] =
    OptionT(ref.get.map(_.get(id)))

  def update(v: ValueType): IO[ValueType] = {
    ???
      // ref
      // .modify(_.updated(v.parentId, v))
      // .map(_ => 1)
  }

  def delete(id: IDType): IO[Unit] = {
   ???
  }
    // ref
    //   .modify(_ - id)
    //   .map(modified => modified.previous.size - modified.now.size)

  // def getPass(userId: IDType): OptionT[IO, AuthInfo] =
  //   OptionT(ref.get.map(_.get(userId)))
}

object PasswordStore {
  type IDType = Int
  type ValueType = AuthInfo

  def apply(implicit ec: ExecutionContext, F: Effect[IO]): IO[PasswordStore] =
    Ref.initialized(HashMap.empty[IDType, ValueType]).map {
      m => new PasswordStore {
        protected val ref: Ref[IO, HashMap[IDType, ValueType]] = m
      }
    }
}
