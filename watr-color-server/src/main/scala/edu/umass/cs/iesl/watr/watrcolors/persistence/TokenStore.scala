package edu.umass.cs.iesl.watr
package watrcolors
package persistence

import java.util.UUID


import cats.data.OptionT
import cats.effect.Effect
import fs2.async.Ref
import tsec.authentication._
import cats.effect.IO
import tsec.cipher.symmetric.imports.AES128

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext

class TokenStore extends BackingStore[IO, TokenStore.IDType, TokenStore.ValueType] {
  import TokenStore._

  // protected val ref: Ref[IO, HashMap[IDType, ValueType]]

  def put(elem: ValueType): IO[ValueType] = {
    ???
  }

  def get(id: IDType): OptionT[IO, ValueType] =
    ???
    // OptionT(ref.get.map(_.get(id)))

  def update(v: ValueType): IO[ValueType] = {
    ???
  }
    // ref
    //   .modify(_.updated(v.id, v))
    //   // .map(_ => 1)

  def delete(id: IDType): IO[Unit] = {
   ???
  }
    // ref
    //   .modify(_ - id)
    //   .map(modified => modified.previous.size - modified.now.size)

}

object TokenStore {
  type IDType = UUID
  type ValueType = AuthEncryptedCookie[AES128, Int]

  def apply(implicit ec: ExecutionContext, F: Effect[IO]): IO[TokenStore] =
    Ref
      .initialized(HashMap.empty[IDType, ValueType])
      .map { m =>
        new TokenStore {
          protected val ref: Ref[IO, HashMap[IDType, ValueType]] = m
        }
      }
}
