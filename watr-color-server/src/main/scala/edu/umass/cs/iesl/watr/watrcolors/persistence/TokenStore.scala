package edu.umass.cs.iesl.watr
package watrcolors
package persistence

import java.util.UUID


import cats.data.OptionT
import tsec.authentication._
import cats.effect.IO
import tsec.cipher.symmetric.imports.AES128
import corpora.database.CorpusAccessDB

import doobie.imports._
import doobie.postgres.pgtypes.UuidType
import TypeTags._

class TokenStore(
  corpusAccessDB: CorpusAccessDB
) extends BackingStore[IO, TokenStore.IDType, TokenStore.ValueType] {
  import TokenStore._

  def runq[A](query: ConnectionIO[A]): A = {
    corpusAccessDB.runq(query)
  }

  def put(elem: ValueType): IO[ValueType] = {
    // println(s"TokenStore:put(${elem})")

    val AuthEncryptedCookie(
      id          , // : UUID,
      name        , // : String,
      content     , // : AEADCookie[A]=String ,
      identity    , // : Id=String (userId: owner),
      expiry      , // : Instant,
      lastTouched , // : Option[Instant],
      secure      , // : Boolean,
      httpOnly    , // : Boolean = true,
      domain      , // : Option[String] = None,
      path        , // : Option[String] = None,
      extension   , // : Option[String] = None
    ) = elem

    runq{
      sql"""
           WITH upsert AS (
             UPDATE token
             SET
               tuuid=${id},
               name=${name},
               content=${content.toString()}
             WHERE owner = ${identity}
             RETURNING *
           )
           INSERT INTO token (token, tuuid, name, content, owner)
           SELECT nextval('token_token_seq'),
                  ${id},
                  ${name},
                  ${content.toString()},
                  ${identity}
           WHERE NOT EXISTS (SELECT * FROM upsert)
      """.update.run
    }

    get(id).getOrElse {
      sys.error("TokenStore:put; just-inserted UUID not found")
    }
  }

  def get(id: IDType): OptionT[IO, ValueType] = {
    OptionT{ IO {
      getTokenForUUID(id)
        .map(getToken(_))
    }}
  }

  def update(v: ValueType): IO[ValueType] = {
    put(v)
  }

  def delete(id: IDType): IO[Unit] = {
    runq{
      sql""" delete from token from token where tuuid = ${id} """
        .update.run
    }
    IO{ () }
  }

  def createUserToken(userId: Int@@UserID): Int@@TokenID = {
    ???
  }

  def deleteToken(tokenId: Int@@TokenID): Unit = {
    runq{
      sql""" delete from token from token where token = ${tokenId.unwrap} """
        .update.run
    }
  }

  def getTokenForUUID(tuuid: UUID): Option[Int@@TokenID] = {
    runq{
      sql""" select token from token where tuuid = ${tuuid} """
        .query[Int]
        .map { TokenID(_) }
        .option
    }
  }

  def getToken(tokenId: Int@@TokenID): ValueType = {
    runq{
      sql"""
        select token, tuuid, name, content, owner
        from token where token = ${tokenId.unwrap}
      """.query[(Int, UUID, String, String, Int)].unique.map {
        case (tokenId, tuuid, name, content, owner)  =>
          val cookieContent = tsec.cookies.AEADCookie[AES128](content)
          AuthEncryptedCookie(
            id=tuuid,
            name=name,
            content=cookieContent,
            identity=owner,
            expiry=null,
            lastTouched=None,
            secure=true,
            httpOnly=true,
            domain = None,
            path = None,
            extension = None
          )
      }
    }
  }

}

object TokenStore {
  type IDType = UUID
  type ValueType = AuthEncryptedCookie[AES128, Int]

  def fromDb(corpusAccessDB: CorpusAccessDB): IO[TokenStore] = {
    IO(new TokenStore(corpusAccessDB))
  }
}
