package edu.umass.cs.iesl.watr
package databasics

import scalaz._, Scalaz._
import scalaz.concurrent.Task

import org.scalatest._

import doobie.imports._

class Smokescreen extends FlatSpec with Matchers {
  val xa = DriverManagerTransactor[Task](
    "org.postgresql.Driver",
    "jdbc:postgresql:watrdev",
    "watrworker", "watrpasswd"
  )

  import xa.yolo._

  behavior of "basic connection"

  it should "select and take consistent memory" in {
    val q = sql"""select a.name, b.name from city a, city b""".query[(String, String)]

    val uio = q.process.take(5).transact(xa).run
    true
  }

  behavior of "DDL create/drop"

  val drop: Update0 = sql"""
    DROP TABLE IF EXISTS person
    """.update

  val create: Update0 = sql"""
    CREATE TABLE person (
      id   SERIAL,
      name VARCHAR NOT NULL UNIQUE,
      age  SMALLINT
    )
    """.update

  it should "drop/recreate table" in {
    (drop.quick *> create.quick).unsafePerformSync
  }

  behavior of "binary BYTEA (byte array) type"


}
