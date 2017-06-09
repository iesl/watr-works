package edu.umass.cs.iesl.watr
package corpora
package database

import scalaz.concurrent.Task

import org.scalatest._

import doobie.imports._
import shapeless._

class Smokescreen extends FlatSpec with Matchers with DoobiePredef {
  val xa = DriverManagerTransactor[Task](
    "org.postgresql.Driver",
    "jdbc:postgresql:watrdev",
    "watrworker", "watrpasswd"
  )

  val drop: Update0 = sql"""
    DROP TABLE IF EXISTS person;
    """.update

  val create: Update0 = sql"""
    CREATE TABLE person (
      id   SERIAL,
      name VARCHAR NOT NULL UNIQUE,
      age  SMALLINT,
      rank INT NOT NULL
    );
    """.update



  import xa.yolo._

  def freshTables() = {
    veryUnsafeDropDatabase().run.transact(xa).unsafePerformSync
    defineOrderingTriggers(
      fr0"person",
      fr0"age"
    ).transact(xa).unsafePerformSync
  }

  def appendPerson(name: String, age: Int): Unit = {
    val query = for {
      x <- sql""" insert into person (name, age) values($name, $age) """.update.run
    } yield x

    query.quick.unsafePerformSync
  }
  def insertPersonAt(name: String, age: Int, rank: Int): Unit = {
    val query = for {
      x <- sql""" insert into person (name, age, rank) values($name, $age, $rank) """.update.run
    } yield x

    query.quick.unsafePerformSync
  }
  def prependPerson(name: String, age: Int): Unit = {
    val query = for {
      x <- sql""" insert into person (name, age, rank) values($name, $age, 0) """.update.run
    } yield x

    query.quick.unsafePerformSync
  }

  def removePerson(name: String, age: Int): Unit = {
    val query = for {
      x <- sql"delete from person where name=$name AND age=$age".update.run
    } yield x

    query.quick.unsafePerformSync
  }

  // FIXME: map(_.map(...)) => traverse
  def getAll(): Seq[(String, Int, Int)] = {
    sql"""select name, age, rank from person order by age,rank ASC"""
      .query[String :: Int :: Int :: HNil]
      .list
      .map{ _.map{
          case a :: b :: c :: HNil => (a, b, c)
      } }
      .transact(xa)
      .unsafePerformSync
  }

  behavior of "ordering data"

  // SECURITY DEFINER LANGUAGE PLPGSQL;


  it should "maintain ordering" in {
    freshTables()

    appendPerson("oliver1", 20)
    prependPerson("oliver01", 20)
    appendPerson("oliver2", 20)
    //
    prependPerson("oliver02", 20)
    appendPerson("oliver3", 20)
    appendPerson("oliver4", 20)
    insertPersonAt("oliver4-ins", 20, 3)

    println(getAll().mkString("{\n  ", "\n  ", "\n}"))

    removePerson("oliver1", 20)
    removePerson("oliver01", 20)
    removePerson("oliver4", 20)
    println(getAll().mkString("{\n  ", "\n  ", "\n}"))
    insertPersonAt("oliver09-ins", 20, 2)
    println(getAll().mkString("{\n  ", "\n  ", "\n}"))
    removePerson("oliver09-ins", 20)
    println(getAll().mkString("{\n  ", "\n  ", "\n}"))

    appendPerson("morgan1", 3)
    prependPerson("morgan01", 3)
    appendPerson("morgan2", 3)

    println(getAll().mkString("{\n  ", "\n  ", "\n}"))

    removePerson("morgan2", 3)

    println(getAll().mkString("{\n  ", "\n  ", "\n}"))

  }


}
