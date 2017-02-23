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

  val reorderTrigger = (
    sql"""
      CREATE OR REPLACE FUNCTION insert_row_func() RETURNS TRIGGER AS $$func$$
          DECLARE
              maxrank integer;
          BEGIN
              maxrank := (SELECT MAX(rank) FROM person WHERE age=NEW.age);

              IF maxrank IS NULL THEN
                  maxrank := -1;
              END IF;

              IF NEW.rank IS NULL THEN
                  NEW.rank := maxrank+1;
              ELSEIF NEW.rank > maxrank+1 THEN
                  NEW.rank := maxrank+1;
              ELSEIF NEW.rank < 0 THEN
                  NEW.rank := 0;
              END IF;

              UPDATE person SET rank = rank+1
                  WHERE rank = NEW.rank AND age=NEW.age;

              RETURN NEW;
          END;
      $$func$$ LANGUAGE plpgsql;

      CREATE OR REPLACE FUNCTION update_insert_func() RETURNS TRIGGER AS $$func$$
          DECLARE
              maxrank integer;
          BEGIN
              maxrank := (SELECT MAX(rank) FROM person WHERE age=NEW.age);

              IF maxrank IS NULL THEN
                  maxrank := -1;
              END IF;

              IF NEW.rank IS NULL THEN
                  NEW.rank := maxrank+1;
              ELSEIF NEW.rank > maxrank+1 THEN
                  NEW.rank := maxrank+1;
              ELSEIF NEW.rank < 0 THEN
                  NEW.rank := 0;
              END IF;

              UPDATE person SET rank = rank+1
                  WHERE rank = NEW.rank AND age=NEW.age;

              RETURN NEW;
          END;
      $$func$$ LANGUAGE plpgsql;

      CREATE OR REPLACE FUNCTION delete_row_func() RETURNS TRIGGER AS $$func$$
          BEGIN
              ALTER TABLE tblname DISABLE TRIGGER update_row_func;

              UPDATE person SET rank = rank-1
                  WHERE rank = OLD.rank;

              ALTER TABLE tblname ENABLE TRIGGER update_row_func;
              RETURN NEW;
          END;
      $$func$$ LANGUAGE plpgsql;

      CREATE TRIGGER reorder_row_trigger
          BEFORE INSERT OR UPDATE ON person
          FOR EACH ROW
          EXECUTE PROCEDURE reorder_row_func();
   """.update)
  import xa.yolo._

  def freshTables() = {
    (drop.quick *> create.quick).unsafePerformSync
    reorderTrigger.run.transact(xa).unsafePerformSync
  }

  def insertPerson(name: String, age: Int): Unit = {
    sql""" insert into person (name, age) values($name, $age) """.update.quick.unsafePerformSync
  }
  def insertPersonAt(name: String, age: Int, rank: Int): Unit = {
    sql""" insert into person (name, age, rank) values($name, $age, $rank) """.update.quick.unsafePerformSync
  }
  def prependPerson(name: String, age: Int): Unit = {
    sql""" insert into person (name, age, rank) values($name, $age, 0) """.update.quick.unsafePerformSync
  }

  def removePerson(name: String, age: Int): Unit = {
    sql"delete from person where name=$name AND age=$age".update.quick.unsafePerformSync
  }

  def getAll(): Seq[(String, Int, Int)] = {
    sql"select name, age, rank from person order by rank ASC"
      .query[(String, Int, Int)]
      .list
      .transact(xa)
      .unsafePerformSync
  }

  behavior of "ordering data"

  // SECURITY DEFINER LANGUAGE PLPGSQL;


  it should "maintain ordering" in {
    freshTables()

    insertPerson("oliver1", 20)
    prependPerson("oliver01", 20)
    insertPerson("oliver2", 20)
    //
    prependPerson("oliver02", 20)
    insertPerson("oliver3", 20)
    insertPerson("oliver4", 20)
    insertPersonAt("oliver4-ins", 20, 3)

    println(getAll().mkString("{\n  ", "\n  ", "\n}"))

    removePerson("oliver3", 20)

    println(getAll().mkString("{\n  ", "\n  ", "\n}"))


  }


}
