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

  // ALTER TABLE person DISABLE TRIGGER post_delete_trigger;
  val setupInsertTriggers = (
    sql"""
       ALTER TABLE person ENABLE TRIGGER post_insert_trigger;
     """.update)
  // ALTER TABLE person ENABLE TRIGGER post_delete_trigger;
  val setupDeleteTriggers = (
    sql"""
       ALTER TABLE person DISABLE TRIGGER post_insert_trigger;
     """.update)
  val reorderTrigger = (
    sql"""
      CREATE OR REPLACE FUNCTION insert_row_func() RETURNS TRIGGER AS $$func$$
          DECLARE
              maxrank integer;
              nextrank integer;
          BEGIN
              maxrank := (SELECT MAX(rank) FROM person WHERE age=NEW.age);

              IF maxrank IS NULL THEN
                  nextrank := 0;
              ELSE
                  nextrank := maxrank+1;
              END IF;

              IF NEW.rank IS NULL THEN
                  -- null means append
                  NEW.rank := nextrank;
              ELSEIF NEW.rank > nextrank THEN
                  NEW.rank := nextrank;
              ELSEIF NEW.rank < 0 THEN
                  -- 0 means prepend, don't allow anything <0
                  NEW.rank := 0;
              END IF;

              UPDATE person SET rank = rank+1
                  WHERE rank = NEW.rank AND age=NEW.age;

              RETURN NEW;
          END;
      $$func$$ LANGUAGE plpgsql;

      CREATE OR REPLACE FUNCTION post_insert_func() RETURNS TRIGGER AS $$func$$
          BEGIN
              UPDATE person SET rank = rank+1
                  WHERE rank = NEW.rank AND age=NEW.age;

              RETURN NEW;
          END;
      $$func$$ LANGUAGE plpgsql;

      CREATE OR REPLACE FUNCTION delete_row_func() RETURNS TRIGGER AS $$func$$
          BEGIN
              -- UPDATE person SET rank = rank-1
              --     WHERE rank = OLD.rank+1 AND age=OLD.age;
              UPDATE person SET rank = rank-1
                  WHERE rank > OLD.rank AND age=OLD.age;

              RETURN OLD;
          END;
      $$func$$ LANGUAGE plpgsql;

      CREATE OR REPLACE FUNCTION post_delete_func() RETURNS TRIGGER AS $$func$$
          BEGIN
              UPDATE person SET rank = rank-1
                  WHERE rank = OLD.rank+1 AND age=OLD.age;

              RETURN OLD;
          END;
      $$func$$ LANGUAGE plpgsql;

      DROP TRIGGER IF EXISTS insert_row_trigger ON person;
      DROP TRIGGER IF EXISTS delete_row_trigger ON person;
      DROP TRIGGER IF EXISTS post_insert_trigger ON person;
      DROP TRIGGER IF EXISTS post_delete_trigger ON person;
      DROP TRIGGER IF EXISTS reorder_row_trigger ON person;

      DROP FUNCTION IF EXISTS reorder_row_func();

      CREATE TRIGGER insert_row_trigger
          BEFORE INSERT ON person
          FOR EACH ROW EXECUTE PROCEDURE insert_row_func();

      CREATE TRIGGER post_insert_trigger
          BEFORE UPDATE ON person
          FOR EACH ROW EXECUTE PROCEDURE post_insert_func();


      CREATE TRIGGER delete_row_trigger
          BEFORE DELETE ON person
          FOR EACH ROW EXECUTE PROCEDURE delete_row_func();

      -- CREATE TRIGGER post_delete_trigger
          -- BEFORE UPDATE ON person
          -- FOR EACH ROW EXECUTE PROCEDURE post_delete_func();

      -- ALTER TABLE person DISABLE TRIGGER post_insert_trigger;
      -- ALTER TABLE person DISABLE TRIGGER post_delete_trigger;
   """.update)
  import xa.yolo._

  def freshTables() = {
    (drop.quick *> create.quick).unsafePerformSync
    reorderTrigger.run.transact(xa).unsafePerformSync
  }

  def appendPerson(name: String, age: Int): Unit = {
    val query = for {
      _ <- setupInsertTriggers.run
      x <- sql""" insert into person (name, age) values($name, $age) """.update.run
    } yield x

    query.quick.unsafePerformSync
  }
  def insertPersonAt(name: String, age: Int, rank: Int): Unit = {
    val query = for {
      _ <- setupInsertTriggers.run
      x <- sql""" insert into person (name, age, rank) values($name, $age, $rank) """.update.run
    } yield x

    query.quick.unsafePerformSync
  }
  def prependPerson(name: String, age: Int): Unit = {
    val query = for {
      _ <- setupInsertTriggers.run
      x <- sql""" insert into person (name, age, rank) values($name, $age, 0) """.update.run
    } yield x

    query.quick.unsafePerformSync
  }

  def removePerson(name: String, age: Int): Unit = {
    val query = for {
      _ <- setupDeleteTriggers.run
      x <- sql"delete from person where name=$name AND age=$age".update.run
    } yield x

    query.quick.unsafePerformSync
  }

  def getAll(): Seq[(String, Int, Int)] = {
    sql"""select name, age, rank from person order by rank ASC"""
      .query[(String, Int, Int)]
      .list
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


  }


}
