package edu.umass.cs.iesl.watr
package textreflow //;import acyclic.file


import doobie.imports._
import scalaz.concurrent.Task

// import shapeless._
// import ops.record._
// import shapeless.record._


object Foo  {
  sealed trait Rec[A]
  // type ARec = Rec[Record.`'x -> Int`.T]
}

import databasics._

object Tables extends DoobiePredef {
  object Document {
    val create = sql"""
      CREATE TABLE document (
        id            SERIAL PRIMARY KEY,
        stable_id     VARCHAR(128) UNIQUE
      );
      CREATE INDEX document_stable_id ON document USING hash (stable_id);
    """.update

    case class Model(
      id: Int,
      stableId: String@@DocumentID
    )

    def insert(docId: String@@DocumentID) = sql"""
       insert into document (stable_id) values (${docId})
    """.update.withUniqueGeneratedKeys[Int]("id")
  }

  object Page {

    val createPageTable: Update0 = sql"""
      CREATE TABLE page (
        id          SERIAL PRIMARY KEY,
        document    INTEGER REFERENCES document,
        pagenum     SMALLINT,
        pageimg     BYTEA,
        bleft       INTEGER,
        btop        INTEGER,
        bwidth      INTEGER,
        bheight     INTEGER
      );
    """.update

  }
}

class TextReflowDBTables(
  val xa: Transactor[Task]
) extends DoobiePredef {
  // import xa.yolo._

  val createDocumentTable: Update0 = sql"""
      CREATE TABLE document (
        id            SERIAL PRIMARY KEY,
        stable_id     VARCHAR(128) UNIQUE
      );
      CREATE INDEX document_stable_id ON document USING hash (stable_id);
    """.update

  val createPageTable: Update0 = sql"""
      CREATE TABLE page (
        id          SERIAL PRIMARY KEY,
        document    INTEGER REFERENCES document,
        pagenum     SMALLINT,
        pageimg     BYTEA,
        bleft       INTEGER,
        btop        INTEGER,
        bwidth      INTEGER,
        bheight     INTEGER
      );
    """.update



  // insert into page (document, pagenum, pageimg, bleft, btop, bwidth, bheight)
  val createLabelTable: Update0 = sql"""
      CREATE TABLE label (
        id             SERIAL PRIMARY KEY,
        key            VARCHAR(50)
      );
      CREATE INDEX label_key ON label USING hash (key);
    """.update

  val createZoneTable: Update0 = sql"""
      CREATE TABLE zone (
        id          SERIAL PRIMARY KEY,
        zoneid      INTEGER NOT NULL,
        document    INTEGER REFERENCES document
      );
    """.update

  // zone - label :: * - *
  val createZoneToLabelTable: Update0 = sql"""
      CREATE TABLE zone_to_label (
        zone         INTEGER REFERENCES zone,
        label        INTEGER REFERENCES label,
        PRIMARY KEY (zone, label)
      );
    """.update

  // zone - targetregion :: * - * (NB not sure if this is the right way to connect these)
  val createZoneToTargetRegion: Update0 = sql"""
      CREATE TABLE zone_to_targetregion (
        zone          INTEGER REFERENCES zone,
        targetregion  INTEGER REFERENCES targetregion
      );
      CREATE UNIQUE INDEX uniq__zone_to_targetregion ON zone_to_targetregion (zone, targetregion);
    """.update

  val createTargetRegion: Update0 = sql"""
      CREATE TABLE targetregion (
        id          SERIAL PRIMARY KEY,
        page        INTEGER REFERENCES page,
        bleft       INTEGER,
        btop        INTEGER,
        bwidth      INTEGER,
        bheight     INTEGER,
        uri         VARCHAR(256) UNIQUE
      );
      CREATE INDEX targetregion_uri ON targetregion USING hash (uri);
    """.update


  val createTextReflowTable: Update0 = sql"""
      CREATE TABLE textreflow (
        id        SERIAL PRIMARY KEY,
        reflow    JSON,
        zone      INTEGER REFERENCES zone
      )
    """.update


  val createTargetRegionImageTable: Update0 = sql"""
      CREATE TABLE targetregion_image (
        id            SERIAL PRIMARY KEY,
        image         BYTEA,
        targetregion  INTEGER REFERENCES targetregion
      )
    """.update

  def createAll = for {
    _ <- putStrLn("create doc")
    _ <- createDocumentTable.run
    _ <- putStrLn("create page")
    _ <- createPageTable.run
    _ <- putStrLn("create label")
    _ <- createLabelTable.run
    _ <- putStrLn("create targetregion")
    _ <- createTargetRegion.run
    _ <- putStrLn("create zone")
    _ <- createZoneTable.run
    _ <- putStrLn("create z-tr")
    _ <- createZoneToTargetRegion.run
    _ <- putStrLn("create z-lbl")
    _ <- createZoneToLabelTable.run
    _ <- putStrLn("create textreflow")
    _ <- createTextReflowTable.run
    _ <- putStrLn("create tr-image")
    _ <- createTargetRegionImageTable.run
  } yield ()

  val dropAll: Update0 = sql"""
    DROP TABLE IF EXISTS textreflow;
    DROP TABLE IF EXISTS zone_to_targetregion;
    DROP TABLE IF EXISTS zone_to_label;
    DROP TABLE IF EXISTS zone;
    DROP TABLE IF EXISTS label;
    DROP TABLE IF EXISTS targetregion_image;
    DROP TABLE IF EXISTS targetregion;
    DROP TABLE IF EXISTS page;
    DROP TABLE IF EXISTS document;
  """.update

  def dropAndCreate = {
    val run = for{
      _ <- dropAll.run;
      _ <- createAll
    } yield ()
    run.transact(xa)
  }

}
