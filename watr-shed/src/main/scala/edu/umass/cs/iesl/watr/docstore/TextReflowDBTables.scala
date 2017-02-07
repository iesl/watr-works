package edu.umass.cs.iesl.watr
package docstore

import doobie.imports._
import scalaz.concurrent.Task

import databasics._

class TextReflowDBTables(
  val xa: Transactor[Task]
) extends DoobiePredef {

  val createDocumentTable: Update0 = sql"""
      CREATE TABLE document (
        document      SERIAL PRIMARY KEY,
        stable_id     VARCHAR(128) UNIQUE
      );
      CREATE INDEX document_stable_id ON document USING hash (stable_id);
    """.update

  val createPageTable: Update0 = sql"""
      CREATE TABLE page (
        page        SERIAL PRIMARY KEY,
        document    INTEGER REFERENCES document,
        pagenum     SMALLINT,
        pageimg     BYTEA,
        bleft       INTEGER,
        btop        INTEGER,
        bwidth      INTEGER,
        bheight     INTEGER
      );
    """.update

  val createZoneTable: Update0 = sql"""
      CREATE TABLE zone (
        zone        SERIAL PRIMARY KEY,
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
        targetregion  SERIAL PRIMARY KEY,
        page          INTEGER REFERENCES page,
        bleft         INTEGER,
        btop          INTEGER,
        bwidth        INTEGER,
        bheight       INTEGER,
        uri           VARCHAR(256) UNIQUE
      );
      CREATE INDEX targetregion_uri ON targetregion USING hash (uri);
    """.update


  val createTextReflowTable: Update0 = sql"""
      CREATE TABLE textreflow (
        textreflow  SERIAL PRIMARY KEY,
        reflow      TEXT,
        zone        INTEGER REFERENCES zone
      )
    """.update


  val createTargetRegionImageTable: Update0 = sql"""
      CREATE TABLE targetregion_image (
        targetregion_image     SERIAL PRIMARY KEY,
        image                  BYTEA,
        targetregion           INTEGER REFERENCES targetregion
      )
    """.update

  // insert into page (document, pagenum, pageimg, bleft, btop, bwidth, bheight)
  val createLabelTable: Update0 = sql"""
      CREATE TABLE label (
        label          SERIAL PRIMARY KEY,
        key            VARCHAR(50) UNIQUE
      );
      CREATE INDEX label_key ON label USING hash (key);
    """.update

  // val createLabelerTable: Update0 = sql"""
  //     CREATE TABLE labelers (
  //       labeler     SERIAL PRIMARY KEY,
  //       widget      TEXT
  //     );
  //   """.update
  // val createLabelProgressTable: Update0 = sql"""
  //     CREATE TABLE labelingtasks (
  //       labelingtask     SERIAL PRIMARY KEY,
  //       taskname         VARCHAR(128), // title/author
  //       progress         VARCHAR(64)// created/todo/assigned/skipped/done
  //       labeler          INTEGER REFERENCES labelers
  //     );
  //   """.update

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

  def dropAndCreateAll = for{
    _ <- dropAll.run
    _ <- createAll
  } yield ()

  def dropAndCreate = {
    val run = for{
      _ <- dropAll.run
      _ <- createAll
    } yield ()
    run.transact(xa)
  }

}
