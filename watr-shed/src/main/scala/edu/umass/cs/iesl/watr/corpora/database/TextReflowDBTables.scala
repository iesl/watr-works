package edu.umass.cs.iesl.watr
package corpora
package database

import doobie.imports._
import corpora._

class CorpusAccessDBTables extends DoobieImplicits {

  val Rel = RelationModel

  val createDocumentTable  = (for {
    _ <- sql""" CREATE TABLE document (
                   document      SERIAL PRIMARY KEY,
                   stableId      VARCHAR(128) UNIQUE
                );
             """.update.run

    _ <- sql"CREATE INDEX document_stable_id ON document USING hash (stableId);".update.run
  } yield ())

  val createPageTable = (for {
    _ <- sql"""
      CREATE TABLE page (
        page        SERIAL PRIMARY KEY,
        document    INTEGER REFERENCES document NOT NULL,
        pagenum     SMALLINT,
        imageclip   INTEGER REFERENCES imageclips,
        bleft       INTEGER,
        btop        INTEGER,
        bwidth      INTEGER,
        bheight     INTEGER
      );
      """.update.run

    _ <- sql"""
      CREATE UNIQUE INDEX document_pagenum ON page (document, pagenum);
      """.update.run
  } yield ())

  val createImageClipTable: Update0 = sql"""
      CREATE TABLE imageclips (
        imageclip   SERIAL PRIMARY KEY,
        image       BYTEA
      );
    """.update


  ////////////////////////////
  val createZoneTables = (for {
    _ <- sql"""
      CREATE TABLE zone (
        zone         SERIAL PRIMARY KEY,
        document     INTEGER REFERENCES document NOT NULL,
        label        INTEGER REFERENCES label NOT NULL,
        rank         INTEGER NOT NULL
      );
      """.update.run

    _ <- sql"""
      CREATE INDEX zone_idx_document ON zone (document, label, rank);
      """.update.run

    _ <- sql"""
      CREATE INDEX zone_idx_label ON zone (label);
      """.update.run
    _ <- sql"""
      CREATE TABLE zone_to_targetregion (
        zone          INTEGER REFERENCES zone ON DELETE CASCADE NOT NULL,
        targetregion  INTEGER REFERENCES targetregion NOT NULL,
        rank          INTEGER NOT NULL
      );
    """.update.run
    _ <- sql"""
      CREATE INDEX zone_to_targetregion_idx2 ON zone_to_targetregion (targetregion, rank);
    """.update.run
    _ <- sql"""
      CREATE UNIQUE INDEX uniq__zone_to_targetregion ON zone_to_targetregion (zone, targetregion);
    """.update.run
  } yield ())


  object zonetables {
    def create(): ConnectionIO[Unit] = {
      for {
        _ <- putStrLn("create zone")
        _ <- createZoneTables
        _ <- defineOrderingTriggers(fr0"zone_to_targetregion", fr0"targetregion")
        _ <- defineOrderingTriggers(fr0"zone", fr0"document", fr0"label")
      } yield ()
    }
  }

  /////////
  val createTargetRegion = (for {
    _ <- sql"""
      CREATE TABLE targetregion (
        targetregion  SERIAL PRIMARY KEY,
        page          INTEGER REFERENCES page,
        rank          INTEGER NOT NULL,
        imageclip     INTEGER REFERENCES imageclips,
        bleft         INTEGER,
        btop          INTEGER,
        bwidth        INTEGER,
        bheight       INTEGER
      );
      """.update.run
    _ <- sql"""
      CREATE INDEX targetregion_bbox ON targetregion (page, bleft, btop, bwidth, bheight);
      """.update.run

    _ <- sql"""
      CREATE INDEX targetregion_page_rank ON targetregion (page, rank);
      """.update.run
  } yield ())


  object targetregions {
    def create(): ConnectionIO[Unit] = {
      for {
        _ <- putStrLn("create targetregion")
        _ <- createTargetRegion
        _ <- putStrLn("create targetregion triggers")
        _ <- defineOrderingTriggers(fr0"targetregion", fr0"page")
      } yield ()
    }
  }


  val createTextReflowTable: Update0 = sql"""
      CREATE TABLE textreflow (
        textreflow  SERIAL PRIMARY KEY,
        reflow      TEXT NOT NULL,
        astext      TEXT NOT NULL,
        zone        INTEGER REFERENCES zone ON DELETE CASCADE
      );
      CREATE UNIQUE INDEX textreflow_idx0 ON textreflow (zone);
    """.update


  val createLabelTable: Update0 = sql"""
      CREATE TABLE label (
        label          SERIAL PRIMARY KEY,
        key            VARCHAR(50) UNIQUE NOT NULL
      );
      CREATE INDEX label_key ON label USING hash (key);
    """.update



  object workflowTables {
    val create: Update0 = sql"""
      CREATE TABLE workflows (
        workflow          VARCHAR(32) PRIMARY KEY,
        description       TEXT
      );

      CREATE TABLE lockgroups (
        lockgroup      SERIAL PRIMARY KEY,
        person         INTEGER REFERENCES persons NOT NULL
      );
      CREATE UNIQUE INDEX lockgroups_person ON lockgroups (person);

      CREATE TABLE zonelocks (
        zonelock       SERIAL PRIMARY KEY,
        lockgroup      INTEGER REFERENCES lockgroups ON DELETE SET NULL,
        zone           INTEGER REFERENCES zone,
        status         VARCHAR(32) NOT NULL
      );
    """.update

  }

  object UserTables {
    val create: Update0 = sql"""
      CREATE TABLE persons (
        person      SERIAL PRIMARY KEY,
        email       VARCHAR(128) NOT NULL
      );
      CREATE UNIQUE INDEX persons_email ON persons (email);
    """.update

    val drop = sql"""
        DROP TABLE IF EXISTS persons;
    """.update.run
  }


  def createAll = for {
    _ <- putStrLn("create imageclips")
    _ <- createImageClipTable.run
    _ <- putStrLn("create doc")
    _ <- createDocumentTable
    _ <- putStrLn("create page")
    _ <- createPageTable
    _ <- putStrLn("create label")
    _ <- createLabelTable.run

    _ <- targetregions.create()
    _ <- zonetables.create()

    _ <- putStrLn("create textreflow")
    _ <- createTextReflowTable.run
    _ <- putStrLn("create user tables")
    _ <- UserTables.create.run
    _ <- putStrLn("create workflow tables")
    _ <- workflowTables.create.run
  } yield ()

  val dropAll = sql"""
    DROP TABLE IF EXISTS labelers;
    DROP TABLE IF EXISTS textreflow;
    DROP TABLE IF EXISTS zone_to_targetregion;
    DROP TABLE IF EXISTS zone_to_label;
    DROP TABLE IF EXISTS zone;
    DROP TABLE IF EXISTS label;
    DROP TABLE IF EXISTS targetregion;
    DROP TABLE IF EXISTS page;
    DROP TABLE IF EXISTS imageclips;
    DROP TABLE IF EXISTS document;
    DROP TABLE IF EXISTS workflows;
    DROP TABLE IF EXISTS lockgroups;
    DROP TABLE IF EXISTS zonelocks;
    DROP TABLE IF EXISTS persons;
  """.update.run

}
