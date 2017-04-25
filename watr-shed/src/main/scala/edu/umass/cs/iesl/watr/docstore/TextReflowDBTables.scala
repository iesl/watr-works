package edu.umass.cs.iesl.watr
package docstore

import doobie.imports._
import corpora._

class TextReflowDBTables extends DoobieImplicits {

  val Rel = RelationModel

  val createDocumentTable: Update0 = sql"""
      CREATE TABLE document (
        document      SERIAL PRIMARY KEY,
        stableId      VARCHAR(128) UNIQUE
      );
      CREATE INDEX document_stable_id ON document USING hash (stableId);
    """.update

  val createPageTable: Update0 = sql"""
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
      CREATE UNIQUE INDEX document_pagenum ON page (document, pagenum);
    """.update

  val createImageClipTable: Update0 = sql"""
      CREATE TABLE imageclips (
        imageclip   SERIAL PRIMARY KEY,
        image       BYTEA
      );
    """.update


  ////////////////////////////

  val createZoneTables: Update0 = sql"""
      CREATE TABLE zone (
        zone         SERIAL PRIMARY KEY,
        document     INTEGER REFERENCES document NOT NULL,
        label        INTEGER REFERENCES label NOT NULL,
        rank         INTEGER NOT NULL
      );
      CREATE INDEX zone_idx_document ON zone (document);

      CREATE TABLE zone_to_targetregion (
        zone          INTEGER REFERENCES zone ON DELETE CASCADE NOT NULL,
        targetregion  INTEGER REFERENCES targetregion NOT NULL,
        rank          INTEGER NOT NULL
      );
      CREATE UNIQUE INDEX uniq__zone_to_targetregion ON zone_to_targetregion (zone, targetregion);
      CREATE INDEX zone_to_targetregion_idx2 ON zone_to_targetregion (targetregion);
    """.update

  object zonetables {
    def create(): ConnectionIO[Unit] = {
      for {
        _ <- putStrLn("create zone")
        _ <- createZoneTables.run
        _ <- defineOrderingTriggers(fr0"zone_to_targetregion", fr0"zone")
        _ <- defineOrderingTriggers(fr0"zone", fr0"document", fr0"label")
      } yield ()
    }
  }

  /////////

  val createTargetRegion: Update0 = sql"""
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
      CREATE INDEX targetregion_bbox ON targetregion (page, bleft, btop);
    """.update


  object targetregions {
    def create(): ConnectionIO[Unit] = {
      for {
        _ <- putStrLn("create targetregion")
        _ <- createTargetRegion.run
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

  val createLabelerTable: Update0 = sql"""
      CREATE TABLE labelers (
        labeler     SERIAL PRIMARY KEY,
        widget      TEXT
      );
    """.update

  // title/author
  // created/todo/assigned/skipped/done
  val createLabelingTaskTable: Update0 = sql"""
      CREATE TABLE labelingtasks (
        labelingtask     SERIAL PRIMARY KEY,
        taskname         VARCHAR(128),
        progress         VARCHAR(64),
        labeler          INTEGER REFERENCES labelers
      );
    """.update

  def createAll = for {
    _ <- putStrLn("create imageclips")
    _ <- createImageClipTable.run
    _ <- putStrLn("create doc")
    _ <- createDocumentTable.run
    _ <- putStrLn("create page")
    _ <- createPageTable.run
    _ <- putStrLn("create label")
    _ <- createLabelTable.run

    _ <- targetregions.create()
    _ <- zonetables.create()

    _ <- putStrLn("create textreflow")
    _ <- createTextReflowTable.run
    _ <- putStrLn("create labelers")
    _ <- createLabelerTable.run
    _ <- putStrLn("create labelingtasks")
    _ <- createLabelingTaskTable.run
  } yield ()

  val dropAll: Update0 = sql"""
    DROP TABLE IF EXISTS labelingtasks;
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
  """.update

}
