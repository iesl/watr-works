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



  ////////////////////////////


  // CREATE INDEX zone_idx_label ON zone (label, document, zone);
  // CREATE INDEX zone_idx_label ON zone (label);
  object zonetables {
    val createZoneTables = (for {
      _ <- sql"""
      CREATE TABLE zone (
        zone         SERIAL PRIMARY KEY,
        document     INTEGER REFERENCES document NOT NULL,
        label        INTEGER REFERENCES label NOT NULL,
        rank         INTEGER NOT NULL,
        glyphs       TEXT
      );

      CREATE INDEX zone_idx_document ON zone (document, label);
      CREATE INDEX zone_idx_label ON zone (label, document);


      CREATE TABLE zone_to_targetregion (
        zone          INTEGER REFERENCES zone ON DELETE CASCADE NOT NULL,
        targetregion  INTEGER REFERENCES targetregion NOT NULL,
        rank          INTEGER NOT NULL
      );

      CREATE INDEX zone_to_targetregion_idx2 ON zone_to_targetregion (targetregion, rank);

      CREATE UNIQUE INDEX uniq__zone_to_targetregion ON zone_to_targetregion (zone, targetregion);

      """.update.run

    } yield ())


    def create(): ConnectionIO[Unit] = {
      for {
        _ <- createZoneTables
        _ <- defineOrderingTriggers(fr0"zone_to_targetregion", fr0"targetregion")
        _ <- defineOrderingTriggers(fr0"zone", fr0"document", fr0"label")

      } yield ()
    }
  }



  object targetregions {
    // CREATE INDEX targetregion_bbox ON targetregion (page, bleft, btop, bwidth, bheight);
    val createTargetRegion = (for {
      _ <- sql"""
      CREATE TABLE targetregion (
        targetregion  SERIAL PRIMARY KEY,
        page          INTEGER REFERENCES page,
        rank          INTEGER NOT NULL,
        bleft         INTEGER,
        btop          INTEGER,
        bwidth        INTEGER,
        bheight       INTEGER
      );


      CREATE INDEX targetregion_page_rank ON targetregion (page, rank);

      CREATE TABLE textregion (
        textregion    SERIAL PRIMARY KEY,
        page          INTEGER REFERENCES page,
        rank          INTEGER NOT NULL,
        regions       TEXT
      );

      """.update.run
    } yield ())
    def create(): ConnectionIO[Unit] = {
      for {
        _ <- createTargetRegion
        _ <- defineOrderingTriggers(fr0"targetregion", fr0"page")
      } yield ()
    }
  }


  val createLabelTable: Update0 = sql"""
      CREATE TABLE label (
        label          SERIAL PRIMARY KEY,
        key            VARCHAR(50) UNIQUE NOT NULL
      );
      CREATE INDEX label_key ON label USING hash (key);
    """.update



  object workflowTables {

    val create: Update0 = sql"""
      CREATE TABLE workflow (
        workflow          VARCHAR(32) PRIMARY KEY,
        description       TEXT,
        targetLabel       INTEGER REFERENCES label NOT NULL,
        curatedLabels     INTEGER[]
      );


      CREATE TABLE zonelock (
        zonelock       SERIAL PRIMARY KEY,
        assignee       INTEGER REFERENCES person ON DELETE CASCADE,
        workflow       VARCHAR(32) REFERENCES workflow ON DELETE CASCADE,
        zone           INTEGER REFERENCES zone ON DELETE CASCADE,
        status         VARCHAR(32) NOT NULL
      );
    """.update

  }

  object UserTables {
    val create: Update0 = sql"""
      CREATE TABLE person (
        person      SERIAL PRIMARY KEY,
        email       TEXT NOT NULL
      );
      CREATE UNIQUE INDEX person_email ON person(email);


      CREATE TABLE person_auth (
        person      INTEGER REFERENCES person ON DELETE CASCADE,
        username    TEXT NOT NULL,
        password    TEXT NOT NULL
      );
      CREATE UNIQUE INDEX person_auth_person ON person_auth(person);


      CREATE TABLE token (
        token        SERIAL PRIMARY KEY,
        tuuid        UUID,
        name         TEXT NOT NULL,
        content      TEXT NOT NULL,
        owner        INTEGER REFERENCES person ON DELETE CASCADE
      );
      CREATE UNIQUE INDEX token_tuuid ON token(tuuid);
      CREATE UNIQUE INDEX token_owner ON token(owner);

    """.update
    // expiry       null,
    //   lastTouched  None,
    //   secure       true,
    //   httpOnly     true,
    //   domain        None,
    //   path          None,
    //   extension     None

    val drop = sql"""
        DROP TABLE IF EXISTS person;
        DROP TABLE IF EXISTS person_auth;
        DROP TABLE IF EXISTS token;
    """.update.run
  }


  def createAll = for {
    _ <- createDocumentTable
    _ <- createPageTable
    _ <- createLabelTable.run

    _ <- targetregions.create()
    _ <- zonetables.create()

    _ <- UserTables.create.run
    _ <- workflowTables.create.run
  } yield ()

  val dropAll = sql"""
    DROP TABLE IF EXISTS zone_to_targetregion;
    DROP TABLE IF EXISTS zone_to_label;
    DROP TABLE IF EXISTS zone;
    DROP TABLE IF EXISTS label;
    DROP TABLE IF EXISTS targetregion;
    DROP TABLE IF EXISTS page;
    DROP TABLE IF EXISTS document;
    DROP TABLE IF EXISTS workflow;
    DROP TABLE IF EXISTS zonelock;
    DROP TABLE IF EXISTS person;
    DROP TABLE IF EXISTS person_auth;
    DROP TABLE IF EXISTS token;
  """.update.run

}
