package edu.umass.cs.iesl.watr
package corpora
package database

import doobie._
import doobie.implicits._
import doobie.free.connection.ConnectionOp

import cats.free._
// import cats._
import cats.implicits._

import corpora._

abstract class TableDef(implicit
  enclosing: sourcecode.Enclosing,
  // pkg: sourcecode.Pkg
) extends DoobiePredef {
  def name(): String = {
    val Array(pre, post) = enclosing.value.split("#")
    post
  }

  def drop(): Update0
  def create(): Update0

  def runDrop(): Free[ConnectionOp, Unit] = for {
    _ <- putStrLn(s"Dropping ${name}")
    _ <- drop().run
  } yield ()

  def runCreate(): Free[ConnectionOp, Unit] = for {
    _ <- putStrLn(s"Creating ${name}")
    _ <- create().run
  } yield ()

  def dropAndCreate(): Free[ConnectionOp, Unit] = for {
    _ <- drop().run
    _ <- create().run
  } yield ()
}


class CorpusAccessDBTables extends DoobieImplicits {

  val Rel = RelationModel

  object documentTables extends TableDef {
    val drop = {
      sql"""
            DROP TABLE IF EXISTS page;
            DROP TABLE IF EXISTS document;
            """.update
    }

    val create = {
      fr"""
           CREATE TABLE document (
                document      SERIAL PRIMARY KEY,
                stableId      VARCHAR(128) UNIQUE
           );

           CREATE INDEX document_stable_id ON document USING hash (stableId);

           CREATE TABLE page (
               page        SERIAL PRIMARY KEY,
               document    INTEGER REFERENCES document ON DELETE CASCADE,
               pagenum     SMALLINT,
               bleft       INTEGER,
               btop        INTEGER,
               bwidth      INTEGER,
               bheight     INTEGER
           );

           CREATE UNIQUE INDEX document_pagenum ON page (document, pagenum);
           """.update
    }
  }


  object labelSchemaTables extends TableDef {

    val drop = sql""" DROP TABLE IF EXISTS labelschema; """.update

    val create =
      sql"""
         CREATE TABLE IF NOT EXISTS labelschema (
           labelschema       SERIAL PRIMARY KEY,
           name              VARCHAR(128) NOT NULL,
           schema            TEXT
         );
         CREATE UNIQUE INDEX labelschema_name ON labelschema (name);
      """.update
  }

  object workflowTables extends TableDef {

    val drop = sql""" DROP TABLE IF EXISTS workflow; """.update

    val create =
      sql"""|
            |  CREATE TABLE IF NOT EXISTS workflow (
            |    workflow          VARCHAR(128) PRIMARY KEY,
            |    labelschema       INTEGER REFERENCES labelschema NOT NULL,
            |    targetPath        LTREE
            |  );
            | CREATE        INDEX workflow_path_gist  ON workflow using gist(targetPath);
            | CREATE UNIQUE INDEX workflow_path_btree ON workflow using btree(targetPath);
            |""".stripMargin.update

  }

  object documentLockTables extends TableDef {

    val drop = sql""" DROP TABLE IF EXISTS corpuslock; """.update

    val create: Update0 = sql"""
      CREATE TABLE IF NOT EXISTS corpuslock (
        corpuslock  SERIAL PRIMARY KEY,
        holder      INTEGER REFERENCES person ON DELETE SET NULL,
        document    INTEGER REFERENCES document ON DELETE CASCADE,
        lockPath    LTREE,
        status      VARCHAR(128)
      );
       CREATE INDEX        corpuslock_path_gist ON corpuslock using gist(lockPath);
    """.update
  }



  object annotationTables extends TableDef {
    val drop = sql""" DROP TABLE IF EXISTS annotation; """.update

    val create = sql"""
        CREATE TABLE IF NOT EXISTS annotation (
          annotation     SERIAL PRIMARY KEY,
          document       INTEGER REFERENCES document,
          owner          INTEGER REFERENCES person DEFAULT NULL,
          annotPath      LTREE DEFAULT NULL,
          created        TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
          body           TEXT DEFAULT NULL
        );

        CREATE INDEX annotation_document ON annotation(document);
        CREATE INDEX annotation_owner ON annotation(owner);
        CREATE INDEX annotation_annotPath_gist ON annotation using gist (annotPath);
        CREATE INDEX annotation_annotPath      ON annotation using btree (annotPath);
    """.update

  }

  object corpusPathTables extends TableDef {

    val drop = sql"""
        DROP TABLE IF EXISTS pathentry;
        DROP TABLE IF EXISTS corpuspath;
    """.update

    val create = sql"""
        CREATE TABLE IF NOT EXISTS corpuspath (
          corpuspath   SERIAL PRIMARY KEY,
          path         LTREE
        );

        CREATE INDEX        corpuspath_path_gist ON corpuspath using gist(path);
        CREATE UNIQUE INDEX corpuspath_path      ON corpuspath using btree(path);

        CREATE TABLE IF NOT EXISTS pathentry (
          document      INTEGER REFERENCES document ON DELETE CASCADE,
          corpuspath    INTEGER REFERENCES corpuspath
        );
        CREATE UNIQUE INDEX pathentry_document ON pathentry (document);
        CREATE INDEX pathentry_corpuspath ON pathentry (corpuspath);
    """.update

  }

  object userTables extends TableDef {
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


    val drop = sql"""
        DROP TABLE IF EXISTS person_auth;
        DROP TABLE IF EXISTS token;
        DROP TABLE IF EXISTS person;
    """.update
  }


  val CorpusTableDefs = List[TableDef](
    documentTables,
    labelSchemaTables,
    workflowTables,
    documentLockTables,
    annotationTables,
    corpusPathTables
  )

  val UserTableDefs = List[TableDef](
    userTables
  )


  val AllTableDefs = UserTableDefs ++ CorpusTableDefs

  def createAll(): Free[ConnectionOp, Unit] =
    (UserTableDefs ++ CorpusTableDefs)
    .map { _.runCreate() }
    .reduce { _ >> _ }

  def dropAll(): Free[ConnectionOp, Unit] = (CorpusTableDefs.reverse ++ UserTableDefs)
    .map { _.runDrop() }
    .reduce { _ >> _ }

}

  ////////////////////////////


  // object zonetables {
  //   val createZoneTables = for {
  //     _ <-
  //       sql"""
  //     CREATE TABLE zone (
  //       zone         SERIAL PRIMARY KEY,
  //       document     INTEGER REFERENCES document ON DELETE CASCADE,
  //       label        INTEGER REFERENCES label NOT NULL,
  //       rank         INTEGER NOT NULL,
  //       glyphs       TEXT
  //     );

  //     CREATE INDEX zone_idx_document ON zone (document, label);
  //     CREATE INDEX zone_idx_label ON zone (label, document);

  //     CREATE TABLE zone_to_targetregion (
  //       zone          INTEGER REFERENCES zone ON DELETE CASCADE NOT NULL,
  //       targetregion  INTEGER REFERENCES targetregion NOT NULL,
  //       rank          INTEGER NOT NULL
  //     );

  //     CREATE INDEX zone_to_targetregion_idx2 ON zone_to_targetregion (targetregion, rank);

  //     CREATE UNIQUE INDEX uniq__zone_to_targetregion ON zone_to_targetregion (zone, targetregion);

  //     """.update.run

  //   } yield ()


  //   def create(): ConnectionIO[Unit] = {
  //     for {
  //       _ <- createZoneTables
  //       _ <- defineOrderingTriggers(fr0"zone_to_targetregion", fr0"targetregion")
  //       _ <- defineOrderingTriggers(fr0"zone", fr0"document", fr0"label")

  //     } yield ()
  //   }
  // }



  // object targetregions {
  //   // CREATE INDEX targetregion_bbox ON targetregion (page, bleft, btop, bwidth, bheight);
  //   val createTargetRegion = for {
  //     _ <- sql"""
  //     CREATE TABLE targetregion (
  //       targetregion  SERIAL PRIMARY KEY,
  //       page          INTEGER REFERENCES page ON DELETE CASCADE,
  //       rank          INTEGER NOT NULL,
  //       bleft         INTEGER,
  //       btop          INTEGER,
  //       bwidth        INTEGER,
  //       bheight       INTEGER
  //     );

  //     CREATE INDEX targetregion_page_rank ON targetregion (page, rank);

  //     """.update.run
  //   } yield ()
  //   def create(): ConnectionIO[Unit] = {
  //     for {
  //       _ <- createTargetRegion
  //       _ <- defineOrderingTriggers(fr0"targetregion", fr0"page")
  //     } yield ()
  //   }
  // }


  // val createLabelTable: Update0 = sql"""
  //     CREATE TABLE label (
  //       label          SERIAL PRIMARY KEY,
  //       key            VARCHAR(128) UNIQUE NOT NULL
  //     );
  //     CREATE INDEX label_key ON label USING hash (key);
  //   """.update

// object zonelockTables {

//   val drop = sql""" DROP TABLE IF EXISTS zonelock; """.update

//   val create: Update0 = sql"""

//     CREATE TABLE IF NOT EXISTS zonelock (
//       zonelock       SERIAL PRIMARY KEY,
//       assignee       INTEGER REFERENCES person ON DELETE CASCADE,
//       workflow       VARCHAR(128) REFERENCES workflow ON DELETE CASCADE,
//       zone           INTEGER REFERENCES zone ON DELETE CASCADE,
//       status         VARCHAR(128) NOT NULL
//     );
//   """.update

// }
