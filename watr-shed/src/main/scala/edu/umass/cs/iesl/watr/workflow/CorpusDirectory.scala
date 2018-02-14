package edu.umass.cs.iesl.watr
package workflow

/**
  * Create corpus directories, one per PDF
  * Gold, batch#1, batch#2/a20, batch#2/a80
  *
  * Zonelocks are temporary; linking a user, zone, and curation status
  *
  * Create an annotation table
  * Mostly the same as zone table, but relates  annotation, user, date-created/updated, workflow,...
  * Annotation= ordered rectangles + glyphs + creator(person) + corpus-path for underlying pdf
  */

import doobie.implicits._

import corpora.database.{CorpusAccessDB, DoobieImplicits}
import TypeTags._

object cpath {
  def apply(s: String): String@@CorpusPath = CorpusPath(s)

  implicit class cpath_RicherString(val self: String) extends AnyVal {
    def path(): String@@CorpusPath = apply(self)
  }

}

class DatabaseCorpusDirectory()(implicit
  corpusAccessDB: CorpusAccessDB
) extends DoobieImplicits {


  def makeDirectory(path: String@@CorpusPath): Either[String, Unit] =  {
    corpusAccessDB.runq {
      sql"""
          INSERT INTO corpuspath (path) VALUES ( text2ltree(${path}) )
      """.update.run
    }
    Right((): Unit)
  }

  def removeDirectory(path: String@@CorpusPath): Either[String, Unit] = {
    corpusAccessDB.runq {
      sql"""
         DELETE FROM corpuspath WHERE path beginsWith ${path}
      """.update.run
    }
    Right((): Unit)

  }

  private def exists_(path: String@@CorpusPath): Boolean =  {
    corpusAccessDB.runq {
      sql"""
         SELECT 1 FROM corpuspath WHERE path = text2ltree(${path})
      """.query[Int].option
    }.isDefined
  }

  def exists(path: String@@CorpusPath): Boolean =  {
    exists_(path)
  }

  def renameDirectory(from: String@@CorpusPath, to: String@@CorpusPath): Either[String, Unit] =  {
    ???
  }

  private val EmptyPath = CorpusPath("")

  def listEntries(path: String@@CorpusPath = EmptyPath): Either[String, Seq[String@@DocumentID]] =  {
    if (path==EmptyPath) {
      Right( corpusAccessDB.docStore.getDocuments() )
    } else {
      Right(
        corpusAccessDB.runq {
          sql"""
             SELECT d.stableId
             FROM corpuspath cp
             JOIN pathentry pe ON (cp.corpuspath=pe.corpuspath)
             JOIN document d ON (d.document=pe.document)
             WHERE cp.path = text2ltree(${path})
          """.query[String@@DocumentID].to[Vector]
        }
      )
    }
  }

  def listDirectories(): Seq[String@@CorpusPath] =  {
    corpusAccessDB.runq {
      sql""" SELECT p.path FROM corpuspath p """
        .query[String@@CorpusPath].to[Vector]
    }
  }


  def moveEntry(entry: String@@DocumentID, path: String@@CorpusPath): Either[String, Unit] =  {

    if (exists_(path)) {
      corpusAccessDB.runq {
        sql"""
           WITH docid as(
               select document from document where stableId=${entry}
           ),
           cpath AS (
               select corpuspath from corpuspath where path=text2ltree(${path})
           ),
           upsert AS (
             UPDATE pathentry
             SET
               corpuspath = (select * from cpath)
             WHERE document = (select * from docid)
             RETURNING *
           )
           INSERT INTO pathentry (document, corpuspath)
             SELECT (select * from docid), (select * from cpath)
             WHERE NOT EXISTS (SELECT * FROM upsert)
      """.update.run

      }
      Right((): Unit)

    } else Left(s"Path ${path} doesn't exists")

  }
}
