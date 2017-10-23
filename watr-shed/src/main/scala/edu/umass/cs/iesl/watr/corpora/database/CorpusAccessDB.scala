package edu.umass.cs.iesl.watr
package corpora
package database

import doobie.imports._
import doobie.free.{ connection => C }
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Properties
import scalaz.syntax.applicative._
import scalaz.std.list._
import scalaz.syntax.traverse._
// import scalaz.syntax.apply._

import geometry._
import corpora._
import workflow._
// import labeling._

import textreflow._
import textreflow.data._
import watrmarks._
import TypeTags._
import play.api.libs.json, json._
import shapeless._

class CorpusAccessDB(
  dbname: String, dbuser: String, dbpass: String
) extends DoobieImplicits  { self =>


  val tables: CorpusAccessDBTables = new CorpusAccessDBTables()

  val Rel = RelationModel

  def time[R](prefix: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"${prefix}: " + (t1 - t0)/1000000.0d + "ms")
    result
  }

  import scalaz.concurrent.Task
  import doobie.hikari.imports._


  val props = new Properties()
  props.setProperty("housekeeper","false")

  def xa0: HikariTransactor[Task] = (for {
    xa <- HikariTransactor[Task]("com.impossibl.postgres.jdbc.PGDriver", s"jdbc:pgsql:${dbname}", dbuser, dbpass)
    _  <- xa.configure(hx => Task.delay{
      hx.setDataSourceProperties(props)
      hx.setAutoCommit(false)
      ()
    })
  } yield {
    xa
  }).unsafePerformSync

  var xa: HikariTransactor[Task] = xa0

  def reinit() = {
    xa = xa0
  }

  def shutdown() = (for {
    r <- sql"select 1".query[Int].unique.transact(xa) ensuring xa.shutdown
  } yield r).unsafePerformSync

  def runq[A](query: C.ConnectionIO[A]): A = {
    try {
      (for {
        r <- query.transact(xa)
      } yield r).unsafePerformSync
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
        throw t
    }
  }
  def runqOnce[A](query: C.ConnectionIO[A]): A = {
    try {
      (for {
        xa0 <- HikariTransactor[Task]("org.postgresql.Driver", s"jdbc:postgresql:${dbname}", dbuser, dbpass)
        r <- query.transact(xa0) ensuring xa0.shutdown
      } yield r).unsafePerformSync
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
        throw t
    }
  }

  def updateGetKey[A: Composite](key: String, up: Update0): C.ConnectionIO[A] = {
    up.withUniqueGeneratedKeys(key)
  }


  def dropTables() = runqOnce {
    tables.dropAll
  }

  def dropAndRecreate() = runqOnce {
    for{
      _ <- tables.dropAll
      _ <- tables.createAll
    } yield ()
  }


  def selectTargetRegion(regionId: Int@@RegionID): ConnectionIO[Rel.TargetRegion] = {
    sql""" select * from targetregion where targetregion=${regionId} """
      .query[Rel.TargetRegion].unique
  }

  def selectTargetRegionForBbox(pageId: Int@@PageID, bbox: LTBounds): ConnectionIO[Option[Rel.TargetRegion]] = {
    val LTBounds.IntReps(l, t, w, h) = bbox

    sql"""select * from targetregion where
       page=${pageId} AND
       bleft=${l} AND btop=${t} AND
       bwidth=${w} AND bheight=${h}
       order by rank
    """.query[Rel.TargetRegion].option
  }

  def selectTargetRegions(pageId: Int@@PageID): ConnectionIO[List[Int@@RegionID]] = {
    sql""" select targetregion from targetregion where page=${pageId} order by rank"""
      .query[Int@@RegionID].list
  }

  def insertZone(docId: Int@@DocumentID, labelId: Int@@LabelID): ConnectionIO[Int@@ZoneID] = {
    sql""" insert into zone (document, label) values ($docId, $labelId) """
      .update.withUniqueGeneratedKeys[Int]("zone")
      .map(ZoneID(_))
  }

  def selectZoneLabelsForDocument(docId: Int@@DocumentID): ConnectionIO[List[Int@@LabelID]] = {
    sql""" select distinct label from  zone
           where document=${docId}
       """.query[Int@@LabelID].list
  }

  def selectZonesForDocument(docId: Int@@DocumentID, labelId: Int@@LabelID): ConnectionIO[List[Int@@ZoneID]] = {
    sql""" select * from  zone where document=${docId} AND label=${labelId} order by rank"""
      .query[Int@@ZoneID].list
  }

  def selectZone(zoneId: Int@@ZoneID): ConnectionIO[Rel.Zone] = {
    sql""" select * from  zone where zone=${zoneId} order by rank """
      .query[Rel.Zone].unique
  }

  def delZone(zoneId: Int@@ZoneID): ConnectionIO[Int] = {
    sql""" delete from zone where zone=${zoneId} """.update.run
  }

  def selectTextReflow(textReflowId: Int@@TextReflowID): ConnectionIO[Rel.TextReflow] = {
    sql"""
     select * from  textreflow
     where textreflow=${textReflowId}
    """.query[Rel.TextReflow].unique
  }


  def insertTextReflow(zonePk: Int@@ZoneID, textReflowJs: String, asText: String): ConnectionIO[Int] = {
    for {
      pk <- sql"""
       insert into textreflow (reflow, astext, zone)
       values (${textReflowJs}, $asText, ${zonePk})
      """.update.withUniqueGeneratedKeys[Int]("textreflow")
    } yield pk
  }

  def selectZoneTargetRegions(zoneId: Int@@ZoneID): ConnectionIO[List[Int@@RegionID]] = {
    sql"""
     select targetregion
     from
        zone_to_targetregion
     where
        zone=${zoneId}
     order by rank
    """.query[Int@@RegionID].list
  }


  def selectModelTextReflowForZone(zoneId: Int@@ZoneID): ConnectionIO[Option[Rel.TextReflow]] = {
    val query = sql"""
     select tr.*
     from   textreflow as tr join zone as z on (tr.zone=z.zone)
     where  z.zone=${zoneId}
    """.query[Rel.TextReflow].option

    query
  }

  def selectTextReflowForZone(zoneId: Int@@ZoneID): ConnectionIO[Option[TextReflow]] = {
    import play.api.libs.json, json._

    val query = sql"""
     select tr.reflow
     from   textreflow as tr join zone as z on (tr.zone=z.zone)
     where  z.zone=${zoneId}
    """.query[String]
      .option
      .map { maybeStr =>
        maybeStr.flatMap { str =>
          docStore.jsonToTextReflow(Json.parse(str))
        }
      }

    query
  }

  def updatePage(pageId: Int@@PageID, geom: LTBounds): ConnectionIO[Int] = {
    val LTBounds.IntReps(l, t, w, h) = geom

    sql"""
       update page set bleft=$l, btop=$t, bwidth=$w, bheight=$h where page=${pageId}
    """.update.run
  }

  def insertPage(docPrKey: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Int@@PageID] = {
    updateGetKey("page",
      sql""" insert into page
                (document, pagenum, imageclip, bleft, btop, bwidth, bheight)
         values (${docPrKey}, ${pageNum}, null, 0, 0, 0, 0) """.update
    )
  }

  def selectPages(docId: Int@@DocumentID): ConnectionIO[List[Int@@PageID]] = {
    sql"""
     select pg.page
     from   page as pg
            join document as d on (pg.document=d.document)
     where d.document = ${docId}
     order by pg.pagenum
    """.query[Int]
      .map(PageID(_))
      .list
  }

  def selectPageAndDocument(pageId: Int@@PageID): ConnectionIO[(Rel.Page, Rel.Document)] = {
    // type Ret = Rel.Page :: Rel.Document :: HNil
    sql"""
     select pg.*, d.*
     from   page as pg join document as d on (pg.document=d.document)
     where  d.document=pg.document AND pg.page=${pageId}
    """.query[(Rel.Page, Rel.Document)]
      .unique
  }

  def selectPageForDocument(docId: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Int@@PageID] = {
    sql"""
     select pg.page
     from   page as pg join document as d on (pg.document=d.document)
     where  d.document=${docId} AND pg.pagenum=${pageNum}
    """.query[Int@@PageID].unique
  }

  def selectPage(pageId: Int@@PageID): ConnectionIO[Rel.Page] = {
    sql"""
     select pg.page, pg.document, pg.pagenum, pg.imageclip, pg.bleft, pg.btop, pg.bwidth, pg.bheight
     from   page as pg
     where  pg.page=${pageId.unwrap}
    """.query[Rel.Page].unique
  }

  def selectPageImage(pageId: Int@@PageID): ConnectionIO[Option[Array[Byte]]] = {
    sql"""select i.image
          from page as p join imageclips as i
               on(p.imageclip=i.imageclip)
          where p.page=${pageId}
       """.query[Array[Byte]].option
  }

  def selectTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Option[Array[Byte]]] = {
    sql"""select i.image
          from targetregion as tr join imageclips as i
               on(tr.imageclip=i.imageclip)
          where tr.targetregion=${regionId}
       """.query[Array[Byte]].option
  }

  def insertImageClip(imageBytes: Array[Byte]): ConnectionIO[Int] = {
    sql"""insert into imageclips (image) values(${imageBytes})"""
      .update.withUniqueGeneratedKeys[Int]("imageclip")
  }

  def deleteImageClip(clipId: Int@@ImageID): ConnectionIO[Int] = {
    sql"""delete from imageclips where imageclip=${clipId}""".update.run
  }

  def delTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Unit] = {
    for {
      tr <- selectTargetRegion(regionId)
      del <- {
        tr.imageclip.fold(FC.delay { 0 })(
          deleteImageClip(_)
        )
      }
      pup <- sql"""update targetregion set imageclip=null where targetregion=${regionId}""".update.run
    } yield ()
  }

  def updatePageImage(pageId: Int@@PageID, imageBytes: Array[Byte]): ConnectionIO[Int] = {
    for {
      page <- selectPage(pageId)
      del <- {
        page.imageclip.fold(FC.delay { 0 })(
          deleteImageClip(_)
        )
      }
      clipId <- insertImageClip(imageBytes)
      pup <- sql"""update page set imageclip=${clipId} where page=${pageId}""".update.run
    } yield pup

  }

  def updatePageGeometry(pageId: Int@@PageID, pageBounds: LTBounds): Update0 = {
    val LTBounds.IntReps(bl, bt, bw, bh) = pageBounds

    sql"""
       update page set bleft=${bl}, btop=${bt}, bwidth=${bw}, bheight=${bh} where page = ${pageId}
    """.update
  }


  def insertDocumentID(stableId: String@@DocumentID): ConnectionIO[Int@@DocumentID] = {
    sql""" insert into document (stableId) values (${stableId}) """.update
      .withUniqueGeneratedKeys[Int]("document")
      .map(DocumentID(_))
  }



  def selectDocumentID(stableId: String@@DocumentID): ConnectionIO[Int@@DocumentID] = {
    sql"select document from document where stableId=${stableId}"
      .query[Int].unique.map(DocumentID(_))
  }

  def selectDocument(docId: Int@@DocumentID): ConnectionIO[Rel.Document] = {
    sql"select * from document where document=${docId}"
      .query[Rel.Document].unique
  }

  def hasDocumentID(stableId: String@@DocumentID): Boolean = {
    val zz = runq{
      sql"select 1 from document where stableId=${stableId}"
        .query[Int].option
    }
    zz.isDefined
  }

  def getOrInsertDocumentID(stableId: String@@DocumentID): ConnectionIO[Int@@DocumentID] = {
    sql"""select document from document where stableId=${stableId}"""
      .query[Int].option
      .flatMap({
        case Some(pk) => FC.delay(DocumentID(pk))
        case None => insertDocumentID(stableId)
      })
  }

  def selectLabel(labelId: Int@@LabelID): ConnectionIO[Rel.Label] = {
    sql"""select * from label where label=${labelId}""".query[Rel.Label].unique
  }

  def getOrInsertLabel(label: Label): ConnectionIO[Int@@LabelID] = for {
    maybePk <- sql"""select label from label where key=${label.fqn}""".query[Int].option
    pk      <- maybePk match {
      case Some(id)    => id.point[ConnectionIO]
      case None        =>
        sql"""insert into label (key) values (${label.fqn})""".update
          .withUniqueGeneratedKeys[Int]("label")
    }
  } yield LabelID(pk)


  def linkZoneToTargetRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): ConnectionIO[Unit] = {
    sql"""
       insert into zone_to_targetregion (zone, targetregion)
       values (${zoneId}, ${regionId})
    """.update.run.map(_ => ())
  }


  def selectPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Option[Int@@PageID]] = {
    sql"""select page from page where document=${docId} and pagenum=${pageNum}"""
      .query[Int]
      .option
      .map(_.map(PageID(_)))
  }


  def ensureTargetRegion(pageId: Int@@PageID, bbox: LTBounds): ConnectionIO[Int@@RegionID] = {
    val LTBounds.IntReps(bl, bt, bw, bh) = bbox

    for {
      page         <- selectPage(pageId)
      maybeRegion  <- selectTargetRegionForBbox(pageId, bbox)
      regionId     <- maybeRegion.fold(
        sql"""insert into targetregion (page, imageclip, bleft, btop, bwidth, bheight)
                 values ($pageId, null, $bl, $bt, $bw, $bh)
             """.update.withUniqueGeneratedKeys[Int]("targetregion").map(RegionID(_))
      )(tr => FC.delay(tr.prKey))
    } yield regionId
  }

  def createTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Array[Byte]] = {
    // val TargetRegion(regionId, stableId, pageNum, bbox) = targetRegion

    def cropTo(bs: Array[Byte], cbbox: LTBounds, pbbox: LTBounds): ConnectionIO[Array[Byte]] = {
      images.ImageManipulation.cropTo(bs, cbbox, pbbox)
        .bytes
        .point[ConnectionIO]
    }

    for {
      _              <- putStrLn(s"createTargetRegionImage(${regionId})")
      _              <- putStrLn(s"  selectTargetRegion(${regionId})")
      region         <- selectTargetRegion(regionId)
      _              <- putStrLn(s"  selectPage(${region})")
      page           <- selectPage(region.page)
      _              <- putStrLn(s"  selectPageImage(${page})")

      // Get page image from filesystem
      maybePageImage <- selectPageImage(region.page)

      imageBytes     <- maybePageImage.map(cropTo(_, region.bounds, page.bounds)).getOrElse { sys.error(s"  createTargetRegionImage: no page image found!") }
      // TODO delete old image if exists
      _              <- putStrLn("  insertTargetRegionImage()")
      clipId         <- insertTargetRegionImage(regionId, imageBytes)
    } yield {
      imageBytes
    }
  }

  def insertTargetRegionImage(regionId: Int@@RegionID, imageBytes: Array[Byte]): ConnectionIO[Int@@ImageID] = {
    for {
      clipId <- insertImageClip(imageBytes)
      pup <- sql"""update targetregion set imageclip=${clipId} where targetregion=${regionId}""".update.run
    } yield ImageID(clipId)
  }


  def getOrCreateTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Array[Byte]] = {
    for {
      _          <- putStrLn(s"getOrCreateTargetRegionImage(${regionId})")
      _          <- putStrLn("  selectTargetRegionImage()")
      maybeImage <- selectTargetRegionImage(regionId)
      _          <- putStrLn("  maybe createTargetRegionImage()")
      imageBytes <- maybeImage.fold(createTargetRegionImage(regionId))(_.point[ConnectionIO])
    } yield imageBytes
  }


  def serveTargetRegionImage(regionId: Int@@RegionID): Array[Byte] = {
    println(s"serveTargetRegionImage(${regionId})")
    runq{
      getOrCreateTargetRegionImage(regionId)
    }
  }

  def getPageAndDocument(pageId: Int@@PageID): (Rel.Page, Rel.Document) = {
    runq{ selectPageAndDocument(pageId) }
  }

  def getCorpusEntryPath(pageId: Int@@PageID): Path = {
    val query = sql"""
       select
          d.stableId
       from
          page as pg
          join document as d on (pg.document=d.document)
       where
          pg.page = ${pageId}
    """.query[String]
      .map(str => Paths.get(str))
      .unique

    runq { query }
  }

  object userbaseApi extends UserbaseApi {
    def addUser(email: String): Int@@UserID = {
      runq { sql"""
        insert into persons (email) values (${email})
        """.update
        .withUniqueGeneratedKeys[Int]("person")
        .map(UserID(_))
      }
    }

    def getUser(userId: Int@@UserID): Option[Rel.Person] = {
      runq { sql"""
        select person, email from persons where person=${userId}
        """.query[Rel.Person].option
      }
    }

    def getUserByEmail(email: String): Option[Int@@UserID] = {
      runq { sql"""
        select person from persons where email=${email}
        """.query[Int@@UserID].option
      }
    }
  }
  object workflowApi extends WorkflowApi {
    def defineWorkflow(slug: String, desc: String): String@@WorkflowID = {
      // select workflow from workflows where workflow=${slug}
      val query = sql"""
         insert into workflows (workflow, description) values (${slug}, ${desc})
         returning workflow
      """.query[String@@WorkflowID].unique
      runq{ query }
    }

    def activateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit] = {
      ???
    }

    def deactivateWorkflow(workflowId:String@@WorkflowID): Either[String, Unit] = {
      ???
    }

    def deleteWorkflow(workflowId:String@@WorkflowID): Either[String, Unit] = {
      ???
    }

    def getWorkflows(): Seq[String@@WorkflowID] = {
      val query = sql"""
          select workflow from workflows
      """.query[String@@WorkflowID].list
      runq{ query }
    }

    def getWorkflow(workflowId:String@@WorkflowID): Rel.WorkflowDef = {
      runq { sql"""
        select workflow, description from workflows
        """.query[Rel.WorkflowDef].unique
      }
    }

    def makeLockGroup(user: Int@@UserID): Int@@LockGroupID = {
      runq { sql"""
        insert into lockgroups (person) values (${user})
        """.update.withUniqueGeneratedKeys[Int]("lockgroup").map(LockGroupID(_))
      }
    }

    def aquireZoneLocks(lockGroup: Int@@LockGroupID, labelId: Int@@LabelID, count: Int): Seq[Int@@ZoneLockID] = {
      runq {
        for {
          q1 <- sql"""
              insert into zonelocks (lockgroup, zone, status)
                select ${lockGroup.unwrap}, z.zone, ${ZoneLockStatus.Unexamined}
                  from      zone as z
                  left join zonelocks as lk using (zone)
                  where  z.label=${labelId}
                    AND  lk.zone is null
                  limit ${count}
             """.update.withGeneratedKeys[Int]("zonelock").map(ZoneLockID(_)).runLog

        } yield q1
      }
    }

    def aquireZoneLocksWithStatus(lockGroupId: Int@@LockGroupID, withStatus: String@@StatusCode, count: Int): Seq[Int@@ZoneLockID] = {
      runq { for {
        q1 <- sql"""
          update zonelocks SET lockgroup=${lockGroupId}
              where zonelock IN (
               select zonelock from zonelocks
                 where lockgroup IS NULL AND status=${withStatus}
                 limit ${count}
              )
          """.update.run

        q2 <- sql"""
          select zonelock from zonelocks where lockgroup = ${lockGroupId} AND status=${withStatus}
          """.query[Int@@ZoneLockID].list

        } yield q2
      }
    }

    def updateZoneStatus(zoneLockId: Int@@ZoneLockID, newStatus: String@@StatusCode): Unit = {
      runq { sql"""
        update zonelocks SET status=${newStatus}
            where zonelock=${zoneLockId}
        """.update.run
      }
    }

    def releaseZoneLocks(lockGroupId: Int@@LockGroupID): Unit = {
      runq { sql"""
        delete from lockgroups where lockgroup=${lockGroupId}
        """.update.run
      }
    }

    def getLockForZone(zoneId: Int@@ZoneID): Option[Rel.ZoneLock] = {
      runq { sql"""
        select * from zonelocks where zone=${zoneId}
        """.query[Rel.ZoneLock].option
      }
    }

    def getZoneLock(zoneLockId: Int@@ZoneLockID): Option[Rel.ZoneLock] = {
      runq { sql"""
        select * from zonelocks where zonelock=${zoneLockId}
        """.query[Rel.ZoneLock].option
      }
    }

    def getUserLockGroup(userId: Int@@UserID): Option[Int@@LockGroupID] = {
      runq { sql"""
        select lockgroup from lockgroups where person=${userId}
        """.query[Int@@LockGroupID].option
      }
    }

    def getLockedZones(lockGroupId: Int@@LockGroupID): Seq[Int@@ZoneLockID] = {
      runq { sql"""
        select zone from zonelocks where lockgroup=${lockGroupId}
        """.query[Int@@ZoneLockID].list
      }
    }

  }

  def setZoneOrder(zoneId: Int@@ZoneID, newRank: Int): Unit = {
    runq { sql"""
        with zdoc as (
             select document from zone where zone=${zoneId}
        ),
        zlabel as (
             select label from zone where zone=${zoneId}
        ),
        rerank as (
            update zone SET rank = rank+1
            where document=(select document from zdoc)
              AND label=(select label from zlabel)
              AND rank >= ${newRank}
            returning ${newRank}

        )
        update zone SET rank=(select * from rerank) where zone=${zoneId}
        """.update.run
    }
  }


  object docStore extends DocumentZoningApi {
    def workflowApi: WorkflowApi = self.workflowApi
    def userbaseApi: UserbaseApi = self.userbaseApi

    /** As seen from object docStore, the missing signatures are as follows.
      *  For convenience, these are usable as stub implementations.
      */
    def getPageText(pageId: Int @@ edu.umass.cs.iesl.watr.PageID): Option[edu.umass.cs.iesl.watr.textgrid.TextGrid] = ???
    def setPageText(pageId: Int @@ edu.umass.cs.iesl.watr.PageID,text: edu.umass.cs.iesl.watr.textgrid.TextGrid): Unit = ???

    // def listDocuments(n: Int=Int.MaxValue, skip: Int=0, labelFilters: Seq[Label]): Seq[(String@@DocumentID, Seq[(Label, Int)])] = {
    //   val query = if (labelFilters.isEmpty) {
    //     sql"select stableId from document limit $n offset $skip".query[String@@DocumentID].list
    //   } else {
    //     val filters = labelFilters.toList.map{ l =>
    //       s""" l.key='${l.fqn}' """
    //     }.mkString(" OR ")

    //     val qstr = {
    //       s"""| select distinct d.stableId, group(l.key)
    //           | from document as d
    //           |   join zone as z on (d.document=z.document)
    //           |   join label as l on (z.label=l.label)
    //           | where ${filters}
    //           | limit $n offset $skip
    //           |""".stripMargin
    //     }

    //     Query0[(String@@DocumentID, Seq[(Label, Int)])](qstr).list
    //   }

    //   runq { query }
    // }

    def getDocuments(n: Int=Int.MaxValue, skip: Int=0, labelFilters: Seq[Label]): Seq[String@@DocumentID] = {
      val query = if (labelFilters.isEmpty) {
        sql"select stableId from document limit $n offset $skip".query[String@@DocumentID].list
      } else {
        val filters = labelFilters.toList.map{ l =>
          s""" l.key='${l.fqn}' """
        }.mkString(" OR ")

        val qstr = {
          s"""| select distinct d.stableId
              | from document as d
              |   join zone as z on (d.document=z.document)
              |   join label as l on (z.label=l.label)
              | where ${filters}
              | limit $n offset $skip
              |""".stripMargin
        }

        Query0[String@@DocumentID](qstr).list
      }

      runq { query }
    }

    def getDocumentCount(labelFilters: Seq[Label]): Int = {
      val query = if (labelFilters.isEmpty) {
        sql"select count(*) from document".query[Int].unique
      } else {
        val filters = labelFilters.toList.map{ l =>
          s""" l.key='${l.fqn}' """
        }.mkString(" OR ")

        val qstr = {
          s"""| select count(distinct(d))
              | from document as d
              |   join zone as z on (d.document=z.document)
              |   join label as l on (z.label=l.label)
              | where ${filters}
              |""".stripMargin
        }

        Query0[Int](qstr).unique
      }

      runq{ query }
    }


    def addDocument(stableId: String@@DocumentID): Int@@DocumentID = runq{
      self.getOrInsertDocumentID(stableId)
    }

    def getDocument(stableId: String@@DocumentID): Option[Int@@DocumentID] = runq {
      sql"""select document from document where stableId=${stableId}"""
        .query[Int@@DocumentID]
        .option
    }

    def getDocumentStableId(docId: Int@@DocumentID): String@@DocumentID = {
      runq{
        sql"""select stableId from document where document=${docId}"""
          .query[String@@DocumentID]
          .unique
      }
    }

    def addPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Int@@PageID = {
      runq { insertPage(docId, pageNum) }
    }

    def getPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
      runq { selectPage(docId, pageNum) }
    }

    def getPages(docId: Int@@DocumentID): Seq[Int@@PageID] = {
      runq{ selectPages(docId) }
    }

    def getPageGeometry(pageId: Int@@PageID): LTBounds = {
      runq { selectPage(pageId).map(_.bounds) }
    }

    def getPageImage(pageId: Int@@PageID): Option[Array[Byte]] = {
      runq { selectPageImage(pageId) }
    }

    def getPageIdentifier(pageId: Int@@PageID): StablePage = {
      val query = for {
        p <- selectPage(pageId)
        d <- selectDocument(p.document)
      } yield StablePage(d.stableId, p.pagenum, pageId)

      runq { query }
    }

    def getPageDef(pageId: Int@@PageID): Option[Rel.Page] = {
      Option(
        runq { selectPage(pageId) }
      )
    }

    def setPageGeometry(pageId: Int@@PageID, geom: LTBounds): Unit = {
      runq { updatePage(pageId, geom) }
    }

    def setPageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
      runq { updatePageImage(pageId, bytes) }
    }


    def addTargetRegion(pageId: Int@@PageID, bbox:LTBounds): Int@@RegionID = {
      runq { self.ensureTargetRegion(pageId, bbox) }
    }

    // G.TargetRegion = M.TargetRegion+M.Page+M.Document
    def selTargetRegion(regionId: Int@@RegionID): ConnectionIO[PageRegion] =
      for {
        mTr <- selectTargetRegion(regionId)
        mPage <- selectPage(mTr.page)
        mDocument <- selectDocument(mPage.document)
      } yield {
        val stablePage = StablePage(mDocument.stableId, mPage.pagenum, mPage.prKey)
        // val page = StablePage(mPage.prKey, stable)
        PageRegion(stablePage, mTr.bounds, mTr.prKey)
        // PageRegion(tr.prKey, doc.stableId, page.pagenum, tr.bounds)
      }


    def getTargetRegion(regionId: Int@@RegionID): PageRegion =
      runq { selTargetRegion(regionId) }

    def setTargetRegionImage(regionId: Int@@RegionID, bytes: Array[Byte]): Unit = {
      runq { insertTargetRegionImage(regionId, bytes) }
    }

    def getTargetRegionImage(regionId: Int@@RegionID): Option[Array[Byte]] = {
      runq { selectTargetRegionImage(regionId) }
    }

    def deleteTargetRegionImage(regionId: Int@@RegionID): Unit = {
      runq { delTargetRegionImage(regionId) }
    }

    def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID] = {
      runq { selectTargetRegions(pageId) }
    }

    // select count(zone), document from zone where label = 1 group by document;
    def getLabel(labelId: Int@@LabelID): Label = {
      val query = selectLabel(labelId)
        .map(l => Labels.fromString(l.key).copy(id=l.prKey))
      runq{ query }
    }


    def getZone(zoneId: Int@@ZoneID): Zone =  {
      val query = for {
        zone          <- selectZone(zoneId)
        regionIds     <- selectZoneTargetRegions(zone.prKey)
        targetRegions <- regionIds.traverse { regionId =>
          selTargetRegion(regionId)
        }
        l             <- selectLabel(zone.label)
      } yield {
        val label = Labels.fromString(l.key).copy(id=l.prKey)
        Zone(zone.prKey, targetRegions, label, zone.rank)
      }

      runq { query }
    }


    def getZoneForRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID] = {
      // TODO this is a bandaid for a  bug!
      val query =  {
        sql"""
            select zn.zone
            from
               zone                      as zn
               join label                as lb    on (zn.label=lb.label)
               join zone_to_targetregion as z2tr  on (zn.zone=z2tr.zone)
            where
               z2tr.targetregion=${regionId} AND
               lb.key=${label.fqn}
           """.query[Int@@ZoneID].list
      }

      val allZoneIds = runq { query }
      if (allZoneIds.length > 1) {
        println(s"Warning: multiple zones with label ${label} detected for region ${regionId}")
      }

      allZoneIds.headOption
    }

    def getModelTextReflowForZone(zoneId: Int@@ZoneID): Option[Rel.TextReflow] = {
      runq { selectModelTextReflowForZone(zoneId) }
    }
    def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow] = {
      runq { selectTextReflowForZone(zoneId) }
    }

    def setTextReflowForZone(zoneId: Int@@ZoneID, textReflow: TextReflow): Unit = {
      val js = textReflowToJson(textReflow)
      val str = textReflow.toText()
      val jsStr = Json.stringify(js)
      runq { insertTextReflow(zoneId, jsStr, str) }
    }

    def ensureLabel(label: Label): Int@@LabelID = {
      runq{ getOrInsertLabel(label) }
    }

    def createZone(regionId: Int@@RegionID, labelId: Int@@LabelID): Int@@ZoneID = {
      val queryf = sql"""
           with pageid as (
             select page, targetregion from targetregion where targetregion=${regionId}
           ),
           docid as (
             select document from page where page=(select page from pageid)
           ),
           zoneid as (
             insert into zone (document, label) values ((select document from docid), ${labelId}) returning zone
           ),
           ztr as (
             insert into zone_to_targetregion (zone, targetregion)
                 values ((select zone from zoneid),
                         (select targetregion from pageid)) returning 0
           )
           select zone from zoneid
       """

      val query = queryf.query[Int@@ZoneID]

      // val theSql = query.sql
      // println(s"sql = ${theSql}")

      runq{ query.unique }
    }

    def createZone0(regionId: Int@@RegionID, label: Label): Int@@ZoneID = {

      val query = for {
        region <- selectTargetRegion(regionId)
        page <- selectPage(region.page)
        labelId <- getOrInsertLabel(label)
        zoneId <- insertZone(page.document, labelId)
        _ <- linkZoneToTargetRegion(zoneId, regionId)
      } yield zoneId

      runq{ query }
    }
    def addZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit = {
      val query = for {
        _ <- linkZoneToTargetRegion(zoneId, regionId)
      } yield ()

      runq{ query }
    }

    def removeZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Option[Int@@ZoneID] = {
      ???
    }

    def deleteZone(zoneId: Int@@ZoneID): Unit = {
      runq{ delZone(zoneId) }
    }

    def getZoneLabelsForDocument(docId: Int@@DocumentID): Seq[Int@@LabelID] = {
      runq{ selectZoneLabelsForDocument(docId) }
    }

    def getZonesForDocument(docId: Int@@DocumentID, labelId: Int@@LabelID): Seq[Int@@ZoneID] = {
      runq { selectZonesForDocument(docId, labelId) }
    }

    import scala.collection.mutable

    def batchImport(otherZoningApi: MemDocZoningApi): Unit = {
      // id translation

      val transDocumentID = mutable.HashMap[Int@@DocumentID    , Int@@DocumentID]()
      val transPageID     = mutable.HashMap[Int@@PageID        , Int@@PageID]()
      val transRegionID   = mutable.HashMap[Int@@RegionID      , Int@@RegionID]()
      val transZoneID     = mutable.HashMap[Int@@ZoneID        , Int@@ZoneID]()
      val transLabelID    = mutable.HashMap[Int@@LabelID       , Int@@LabelID]()
      val transReflowID   = mutable.HashMap[Int@@TextReflowID  , Int@@TextReflowID]()

      val insertLog = mutable.ArrayBuffer[String]()


      {// BLOCK
        val d0 =  otherZoningApi.tables.documents
        val allRecs = d0.all().toList
        val allStableIds = allRecs.map{_.stableId}

        val ins = "insert into document (stableId) values (?)"
        val up =  Update[String@@DocumentID](ins)
          .updateManyWithGeneratedKeys[Int@@DocumentID]("document")(allStableIds)


        val keys = runq{
          up.runLog
        }

        allRecs.zip(keys)
          .foreach{ case (rec, key) =>
            transDocumentID.put(rec.prKey, key)
          }

      } // END BLOCK

      {
        val allRecs =  otherZoningApi.tables.pages.all().toList
        insertLog.append(s"pages:${allRecs.length}")

        val allEntries = allRecs.map{rec =>
            (
              transDocumentID(rec.document),
              rec.pagenum,
              rec.imageclip,
              rec.bounds
            )
          }

        val sqlStr = (
          """|insert into page
             |  (document, pagenum, imageclip, bleft, btop, bwidth, bheight)
             |values (?, ?, ?, ?, ?, ?, ?)
             |""".stripMargin)

        val up =  Update[(
          Int@@DocumentID,
          Int@@PageNum,
          Option[Int@@ImageID],
          LTBounds
        )](sqlStr)
          .updateManyWithGeneratedKeys[Int@@PageID]("page")(allEntries)


        val keys = runq{
          up.runLog
        }
        allRecs.zip(keys)
          .foreach{ case (rec, key) =>
            transPageID.put(rec.prKey, key)
          }

      }

      {
        val allRecs =  otherZoningApi.tables.targetregions.all().toList
        insertLog.append(s"targetregions: ${allRecs.length}")
        val allEntries = allRecs.map{rec =>
          (
            transPageID(rec.page),
            rec.imageclip,
            rec.bounds
          )
        }

        // TODO decide whether to disable rank trigger on import or trust db
        val sqlStr = (
          """|insert into targetregion
             |  (page, imageclip, bleft, btop, bwidth, bheight)
             |values (?, ?, ?, ?, ?, ?)
             |""".stripMargin)

        val up =  Update[(
          Int@@PageID,
          Option[Int@@ImageID],
          LTBounds
        )](sqlStr)
          .updateManyWithGeneratedKeys[Int@@RegionID]("targetregion")(allEntries)


        val keys = runq{
          up.runLog
        }
        allRecs.zip(keys)
          .foreach{ case (rec, key) =>
            transRegionID.put(rec.prKey, key)
          }
      }

      {// BLOCK
        val allRecs =  otherZoningApi.tables.labels.all().toList
        allRecs
          .toList.foreach{rec =>
            val labelId = ensureLabel(Labels.fromString(rec.key))
            transLabelID.put(rec.prKey, labelId)
          }

      }// END Block

      {// BLOCK
        val allRecs =  otherZoningApi.tables.zones.all().toList
        insertLog.append(s"zones: ${allRecs.length}")
        val allEntries = allRecs.map{rec =>
          (
            transDocumentID(rec.document),
            transLabelID(rec.label),
            rec.rank
          )
        }

        val sqlStr = (
          """|insert into zone
             |  (document, label, rank)
             |values (?, ?, ?)
             |""".stripMargin)

        val up =  Update[(
          Int@@DocumentID,
          Int@@LabelID,
          Int
        )](sqlStr)
          .updateManyWithGeneratedKeys[Int@@ZoneID]("zone")(allEntries)


        val keys = runq{
          up.runLog
        }

        allRecs.zip(keys)
          .foreach{ case (rec, key) =>
            transZoneID.put(rec.prKey, key)
          }

      }// END Block

      {// BLOCK
        val allRecs =  otherZoningApi.tables.textreflows.all().toList
        val allEntries = allRecs.map{rec =>
          (
            rec.reflow,
            rec.astext,
            transZoneID(rec.zone)
          )
        }

        val sqlStr = (
          """|insert into textreflow
             |  (reflow, astext, zone)
             |values (?, ?, ?)
             |""".stripMargin)

        val up =  Update[(
          String,
          String,
          Int@@ZoneID
        )](sqlStr)
          .updateManyWithGeneratedKeys[Int@@TextReflowID]("textreflow")(allEntries)


        val keys = runq{
          up.runLog
        }

        allRecs.zip(keys)
          .foreach{ case (rec, key) =>
            transReflowID.put(rec.prKey, key)
          }

        insertLog.append(s"textreflows:${keys.length}")

      }// END Block

      {// BLOCK
        val allEntries =  otherZoningApi.tables.zones.toTargetRegion.getEdges()
          .toList.map{ case (lhs, rhs) =>
            (transZoneID(lhs), transRegionID(rhs))
          }

        val sqlStr = (
          """|insert into zone_to_targetregion
             |  (zone, targetregion)
             |values (?, ?)
             |""".stripMargin)

        val up =  Update[(
          Int@@ZoneID,
          Int@@RegionID
        )](sqlStr)
          .updateMany(allEntries)


        val keys = runq{
          up
        }

        insertLog.append(s"zone_to_targetregion: ${keys}")

      }// END Block

      val logmsg = insertLog.mkString(";; ")
      println(logmsg)

    }

  }
}
