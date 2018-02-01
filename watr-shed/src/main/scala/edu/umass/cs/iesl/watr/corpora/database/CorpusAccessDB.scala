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

import geometry._
import corpora._
import workflow._

import watrmarks._
import TypeTags._
import shapeless._
import textgrid._

import _root_.io.circe, circe._, circe.syntax._
import circe.parser.decode
// import circe.generic.semiauto._

class CorpusAccessDB(
  dbname: String, dbuser: String, dbpass: String
) extends DoobieImplicits  { self =>


  val tables: CorpusAccessDBTables = new CorpusAccessDBTables()

  val Rel = RelationModel

  import scalaz.concurrent.Task
  import doobie.hikari.imports._


  val props = new Properties()
  props.setProperty("housekeeper","false")

  def xa0: HikariTransactor[Task] = (for {
    xa <- HikariTransactor[Task]("com.impossibl.postgres.jdbc.PGDriver", s"jdbc:pgsql:${dbname}", dbuser, dbpass)
    _  <- xa.configure(hx => Task.delay{
      hx.setDataSourceProperties(props)
      hx.setAutoCommit(true)
      ()
    })
  } yield {
    xa
  }).unsafePerformSync

  var xa: HikariTransactor[Task] = xa0

  def reinit() = {
    shutdown()
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


  def updatePage(pageId: Int@@PageID, geom: LTBounds): ConnectionIO[Int] = {
    val LTBounds.IntReps(l, t, w, h) = geom

    sql"""
       update page set bleft=$l, btop=$t, bwidth=$w, bheight=$h where page=${pageId}
    """.update.run
  }

  def insertPage(docPrKey: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Int@@PageID] = {
    updateGetKey("page",
      sql""" insert into page
                (document, pagenum, bleft, btop, bwidth, bheight)
         values (${docPrKey}, ${pageNum}, 0, 0, 0, 0) """.update
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
     select pg.page, pg.document, pg.pagenum, pg.bleft, pg.btop, pg.bwidth, pg.bheight
     from   page as pg
     where  pg.page=${pageId.unwrap}
    """.query[Rel.Page].unique
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
        sql"""insert into targetregion (page, bleft, btop, bwidth, bheight)
                 values ($pageId, $bl, $bt, $bw, $bh)
             """.update.withUniqueGeneratedKeys[Int]("targetregion").map(RegionID(_))
      )(tr => FC.delay(tr.prKey))
    } yield regionId
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
    def addUser(email: String@@EmailAddr): Int@@UserID = {
      runq { sql"""
        insert into person (email) values (${email})
        """.update
        .withUniqueGeneratedKeys[Int]("person")
        .map(UserID(_))
      }
    }

    def getUser(userId: Int@@UserID): Option[Rel.Person] = {
      runq { sql"""
        select person, email from person where person=${userId}
        """.query[Rel.Person].option
      }
    }

    def getUsers(): Seq[Int@@UserID] = {
      runq { sql""" select person from person """.query[Int@@UserID].list }
    }

    def getUserByEmail(email: String@@EmailAddr): Option[Int@@UserID] = {
      runq { sql"""
        select person from person where email=${email}
        """.query[Int@@UserID].option
      }
    }

    def deleteUser(userId: Int@@UserID): Unit = {
      ???
    }
    def setPassword(userId:Int@@UserID, passhash: String@@PasswordHash): Unit = {
      ???

    }

    def getPassword(userId:Int@@UserID): String@@PasswordHash = {
      ???
    }

    def grantRole(userId: Int@@UserID, role: String@@Role): Unit = {

      ???
    }
    def getRoles(userId: Int@@UserID): Seq[String@@Role] = {

      ???
    }
    def revokeRole(userId: Int@@UserID, role: String@@Role): Unit = {
      ???

    }
  }
  object workflowApi extends WorkflowApi {
    import doobie.postgres.imports._
    import watrmarks.{StandardLabels => LB}

    def defineWorkflow(slug: String, desc: String, targetLabelOpt: Option[Label], labelSchemas: LabelSchemas): String@@WorkflowID = {

      val targetLabel = targetLabelOpt.getOrElse{ LB.FullPdf }
      val targetLabelId = docStore.ensureLabel(targetLabel)

      labelSchemas.allLabels.map(Label(_)).foreach { l =>
          docStore.ensureLabel(_)
      }

      val jsonSchema = labelSchemas.asJson.noSpaces

      runq {
        sql"""
           insert into workflow (workflow, description, targetLabel, labelSchemas)
           values (${slug}, ${desc}, ${targetLabelId}, ${jsonSchema})
           returning workflow
        """.query[String@@WorkflowID].unique
      }
    }



    def getWorkflow(workflowId:String@@WorkflowID): Rel.WorkflowDef = {
      runq {
        sql""" select workflow, description, targetLabel, labelSchemas
               from   workflow
               where  workflow=${workflowId}
        """.query[(String, String, Int@@LabelID, String)]

          .map{ case (workflowId, desc, target, schema) =>
            val labelSchemas = decode[LabelSchemas](schema).fold(err => {
              sys.error(s"could not decode LabelSchemas from ${schema}")
            }, labelSchemas => {
              labelSchemas
            })

            val l = docStore.getLabel(target)

            Rel.WorkflowDef(
              WorkflowID(workflowId), desc,
              Rel.Label(l.id, l.key),
              labelSchemas
            )
          }.unique
      }
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

    def getWorkflows(): Seq[String@@WorkflowID] = runq {
      sql""" select workflow from workflow """.query[String@@WorkflowID].list
    }


    def getWorkflowReport(workflowId:String@@WorkflowID): WorkflowReport = {
      val unassignedCount = runq {
        sql"""
          select count(*)
            from      zone as z
            left join zonelock as lk using (zone)
            where  z.label=(select targetLabel from workflow where workflow=${workflowId})
              AND  lk.zone is null
       """.query[Int].unique
      }

      val tuples = for {
        status <- ZoneLockStatus.all
      } yield {
        val statusCount = runq {
          sql""" select count(*) from zonelock where status=${status} AND workflow=${workflowId}"""
            .query[Int].unique
        }
        (status, statusCount)
      }

      val assigneeCounts  = runq {
        sql"""
            select assignee, count(zone)
            from zonelock where workflow=${workflowId} AND assignee is not null
            group by assignee
         """.query[(Int@@UserID, Int)].list
      }
      val assigneeNames  = runq {
        sql"""
           select distinct assignee, email from zonelock z join person p on z.assignee=p.person where workflow=${workflowId};
         """.query[(Int@@UserID, String@@EmailAddr)].list
      }
      WorkflowReport(
        unassignedCount,
        tuples.toMap,
        assigneeCounts.toMap,
        assigneeNames.toMap
      )
    }

    def lockUnassignedZones(userId: Int@@UserID, workflowId: String@@WorkflowID, count: Int): Seq[Int@@ZoneLockID] = runq {
      sql"""
        insert into zonelock (assignee, workflow, zone, status)
          select ${userId}, ${workflowId}, z.zone, ${ZoneLockStatus.Assigned}
            from      zone as z
            left join zonelock as lk using (zone)
            where  z.label=(select targetlabel from workflow where workflow=${workflowId})
              AND  lk.zone is null
            limit ${count}
       """.update.withGeneratedKeys[Int]("zonelock").map(ZoneLockID(_)).runLog
    }


    def updateZoneStatus(zoneLockId: Int@@ZoneLockID, newStatus: String@@StatusCode): Unit = {
      runq {
        sql""" update zonelock SET status=${newStatus} where zonelock=${zoneLockId} """.update.run
      }
    }

    def releaseZoneLock(zoneLockId: Int@@ZoneLockID): Unit = {
      runq {
        sql""" update zonelock set assignee = null where zonelock=${zoneLockId} """
          .update.run
      }
    }

    def getLockForZone(zoneId: Int@@ZoneID): Option[Int@@ZoneLockID] = {
      runq {
        sql""" select zonelock from zonelock where zone=${zoneId} """
          .query[Int@@ZoneLockID].option
      }
    }

    def getZoneLock(zoneLockId: Int@@ZoneLockID): Option[Rel.ZoneLock] = {
      runq { sql"""
        select * from zonelock where zonelock=${zoneLockId}
        """.query[Rel.ZoneLock].option
      }
    }


    def getLockedZones(userId: Int@@UserID): Seq[Int@@ZoneLockID] = {
      runq { sql"""
        select zonelock from zonelock where assignee=${userId}
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

    def getPageText(pageId: Int@@PageID): Option[TextGrid] = {
      ???

    }
    def setPageText(pageId: Int@@PageID, textgrid: TextGrid): Unit = {
      ???
    }

    def getZoneTextAsJsonStr(zoneId: Int@@ZoneID): Option[String] = {
      runq{
        sql""" select glyphs from zone where zone = ${zoneId} """.query[Option[String]].unique
      }
    }

    def setZoneText(zoneId: Int@@ZoneID, textgrid: TextGrid): Unit = {
      val gridJs = textgrid.toJson()
      val gridJsStr = gridJs.noSpaces
      runq{
        sql""" update zone SET glyphs = ${gridJsStr} where zone = ${zoneId} """.update.run
      }
    }


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
        val jsStr = getZoneTextAsJsonStr(zoneId)
        Zone(zone.prKey, targetRegions, label, zone.rank, jsStr)
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
              rec.bounds
            )
          }

        val sqlStr = (
          """|insert into page
             |  (document, pagenum, bleft, btop, bwidth, bheight)
             |values (?, ?, ?, ?, ?, ?)
             |""".stripMargin)

        val up =  Update[(
          Int@@DocumentID,
          Int@@PageNum,
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

      {// BLOCK
        val allRecs =  otherZoningApi.tables.targetregions.all().toList
        insertLog.append(s"targetregions: ${allRecs.length}")
        val allEntries = allRecs.map{rec =>
          (
            transPageID(rec.page),
            rec.bounds
          )
        }

        // TODO decide whether to disable rank trigger on import or trust db
        val sqlStr = (
          """|insert into targetregion
             |  (page, bleft, btop, bwidth, bheight)
             |values (?, ?, ?, ?, ?)
             |""".stripMargin)

        val up =  Update[(
          Int@@PageID,
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
            rec.rank,
            rec.glyphs
          )
        }

        val sqlStr = (
          """|insert into zone
             |  (document, label, rank, glyphs)
             |values (?, ?, ?, ?)
             |""".stripMargin)

        val up =  Update[(
          Int@@DocumentID,
          Int@@LabelID,
          Int,
          Option[String]
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
