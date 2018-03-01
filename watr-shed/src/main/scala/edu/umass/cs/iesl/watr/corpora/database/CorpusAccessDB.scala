package edu.umass.cs.iesl.watr
package corpora
package database

import cats._
import cats.implicits._
import cats.data._
import cats.effect._

import doobie._
import doobie.implicits._

import doobie.free.{ connection => C }
import java.io.PrintWriter


import java.nio.file.Path
import java.nio.file.Paths
import java.util.Properties


import geometry._
import corpora._
import workflow._

import watrmarks._
import TypeTags._
import shapeless._

import _root_.io.circe, circe.syntax._
import utils.DoOrDieHandlers._

class CorpusAccessDB(
  dbname: String, dbuser: String, dbpass: String
) extends DoobieImplicits  { self =>


  val tables: CorpusAccessDBTables = new CorpusAccessDBTables()

  val Rel = RelationModel

  import doobie.hikari._
  import doobie.hikari.implicits._
  import doobie.postgres.syntax._

  /** Prepend a ConnectionIO program with a log message. */
  def printBefore(tag: String, s: String): ConnectionIO[Unit] => ConnectionIO[Unit] =
    HC.delay(Console.println(s"$tag: $s")) <* _


  /** Derive a new transactor that logs stuff. */
  def addLogging[A](name: String)(xa: Transactor[IO]): Transactor[IO] = {
    import Transactor._ // bring the lenses into scope
    val update: State[Transactor[IO], Unit] =
      for {
        _ <- before %= printBefore(name, "before - setting up the connection")
        _ <- after  %= printBefore(name, "after - committing")
        _ <- oops   %= printBefore(name, "oops - rolling back")
        _ <- always %= printBefore(name, "always - closing")
      } yield ()
    update.runS(xa).value
  }


  val props = new Properties()
  // props.setProperty("housekeeper","true")
  props.setProperty("logUnclosedConnections","true")
	props.setProperty("maximumPoolSize", "20");
	props.setProperty("minimumIdle", "20");
	props.setProperty("maxLifetime", "30000");
	props.put("dataSource.logWriter", new PrintWriter(System.out));
  // def initTransactor(): HikariTransactor[IO] = (for {
  //   xa <- HikariTransactor.newHikariTransactor[IO]("com.impossibl.postgres.jdbc.PGDriver", s"jdbc:pgsql:${dbname}", dbuser, dbpass)
  //   _  <- xa.configure(hx => IO {
  //     hx.setDataSourceProperties(props)
  //     hx.setAutoCommit(false)
  //     // ()
  //   })
  // } yield {
  //   xa
  // }).unsafeRunSync

  val JdbcDriverName = "org.postgresql.ds.PGSimpleDataSource"
  // "org.postgresql.Driver"

  def initPostgresDriverTransactor2(): HikariTransactor[IO] = (for {
    transactor <- HikariTransactor.newHikariTransactor[IO](
      JdbcDriverName,
      s"jdbc:postgresql:${dbname}",
      dbuser,
      dbpass
    )
    // logTransactor = addLogging("Postgres")(transactor)
    _ <- transactor.configure { h => IO {
      // h.setDataSourceProperties(props)
      // h.setAutoCommit(false)
      // h.setMaximumPoolSize(20)
    }}
  } yield {
    // logTransactor.asInstanceOf[HikariTransactor[IO]]
    transactor
  }).unsafeRunSync

  // var _hikariTransactor: HikariTransactor[IO] = initTransactor()

  var _hikariTransactor2: HikariTransactor[IO] = initPostgresDriverTransactor2()

  // def getTransactor(): HikariTransactor[IO] = {
  //   if (_hikariTransactor == null) {
  //     _hikariTransactor = initTransactor()
  //   }
  //   _hikariTransactor
  // }

  def getTransactor2(): HikariTransactor[IO] = {
    if (_hikariTransactor2 == null) {
      _hikariTransactor2 = initPostgresDriverTransactor2()
    }
    _hikariTransactor2
  }

  def reinit() = {
    shutdown()
  }

  def shutdown(): Unit = {
    println(s"Running Shutdown.")
    if (_hikariTransactor2 != null) {
      // _hikariTransactor2.shutdown.unsafeRunSync()
      (for {
         _ <-  _hikariTransactor2.asInstanceOf[HikariTransactor[IO]].shutdown
      } yield {
        _hikariTransactor2 = null
      }).unsafeRunSync()
    }
  }

  def runq[A](query: C.ConnectionIO[A]): A = {
    runWithTransactor(getTransactor2())(query)
  }

  // def runqOnceOld[A](query: C.ConnectionIO[A]): A = {
  //   runWithTransactor(getTransactor2())(query)
  // }

  def runqOnce[A](query: C.ConnectionIO[A]): A = runq(query)

  def runWithTransactor[A](t: HikariTransactor[IO])(query: C.ConnectionIO[A]): A = {
    val run = for {
      r <- query.transact(t)
    } yield r

    run.unsafeRunSync()
  }

  def decodeSQLExceptions(err: Throwable): Throwable = {
    err match {
      case t: org.postgresql.util.PSQLException =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        val err = t.getServerErrorMessage()
        println(s"ERROR: ${message}")
        println(s"Server ERROR: ${err}")
        val state = t.getSQLState
        println(s"Server State: ${state}")

        throw t
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
      .to[List]
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


  def selectPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Option[Int@@PageID]] = {
    sql"""select page from page where document=${docId} and pagenum=${pageNum}"""
      .query[Int]
      .option
      .map(_.map(PageID(_)))
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
      runq { sql""" select person from person """.query[Int@@UserID].to[List] }
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
    import watrmarks.{StandardLabels => LB}

    override def corpusLockingApi(): CorpusLockingApi = self.corpusLockApi

    def defineWorkflow(
      slug: String,
      schemaName: String@@LabelSchemaName,
      targetPath: String@@CorpusPath,
    ): String@@WorkflowID = {

      runq {
        sql"""
           insert into workflow (workflow, labelSchema, targetPath)
           values (${slug}, (select labelschema from labelschema where name=${schemaName.unwrap}), text2ltree(${targetPath}))
           returning workflow;
        """.query[String@@WorkflowID].unique
      }
    }

    def getWorkflow(workflowId:String@@WorkflowID): Rel.WorkflowDef = {
      runq {
        sql""" SELECT w.workflow, l.schema, w.targetPath
               FROM   workflow w JOIN labelschema l on (w.labelschema=l.labelschema)
               WHERE  w.workflow=${workflowId}
        """.query[(String, String, String@@CorpusPath)]
          .map{ case (workflowId, schema, path) =>
            Rel.WorkflowDef(
              WorkflowID(workflowId),
              schema.asJson.decodeOrDie[LabelSchemas](),
              path
            )
          }.unique
      }
    }


    def deleteWorkflow(workflowId:String@@WorkflowID): Unit = {
      sql""" DELETE FROM workflow WHERE workflow = ${workflowId} """.update.run
    }

    def getWorkflows(): Seq[String@@WorkflowID] = runq {
      sql""" select workflow from workflow """.query[String@@WorkflowID].to[List]
    }


    def getWorkflowReport(workflowId:String@@WorkflowID): WorkflowReport = {
      val statusCounts = for {
        status <- CorpusLockStatus.all
      } yield {
        val statusCount = runq {
          sql"""
              WITH targetPath as (
                select targetPath from workflow where workflow=${workflowId}
              )
              select count(*)
                from   corpuslock cl
                where  cl.lockPath ~ targetPath
                  AND  cl.status=${status}
              """.query[Int].unique
        }
        (status, statusCount)
      }
      // select count(*) from zonelock where status=${status} AND workflow=${workflowId}
      val assigneeCounts  = runq {
        sql"""
              WITH targetPath as (
                select targetPath from workflow where workflow=${workflowId}
              )
              select p.person, p.email, cl.status, count(corpuslock)
              from corpuslock cl join person p on (cl.holder=p.person)
              where cl.lockPath = targetPath AND cl.holder is not null
              group by p.person, cl.status
         """.query[(Int@@UserID, String@@EmailAddr, String@@StatusCode, Int)].to[List]
      }

      // val assigneeNames  = runq {
      //   sql"""
      //      select distinct assignee, email from zonelock z join person p on z.assignee=p.person where workflow=${workflowId};
      //    """.query[(Int@@UserID, String@@EmailAddr)].to[List]
      // }

      WorkflowReport(
        statusCounts.toMap,
        assigneeCounts
      )
    }


  }



  object docStore extends DocumentZoningApi {
    def workflowApi: WorkflowApi = self.workflowApi
    def userbaseApi: UserbaseApi = self.userbaseApi

    def getDocuments(n: Int=Int.MaxValue, skip: Int=0): Seq[String@@DocumentID] = runq {
      sql"select stableId from document limit $n offset $skip".query[String@@DocumentID].to[List]
    }

    def getDocumentCount(): Int = {
      runq { sql"select count(*) from document".query[Int].unique }
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

    import scala.collection.mutable

    def batchImport(otherZoningApi: MemDocZoningApi): Unit = {
      // id translation

      val transDocumentID = mutable.HashMap[Int@@DocumentID    , Int@@DocumentID]()
      val transPageID     = mutable.HashMap[Int@@PageID        , Int@@PageID]()

      val insertLog = mutable.ArrayBuffer[String]()


      {// BLOCK
        val d0 =  otherZoningApi.tables.documents
        val allRecs = d0.all().toList
        val allStableIds = allRecs.map{_.stableId}

        val ins = "insert into document (stableId) values (?)"
        val up =  Update[String@@DocumentID](ins)
          .updateManyWithGeneratedKeys[Int@@DocumentID]("document")(allStableIds)


        val keys = runq{
          up.compile.toVector
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
          up.compile.toVector
        }
        allRecs.zip(keys)
          .foreach{ case (rec, key) =>
            transPageID.put(rec.prKey, key)
          }

      }

      val logmsg = insertLog.mkString(";; ")
      println(logmsg)

    }

  }

  object annotApi extends DocumentAnnotationApi {

    def createAnnotation(docId: Int@@DocumentID): Int@@AnnotationID = runq {
      sql"""
            INSERT INTO annotation (document) VALUES ( ${docId} )
         """.update.withUniqueGeneratedKeys[Int@@AnnotationID]("annotation")
    }


    def createLabelSchema(labelSchema: LabelSchemas): Int@@LabelSchemaID = {
      val jsonSchema = labelSchema.asJson.noSpaces
      runq {
        sql"""
           INSERT INTO labelschema (schema) VALUES ( ${jsonSchema} )
        """.update.withUniqueGeneratedKeys[Int@@LabelSchemaID]("labelschema")
      }
    }

    def getLabelSchema(schemaId: Int@@LabelSchemaID): LabelSchemas = runq {
      sql""" SELECT schema FROM labelschema WHERE labelschema = ${schemaId} """
        .query[String].map{ s  =>
          s.asJson.decodeOrDie[LabelSchemas]()
        }.unique
    }

    def getLabelSchema(schemaName: String@@LabelSchemaName): Int@@LabelSchemaID = runq {
      sql""" SELECT labelschema FROM labelschema WHERE name = ${schemaName.unwrap} """
        .query[Int@@LabelSchemaID].unique
    }

    def deleteLabelSchema(schemaId: Int@@LabelSchemaID): Unit = runq {
      sql""" DELETE FROM labelschema WHERE labelschema = ${schemaId} """.update.run
    }

    def assignOwnership(annotId: Int@@AnnotationID, userId: Int@@UserID): Unit = runq {
      sql"""UPDATE annotation SET owner=${userId} WHERE annotation=${annotId} """.update.run
    }

    def setCorpusPath(annotId: Int@@AnnotationID, path: String@@CorpusPath): Unit = runq {
      sql"""UPDATE annotation SET annotPath= text2ltree( ${path} ) WHERE annotation=${annotId} """.update.run
    }


    def listAnnotations(pathQuery: String@@CorpusPathQuery): Seq[Int@@AnnotationID] = runq {
      sql"""
            SELECT annotation FROM annotation
            WHERE annotPath ~ ${pathQuery}
        """.query[Int@@AnnotationID].to[Vector]
    }

    def listAnnotations(userId: Int@@UserID, path: Option[String@@CorpusPathQuery]): Seq[Int@@AnnotationID] = runq {
      path match {
        case Some(query) =>
          sql"""
            SELECT annotation FROM annotation
            WHERE annotPath ~ ${query} AND owner = ${userId}
            """.query[Int@@AnnotationID].to[Vector]

        case None =>
          sql"""
            SELECT annotation FROM annotation
            WHERE  owner = ${userId}
            """.query[Int@@AnnotationID].to[Vector]
      }
    }


    def updateBody(annotId: Int@@AnnotationID, body: String): Unit =  runq {
      sql"""UPDATE annotation SET body=${body} WHERE annotation=${annotId} """.update.run
    }

    def deleteAnnotation(annotId: Int@@AnnotationID): Unit =  runq {
      sql""" DELETE FROM annotation WHERE annotation=${annotId} """.update.run
    }

    def getAnnotations(): Seq[Int@@AnnotationID]= runq {
      sql""" SELECT annotation FROM annotation """
        .query[Int@@AnnotationID].to[Vector]
    }

    def getAnnotationRecord(annotId: Int@@AnnotationID): Rel.AnnotationRec =  runq {
      sql"""
          SELECT annotation, document, owner, annotPath, created, body
          FROM annotation WHERE annotation=${annotId}
      """.query[Rel.AnnotationRec].unique
    }

  }

  object corpusLockApi extends CorpusLockingApi {

    def createLock(docId: Int@@DocumentID, lockPath: String@@CorpusPath): Int@@LockID = runq {
      sql"""
           INSERT INTO corpuslock (holder, document, lockPath, status)
           SELECT NULL, ${docId}, text2ltree($lockPath), ${CorpusLockStatus.Available}
      """.update.withUniqueGeneratedKeys[Int@@LockID]("corpuslock")
    }

    def deleteLock(lockId: Int@@LockID): Unit = runq {
      sql"""
           DELETE FROM corpuslock WHERE corpuslock = ${lockId};
      """.update.run
    }

    def getLocks(): Seq[Int@@LockID] = runq {
      sql"""
          SELECT corpuslock FROM corpuslock;
      """.query[Int@@LockID].to[Vector]
    }


    def getLockRecord(lockId: Int@@LockID): Option[R.CorpusLock] = {
      // println(s"getLockRecord ${lockId}")

      runq {
        sql"""
          SELECT corpuslock, holder, document, lockPath, status
          FROM corpuslock
          WHERE corpuslock=${lockId.unwrap};
      """.query[Rel.CorpusLock].option
      }
    }

    def getDocumentLocks(docId: Int@@DocumentID): Seq[Int@@LockID] = runq {
      sql"""
          SELECT corpuslock FROM corpuslock WHERE document=${docId}
      """.query[Int@@LockID].to[Vector]
    }


    def getUserLocks(userId: Int@@UserID): Seq[Int@@LockID] = runq {
      sql"""
          SELECT corpuslock FROM corpuslock WHERE holder=${userId}
      """.query[Int@@LockID].to[Vector]
    }

    def acquireLock(userId: Int@@UserID, lockPath: String@@CorpusPath): Option[Int@@LockID] = runq {
      sql"""
          WITH priorLockedDocs AS (
            SELECT document FROM corpuslock WHERE holder=${userId} AND lockPath=text2ltree( ${lockPath} )
          ),
          heldLocks AS (
            SELECT corpuslock FROM corpuslock
            WHERE   holder=${userId}
              AND   status=${CorpusLockStatus.Locked}
          ),
          nextAvailable AS (
            SELECT corpuslock FROM corpuslock
            WHERE   lockPath=text2ltree( ${lockPath} )
              AND   status=${CorpusLockStatus.Available}
              AND   document NOT IN (SELECT * FROM priorLockedDocs)
              AND   (SELECT count(*) FROM heldLocks) = 0
            LIMIT 1
            FOR UPDATE
          )
          UPDATE corpuslock cl
          SET status=${CorpusLockStatus.Locked}, holder=${userId}
          FROM nextAvailable
          WHERE  nextAvailable.corpuslock = cl.corpuslock
          RETURNING cl.corpuslock
      """.query[Int@@LockID].option
    }

    def releaseLock(lockId: Int@@LockID): Unit = runq {
      sql"""
          UPDATE corpuslock SET status=${CorpusLockStatus.Completed}
          WHERE   corpuslock=${lockId}
      """.update.run
    }

  }
}
