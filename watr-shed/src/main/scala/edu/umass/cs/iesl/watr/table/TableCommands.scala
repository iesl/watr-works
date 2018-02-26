package edu.umass.cs.iesl.watr
package table

import TypeTags._
import apps._
import cats.effect._
import corpora._
import corpora.database._
import java.text.SimpleDateFormat
import utils.Timer.time
import watrmarks._

object ShellCommands {

  private[this] val log = org.log4s.getLogger

  import apps.ProcessPipelineSteps._

  def initReflowDB(dbname: String, dbpass: String): CorpusAccessDB = {
    new CorpusAccessDB(
      dbname=dbname,
      dbuser="watrworker",
      dbpass=dbpass
    )
  }


  private def importPaperIntoDB()(implicit
    corpusAccessApi: CorpusAccessApi
  ):fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
    _.map {
      case m@ Right(Processable.ExtractedFile(segmentation, input)) =>
        log.info(s"Importing ${segmentation.stableId} into database.")

        time("DB Batch Import") {
          corpusAccessApi.corpusAccessDB.docStore.batchImport(segmentation.docStore.asInstanceOf[MemDocZoningApi])
        }
        println(s"done ${segmentation.stableId}")
        m

      case x => x
    }
  }

  private def cleanDBArtifacts(conf: TextWorksConfig.Config)(implicit
    corpusAccessApi: CorpusAccessApi
  ): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    _.map {
      case m@ Right(input) => input match {
        case p@ Processable.CorpusFile(corpusEntry) =>
          val stableId = DocumentID(corpusEntry.entryDescriptor)
          val maybeDoc = corpusAccessApi.docStore.getDocument(stableId)
          val isInDb = maybeDoc.isDefined

          if (isInDb) {
            log.info(s"Skipping ${corpusEntry}; already in DB")
            Left(input)
            // if (conf.ioConfig.overwrite) {
            //   log.info(s"Overwriting DB Entry for ${corpusEntry}")
            //   Right(p)
            // } else {
            //   log.info(s"Skipping ${corpusEntry}; already in DB")
            //   Left(input)
            // }
          } else {
            Right(input)
          }

        case x => Left(x)
      }
      case x => x
    }
  }



  def segmentAll(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
      outputPath= None,
      overwrite = true
    ))

    val processStream = createInputStream[IO](conf.ioConfig)
      .drop(skip.toLong).take(n.toLong)
      .through(initMarkedInput())
      .through(cleanDBArtifacts(conf))
      .through(cleanFileArtifacts(conf))
      .through(markUnextractedProcessables(conf))
      .through(runSegmentation(conf))
      .through(writeExtractedTextFile(conf))
      .through(importPaperIntoDB())

    processStream.compile.drain
      .unsafeRunSync()

  }

  import workflow._

  def cdirs()(implicit corpusAccessApi: CorpusAccessApi): DatabaseCorpusDirectory = {
    new DatabaseCorpusDirectory()(corpusAccessApi.corpusAccessDB)
  }

  object curationSetup {
    import workflow._
    val dryRunPath = CorpusPath("BioarxivPmid.Dryrun")


    def reset()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      recreateCurationTables()
      defineTestrunWorkflow()
      moveDocsToDryrunPath(3)
    }


    def recreateCurationTables()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val tables = corpusAccessApi.corpusAccessDB.tables

      corpusAccessApi.corpusAccessDB.runqOnce{
        for {
          _ <- tables.corpusPathTables.drop.update.run
          _ <- tables.zonelockTables.drop.run
          _ <- tables.annotationTables.drop.run
          _ <- tables.workflowTables.drop.update.run

          _ <- tables.workflowTables.create.update.run
          _ <- tables.zonelockTables.create.run
          _ <- tables.annotationTables.create.run
          _ <- tables.corpusPathTables.create.update.run

        } yield ()
      }
    }



    def moveDocsToDryrunPath(n: Int)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val cd = cdirs()
      cd.makeDirectory(dryRunPath)

      corpusAccessApi.docStore.getDocuments(n).foreach { stableId =>
        cd.moveEntry(stableId, dryRunPath)
      }

    }

    def defineTestrunWorkflow()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val workflowApi = corpusAccessApi.workflowApi

      workflowApi.defineWorkflow("headers-dryrun-1", "Trial Run", None, ExampleLabelSchemas.headerLabelSchema,
        dryRunPath,
        curationCount = 2
      )
    }
  }


  object display {
    import textboxing.{TextBoxing => TB}, TB._

    def curators()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val userbaseApi = corpusAccessApi.userbaseApi
      val userEmails = userbaseApi.getUsers().map { userId =>
        val user = userbaseApi.getUser(userId)
        val person  = user.get
        userId.unwrap.toString.box + ":  " + person.email.unwrap
      }
      val users= "All Users".atop(indent(4,
        vjoins(userEmails)
      ))

      println(users)
    }

    def workflows()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val workflowApi = corpusAccessApi.workflowApi
      val workflowsWithReports = workflowApi.getWorkflows.map{  id =>
        (workflowApi.getWorkflowReport(id), workflowApi.getWorkflow(id))
      }

      println(WorkflowReport.prettyPrint(workflowsWithReports))
    }

    def dirs()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val cd = cdirs()
      val res = TB.vjoins(
        cd.listDirectories().map{ d =>
          TB.tbox(d.unwrap).atop(indent(4,
            TB.vjoins(
              cd.listEntries(d).fold(
                err => Seq(err.box),
                ids => ids.map { id => TB.tbox(id.unwrap) }
              )
            )
          ))
        }
      )
      println(res)
    }



    def annots()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val db = corpusAccessApi.corpusAccessDB
      // val datefmt = new SimpleDateFormat("dd MMM yyyy HH:mm:ss z")
      // s"""Created: ${datefmt.format(annot.created)}""",
      val allAnnots = db.getAnnotations().map{ annotId =>
        val annot = db.getAnnotation(annotId)
        s"${annot.id} on ${annot.document}" atop(indent(4, {
          vjoin(
            s"Creator: ${annot.creator}",
            s"""Created: ${annot.created}""",
            s"Status: ${annot.status}",
            s"Json: ${annot.jsonRec}",
          )
        }))
      }
      val res = s"Annotations".box atop(indent(4,
        vjoins(allAnnots)
      ))
      println(res)
    }
  }

  object annot {


    def updateStatus(annotId: Int@@AnnotationID, status: String@@StatusCode)(implicit
      corpusAccessApi: CorpusAccessApi
    ): Unit = {
      val db = corpusAccessApi.corpusAccessDB
      db.updateAnnotationStatus(annotId, status)
    }

    def updateJson(annotId: Int@@AnnotationID, jsonRec: String)(implicit
      corpusAccessApi: CorpusAccessApi
    ): Unit = {
      val db = corpusAccessApi.corpusAccessDB
      db.updateAnnotationJson(annotId, jsonRec)
    }


    import utils.DoOrDieHandlers._


    def createAnnotation(
      workflowId: String@@WorkflowID,
      stableId: String@@DocumentID,
      userEmail: String@@EmailAddr
    )(implicit corpusAccessApi: CorpusAccessApi): Int@@AnnotationID = {
      val db = corpusAccessApi.corpusAccessDB
      val users = corpusAccessApi.userbaseApi
      (for {
        docId <- corpusAccessApi.docStore.getDocument(stableId)
        userId <- users.getUserByEmail(userEmail)
      } yield {
        db.createAnnotation(userId, docId, workflowId)
      }).orDie("")
    }

  }

  implicit def StringToTypeTag_CorpusPath(s: String): String@@CorpusPath = CorpusPath(s)
  implicit def StringToTypeTag_WorkflowID(s: String): String@@WorkflowID = WorkflowID(s)
  implicit def StringToTypeTag_EmailAddr(s: String): String@@EmailAddr = EmailAddr(s)
  implicit def StringToTypeTag_DocumentID(s: String): String@@DocumentID = DocumentID(s)
  implicit def IntToTypeTag_UserID(i: Int): Int@@UserID = UserID(i)


  object assign {

    def getNextAssignment(workflowId: String@@WorkflowID, userEmail: String@@EmailAddr)(implicit corpusAccessApi: CorpusAccessApi): Option[Int@@ZoneLockID] = {
      val workflowApi = corpusAccessApi.workflowApi
      val users = corpusAccessApi.userbaseApi
      for {
        userId <- users.getUserByEmail(userEmail)
        lockId <- ( workflowApi.getLockedZones(userId).headOption
                    orElse workflowApi.lockUnassignedZones(userId, workflowId) )
      } yield lockId
    }

    def completeAssignment(zonelockId: Int@@ZoneLockID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val workflowApi = corpusAccessApi.workflowApi
      workflowApi.updateZoneStatus(zonelockId, ZoneLockStatus.Completed)
    }

  }

  object users {

    def addUser(email: String@@EmailAddr)(implicit corpusAccessApi: CorpusAccessApi): Either[Int@@UserID, Int@@UserID] = {
      val userbaseApi = corpusAccessApi.userbaseApi
      userbaseApi.getUserByEmail(email)
        .map(Left(_))
        .getOrElse { Right(userbaseApi.addUser(email)) }
    }
  }

}

