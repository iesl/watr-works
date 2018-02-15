package edu.umass.cs.iesl.watr
package table

import TypeTags._
import apps._
import cats.effect._
import corpora._
import corpora.database._
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

      val res = vjoins(
        workflowApi.getWorkflows.map { workflowId =>
          val report = workflowApi.getWorkflowReport(workflowId)

          val workflowDef = workflowApi.getWorkflow(workflowId)
          val path = workflowDef.corpusPath
          val count = workflowDef.curationCount

          val boxLabelSchema = LabelSchemas.labelSchemaToBox(
            workflowDef.labelSchemas
          )

          val userAssignments = "User Assignment Counts".atop(indent(4,
            vjoins(
              report.userAssignmentCounts.toSeq.map{ case (userId, count) =>
                val email = report.usernames(userId)
                s"$email :  ${count}".box
              }
            )))

          val statusCounts = "User Assignment Status".atop(indent(4,
            vjoins(
              report.statusCounts.toSeq.map { case (statusCode, num) =>
                s"${statusCode}: ${num}".box
              }
            )
          ))


          val allActiveUsers = "Active Users".atop(indent(4, vjoins(
            report.usernames.toSeq.map { case (userId, email) =>
              email.unwrap.box
            }
          )))

          s"Workflow: ${workflowId} in ${path}" atop(
            indent(4, vjoin(
              userAssignments,
              statusCounts,
              s"Unassigned:  ${report.unassignedCount} ".box,
              s"Curation Count: ${count}",
              allActiveUsers,
              boxLabelSchema
            ))
          )
        }
      )

      println(res)
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

  }

  object annot {
    //

  }

  implicit def StringToTypeTag_CorpusPath(s: String): String@@CorpusPath = CorpusPath(s)
  implicit def StringToTypeTag_WorkflowID(s: String): String@@WorkflowID = WorkflowID(s)
  implicit def IntToTypeTag_UserID(i: Int): Int@@UserID = UserID(i)


  // object TypeTagAuto
  object assign {

    def getNextAssignment(workflowId: String@@WorkflowID, userId: Int@@UserID)(implicit corpusAccessApi: CorpusAccessApi): Option[Int@@ZoneLockID] = {
      val workflowApi = corpusAccessApi.workflowApi
      val existingLock  = workflowApi.getLockedZones(userId).headOption
      lazy val newLock =  workflowApi.lockUnassignedZones(userId, workflowId).headOption

      existingLock orElse newLock
    }

  }

}
