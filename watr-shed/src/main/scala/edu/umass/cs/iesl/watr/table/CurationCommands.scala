package edu.umass.cs.iesl.watr
package table


import TypeTags._
import corpora._
import utils.DoOrDieHandlers._

import workflow._
import textboxing.{TextBoxing => TB}, TB._

object CurationCommands {


  implicit def StringToTypeTag_CorpusPath(s: String): String@@CorpusPath = CorpusPath(s)
  implicit def StringToTypeTag_WorkflowID(s: String): String@@WorkflowID = WorkflowID(s)
  implicit def StringToTypeTag_EmailAddr(s: String): String@@EmailAddr = EmailAddr(s)
  implicit def StringToTypeTag_DocumentID(s: String): String@@DocumentID = DocumentID(s)
  implicit def IntToTypeTag_UserID(i: Int): Int@@UserID = UserID(i)


  def cdirs()(implicit corpusAccessApi: CorpusAccessApi): DatabaseCorpusDirectory = {
    new DatabaseCorpusDirectory()(corpusAccessApi.corpusAccessDB)
  }

  object curationSetup {
    val dryRunPath = CorpusPath("BioarxivPmid.Dryrun")


    def reset()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      unsafeDropRecreateEverything()
      defineTestrunWorkflow()
      moveDocsToDryrunPath(3)
    }

    def unsafeDropRecreateEverything()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val tables = corpusAccessApi.corpusAccessDB.tables

      corpusAccessApi.corpusAccessDB.runqOnce{
        for {
          _ <- tables.dropAll()
          _ <- tables.createAll()
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
      val annotApi = corpusAccessApi.annotApi
      val schemaId = annotApi.createLabelSchema(ExampleLabelSchemas.headerLabelSchema)

      workflowApi.defineWorkflow(
        "headers-dryrun-1",
        ExampleLabelSchemas.headerLabelSchema.name,
        dryRunPath
      )
    }
  }


  object display {

    def locks(): Unit = {

    }

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
      val allAnnots = db.annotApi.getAnnotations().map{ annotId =>
        val annot = db.annotApi.getAnnotationRecord(annotId)
        s"${annot.id} on ${annot.document}" atop(indent(4, {
          vjoin(
            s"Owner: ${annot.owner}",
            s"""Created: ${annot.created}""",
            s"Body: ${annot.body}",
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


    // def updateStatus(annotId: Int@@AnnotationID, status: String@@StatusCode)(implicit
    //   corpusAccessApi: CorpusAccessApi
    // ): Unit = {
    //   val db = corpusAccessApi.corpusAccessDB
    //   db.annotApi.updateAnnotationStatus(annotId, status)
    // }

    def updateBody(annotId: Int@@AnnotationID, body: AnnotationBody)(implicit
      corpusAccessApi: CorpusAccessApi
    ): Unit = {
      val db = corpusAccessApi.corpusAccessDB
      db.annotApi.updateBody(annotId, body)
    }




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
        val annotId = db.annotApi.createAnnotation(docId)
        db.annotApi.assignOwnership(annotId, userId)
        val path = db.workflowApi.getWorkflow(workflowId).targetPath
        db.annotApi.setCorpusPath(annotId, path)
        annotId

      }).orDie("createAnnotation")
    }

  }


  object assign {

    def getNextAssignment(workflowId: String@@WorkflowID, userEmail: String@@EmailAddr)(implicit corpusAccessApi: CorpusAccessApi): Option[Int@@LockID] = {
      val workflowApi = corpusAccessApi.workflowApi
      val users = corpusAccessApi.userbaseApi
      val lockApi = corpusAccessApi.corpusLockApi
      for {
        userId <- users.getUserByEmail(userEmail)
        lockId <- ( lockApi.getUserLocks(userId).headOption
                    orElse workflowApi.lockNextDocument(userId, workflowId) )
      } yield lockId
    }

    def completeAssignment(lockId: Int@@LockID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
      val lockApi = corpusAccessApi.corpusLockApi
      lockApi.releaseLock(lockId)
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
