package edu.umass.cs.iesl.watr
package watrcolors

import corpora._
// import org.http4s._
// import _root_.io.circe
// import circe.literal._
import watrmarks.Label
import TypeTags._
import workflow._

import services._

import watrmarks._

// class CurationWorkflowSpec extends DatabaseFreeSpec with DatabaseTest with DocSegLabels {
class CurationWorkflowSpec extends  DatabaseTest with DocSegLabels {
  behavior of "Curation Workflow"

  def workflowService() = new CurationWorkflow {
    def corpusAccessApi: CorpusAccessApi = CorpusAccessApi(reflowDB, null)
    def docStore = corpusAccessApi.docStore
    def workflowApi = corpusAccessApi.workflowApi
    def userbaseApi = corpusAccessApi.userbaseApi
  }


  val Authors   = Label.auto
  val Author    = Label.auto
  val FirstName = Label.auto
  val LastName  = Label.auto

  val NameLabelSchema =
    LabelSchemas(List(
      LabelSchema(
        Authors, Some(('a', 's')), None, List(
          LabelSchema(
            Author, Some(('a', 'u')), None, List(
              LabelSchema(FirstName),
              LabelSchema(LastName)))))))


  def addSampleDocs(n: Int): Seq[String@@DocumentID] = {
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr"
    )

    (0 until n).map{ i =>
      val stableId = DocumentID(s"doc#${i}")
      addDocument(stableId, doc)
      stableId
    }
  }

  def initWorkflows(l: Label, n: Int): Seq[String@@WorkflowID] = {
    0 until n map { i =>
      workflowApi.defineWorkflow(
        s"wf-${l.fqn}-${i}",
        s"Annot. ${l.fqn} #$i",
        Some(l),
        NameLabelSchema
      )
    }
  }

  def initUsers(n: Int): Seq[Int@@UserID] = {
    0 until n map { i =>
      userbaseApi.addUser(EmailAddr(s"user${i}@umass.edu"))
    }
  }

  initEmpty()

  val users = initUsers(2)
  val sampleDocs = addSampleDocs(3) // 9 zones/doc, so 27 total zones
  val workflows0 = initWorkflows(FullPdf, 3)

  lazy val service = workflowService()

  // it should "get a list of available workflows" in {

  //   val actual =  service.GET_workflows()
  //   // println(actual)
  //   // assertResult{
  //   //   json"""
  //   //     [
  //   //       {
  //   //         "workflow" : "wf-VisualLine-0",
  //   //         "description" : "Annot. VisualLine #0",
  //   //         "targetLabel" : { "prKey" : 1, "key" : "VisualLine" },
  //   //         "curatedLabels" : [
  //   //           { "prKey" : 3, "key" : "Sup" },
  //   //           { "prKey" : 4, "key" : "Sub" }]
  //   //       },
  //   //       {
  //   //         "workflow" : "wf-VisualLine-1",
  //   //         "description" : "Annot. VisualLine #1",
  //   //         "targetLabel" : { "prKey" : 1, "key" : "VisualLine" },
  //   //         "curatedLabels" : [
  //   //           { "prKey" : 3, "key" : "Sup" },
  //   //           { "prKey" : 4, "key" : "Sub" }
  //   //         ]
  //   //       },
  //   //       {
  //   //         "workflow" : "wf-Authors-0",
  //   //         "description" : "Annot. Authors #0",
  //   //         "targetLabel" : { "prKey" : 5, "key" : "Authors" },
  //   //         "curatedLabels" : [
  //   //           { "prKey" : 3, "key" : "Sup" },
  //   //           { "prKey" : 4, "key" : "Sub" }
  //   //         ]
  //   //       }
  //   //     ]
  //   //   """
  //   // } { actual }
  // }

  it should "get workflow report" in {
    workflowApi.lockUnassignedZones(users(0), workflows0(0), 3)
    workflowApi.lockUnassignedZones(users(1), workflows0(0), 4)

    workflowApi.getLockedZones(users(0)).drop(1).foreach { zoneLockId =>
      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Completed)
      workflowApi.releaseZoneLock(zoneLockId)
    }

    workflowApi.getLockedZones(users(1)).drop(2).foreach { zoneLockId =>
      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Skipped)
    }

    val actual = service.GET_workflows_report(workflows0(0))
    println("Workflow Report==============")
    println(actual)

    val curatorAssignments = service.GET_curators_assignments(users(0))
    println(s"Curator ${users(0)} Assignments ==============")
    println(curatorAssignments)

    val documentAssignments = service.GET_documents(DocumentID(s"doc#0"))
    println(s"Document Assignments ==============")
    println(documentAssignments)

    // assertResult{
    //   json"""
    //     {
    //       "unassignedCount" : 20,
    //       "statusCounts" : {
    //         "Assigned" : 3,
    //         "Completed" : 2,
    //         "Skipped" : 2
    //       },
    //       "userAssignmentCounts" : {
    //         "1" : 1,
    //         "2" : 4
    //       }
    //     }
    //   """
    // } { actual }
  }


  // it should "get next workflow assignment" in {
  //   workflowApi.lockUnassignedZones(users(0), workflows0(0), 3)
  //   val actual = service.POST_workflows_assignments(workflows0(0), users(0))

  //   // assertResult{
  //   //   json"""
  //   //    [{
  //   //       "id" : 8,
  //   //       "regions" : [
  //   //         {
  //   //           "page" : {
  //   //             "stableId" : "doc#0",
  //   //             "pageNum" : 2,
  //   //             "pageId" : 3
  //   //           },
  //   //           "bbox" : { "left" : 10, "top" : 1010, "width" : 2980, "height" : 980 },
  //   //           "regionId" : 8
  //   //         }
  //   //       ],
  //   //       "label" : "VisualLine",
  //   //       "order" : 7,
  //   //       "glyphDefs" : "{\"rows\":[{\"line\":0,\"text\":\"mno\",\"loci\":[[[\"m\",2,[10,1010,980,980]]],[[\"n\",2,[1010,1010,980,980]]],[[\"o\",2,[2010,1010,980,980]]]]}]}"
  //   //     }]
  //   //   """
  //   // } { actual }
  // }


}
