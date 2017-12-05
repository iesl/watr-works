package edu.umass.cs.iesl.watr
package watrcolors

import corpora._
import org.http4s._
import org.http4s.circe._
import _root_.io.circe
import circe._
// import circe.syntax._
import circe.literal._
import watrmarks.Label
import TypeTags._
import workflow._

import cats.effect.IO
import services._

class WorkflowRestApiSpec extends Http4sSpec with DatabaseTest {
  behavior of "Workflow Rest API"


  def workflowService() = new CurationWorkflowServices {
    def corpusAccessApi: CorpusAccessApi = CorpusAccessApi(reflowDB, null)
  }

  def endpoints() = workflowService().curationWorkflowEndpoints

  val VisualLine: Label = Label.auto
  val DocumentPages: Label = Label.auto
  val Authors: Label = Label.auto

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
      workflowApi.defineWorkflow(s"wf-${l.fqn}-${i}", s"Annot. ${l.fqn} #$i", l)
    }
  }

  def initUsers(n: Int): Seq[Int@@UserID] = {
    0 until n map { i =>
      userbaseApi.addUser(EmailAddr(s"user${i}@umass.edu"))
    }
  }

  initEmpty()

  initUsers(2)
  initWorkflows(VisualLine, 2)
  initWorkflows(Authors, 2)

  it should "get a list of available workflows" in {
    val req = Request[IO](uri = Uri(path = "/workflows"))
    // val res = endpoints.orNotFound(req).as[Json].unsafeRunSync()
    val expect = {
      json"""
        [
          {
            "workflow" : "wf-VisualLine-0",
            "description" : "Annot. VisualLine #0",
            "labelId" : 1
          },
          {
            "workflow" : "wf-VisualLine-1",
            "description" : "Annot. VisualLine #1",
            "labelId" : 1
          },
          {
            "workflow" : "wf-Authors-0",
            "description" : "Annot. Authors #0",
            "labelId" : 2
          },
          {
            "workflow" : "wf-Authors-1",
            "description" : "Annot. Authors #1",
            "labelId" : 2
          }
        ]
      """
    }
    endpoints.run(req).fold(
      fail
    )(response => {
      val jsonResp = response.as[Json].unsafeRunSync()
      // println(resp)
      assert(expect === jsonResp)

    })

  }

  it should "get workflow report" in new EmptyDatabase {
    addSampleDocs(3) // 9 zones/doc, so 27 total zones
    val users = initUsers(2)
    val workflows = initWorkflows(VisualLine, 2)

    workflowApi.lockUnassignedZones(users(0), workflows(0), 3)
    workflowApi.lockUnassignedZones(users(1), workflows(0), 4)
    workflowApi.getLockedZones(users(0)).drop(1).foreach { zoneLockId =>
      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Completed)
      workflowApi.releaseZoneLock(zoneLockId)
    }

    workflowApi.getLockedZones(users(1)).drop(2).foreach { zoneLockId =>
      workflowApi.updateZoneStatus(zoneLockId, ZoneLockStatus.Skipped)
    }
    val expect = {
      json"""
        {
          "unassignedCount" : 20,
          "statusCounts" : {
            "Assigned" : 3,
            "Completed" : 2,
            "Skipped" : 2
          },
          "userAssignmentCounts" : {
            "1" : 1,
            "2" : 4
          }
        }
      """
    }

    val req = Request[IO](uri = Uri(path = "/workflow/wf-VisualLine-0/report"))
    endpoints.run(req).fold(
      fail
    )(response => {
      val jsonResp = response.as[Json].unsafeRunSync()
      assert(expect === jsonResp)
    })
  }

  it should "get next workflow assignment" in new EmptyDatabase {
    addSampleDocs(1)
    initUsers(2)
    initWorkflows(DocumentPages, 2)

    val expect = {
      json"""
         [
           {
             "id" : 10,
             "regions" : [
               {
                 "page" : {
                   "stableId" : "doc#0",
                   "pageNum" : 0,
                   "pageId" : 1
                 },
                 "bbox" : {
                   "left" : 10,
                   "top" : 10,
                   "width" : 2980,
                   "height" : 2980
                 },
                 "regionId" : 10
               },
               {
                 "page" : {
                   "stableId" : "doc#0",
                   "pageNum" : 1,
                   "pageId" : 2
                 },
                 "bbox" : {
                   "left" : 10,
                   "top" : 10,
                   "width" : 2980,
                   "height" : 2980
                 },
                 "regionId" : 11
               },
               {
                 "page" : {
                   "stableId" : "doc#0",
                   "pageNum" : 2,
                   "pageId" : 3
                 },
                 "bbox" : {
                   "left" : 10,
                   "top" : 10,
                   "width" : 2980,
                   "height" : 2980
                 },
                 "regionId" : 12
               }
             ],
             "label" : "DocumentPages",
             "order" : 0
           }
         ]
      """
    }

    val uq = Query.fromPairs(("user", "user0@umass.edu"))
    val req = Request[IO](uri = Uri(path = "/workflow/wf-DocumentPages-0/assignment", query=uq))
    endpoints.run(req).fold(
      fail
    )(response => {
      val jsonResp = response.as[Json].unsafeRunSync()
      assert(expect === jsonResp)
    })

  }
}
