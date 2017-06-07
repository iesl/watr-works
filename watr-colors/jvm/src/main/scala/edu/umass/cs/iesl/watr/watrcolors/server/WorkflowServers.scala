package edu.umass.cs.iesl.watr
package watrcolors
package server

import scala.collection.mutable

import watrmarks.{StandardLabels => LB}
import watrmarks._
import utils.Color
import textboxing.{TextBoxing => TB}, TB._

import geometry._
import corpora._
import labeling._
import labeling.data._
import TypeTags._

import utils.Colors

object WorkflowServers {
  val servers = mutable.Map[String@@WorkflowID, (DocumentCorpus, Int@@UserID) => LabelerBuilder]()

  // def init(): Unit = {
  //   servers.put(WorkflowID("1-page"), (c, u) => new PageOneLabeler(c, u))
  // }
  // init()
}


class PageOneLabeler(
  docStore: DocumentCorpus,
  workflowDef: WorkflowDef,
  userId: Int@@UserID
) extends LabelerBuilder {

  def workflowApi: WorkflowApi = ???

  def createLabeler(): (LabelWidget, LabelerIdentifier) = {
    // Release any prior locks held by this user
    workflowApi.getUserLocks(userId).foreach { lockGroupId =>
      workflowApi.releaseZoneLocks(lockGroupId)
    }

    val lockGroupId = workflowApi.makeLockGroup(userId)

    val zoneLocks = workflowApi.aquireZoneLocks(
      lockGroupId,
      ZoneLock.Status.Unexamined,
      docStore.ensureLabel(LB.DocumentPages),
      10
    )

    val pageOnes = zoneLocks.map{ zoneLock =>
      docStore.getZone(zoneLock.zone).regions.headOption
    }.flatten


    val (widget0, labelerId0) = singlePageLabeler(pageOnes)

    (widget0, labelerId0)
  }

  def singlePageLabeler(targetRegions: Seq[TargetRegion]): (LabelWidget, LabelWidgetConfig) = {

    val pageOnes = targetRegions.map{ pageTargetRegion =>
      LW.pad(
        LW.targetOverlay(pageTargetRegion, overlays=List()),
        Padding.Ints(2),
        Colors.DarkSlateBlue
      )
    }

    val placeholders = Stream.continually(LW.textbox("<empty page>"))
    val widgets = (pageOnes.toStream ++ placeholders).take(10)

    val rows = widgets.grouped(4).toList.map{ws =>
      LW.row(ws:_*)
    }

    val body = LW.col(rows:_*)

    val config = LabelWidgetConfig(
      workflowDef.workflow
    )

    val updatedIdentifier =  DocumentLabelerIdentifier(
      DocumentID("???"), "single-page",
      Pagination(0, PageNum(0), None),
      targetLabels()
    )

    (body, config)
  }

  def targetLabels(): Map[Label, Color] = Map(
    (LB.Title, Colors.Wheat),
    (LB.Authors, Colors.Orange),
    (LB.Abstract, Colors.MediumTurquoise),
    (LB.Affiliations, Colors.OliveDrab)
  )
}
