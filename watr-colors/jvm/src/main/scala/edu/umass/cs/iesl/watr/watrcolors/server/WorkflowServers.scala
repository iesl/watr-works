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
import utils.Colors
// import corpora.{RelationModel => Rel}
import TypeTags._

object WorkflowServers {
  val servers = mutable.Map[String@@WorkflowID, (DocumentCorpus, String@@WorkflowID) => LabelerBuilder]()

  servers.put(WorkflowID("1-page"), (c, wid) => new PageOneLabeler(c, wid))
}


class PageOneLabeler(
  docStore: DocumentCorpus,
  workflowId: String@@WorkflowID
) extends LabelerBuilder {

  val workflowApi: WorkflowApi = docStore.workflowApi
  val userbaseApi: UserbaseApi = docStore.userbaseApi

  def createLabeler(userId: Int@@UserID): LabelWidgetConfig = {
    // Release any prior locks held by this user
    println(s"createLabeler for ${userId}")

    workflowApi.getUserLockGroup(userId).foreach { lockGroupId =>
      println(s"createLabeler: releasing old lock for ${lockGroupId}")
      workflowApi.releaseZoneLocks(lockGroupId)
    }

    val lockGroupId = workflowApi.makeLockGroup(userId)
    println(s"createLabeler: makeLockGroup ${lockGroupId}")

    println(s"createLabeler: acquiring zone locks ")
    val zoneLocks = workflowApi.aquireZoneLocks(
      lockGroupId,
      docStore.ensureLabel(LB.DocumentPages),
      10
    )
    println(s"createLabeler: acquired zone locks ${zoneLocks} ")

    val pageOnes = for {
      zoneLockId <- zoneLocks
      zoneLock <- workflowApi.getZoneLock(zoneLockId)
      region <- docStore.getZone(zoneLock.zone).regions.headOption
    } yield region


    singlePageLabeler(pageOnes)

  }

  def singlePageLabeler(targetRegions: Seq[TargetRegion]): LabelWidgetConfig = {

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

    val widget = LW.col(rows:_*)

    LabelWidgetConfig(
      workflowId,
      widget
    )
  }

  def targetLabels(): Map[Label, Color] = Map(
    (LB.Title, Colors.Wheat),
    (LB.Authors, Colors.Orange),
    (LB.Abstract, Colors.MediumTurquoise),
    (LB.Affiliations, Colors.OliveDrab)
  )
}
