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
import workflow._

object WorkflowServers {
  val servers = mutable.Map[String@@WorkflowID, (CorpusAccessApi, String@@WorkflowID) => LabelerBuilder]()

  servers.put(WorkflowID("1-page"), (c, wid) => new PageOneLabeler(c, wid))
  servers.put(WorkflowID("au-names1"), (c, wid) => new AuthorNamesLabeler1(c, wid))
}


class AuthorNamesLabeler1(
  corpusAccessApi: CorpusAccessApi,
  workflowId: String@@WorkflowID
) extends LabelerBuilder {

  val docStore: DocumentZoningApi = corpusAccessApi.docStore
  val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  val userbaseApi: UserbaseApi = corpusAccessApi.userbaseApi

  val BATCH_SIZE = 12
  def createLabeler(userId: Int@@UserID): LabelWidgetConfig = {
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
      docStore.ensureLabel(LB.Authors),
      BATCH_SIZE
    )
    println(s"createLabeler: acquired zone locks ${zoneLocks} ")

    val targetRegions = for {
      zoneLockId <- zoneLocks.toSeq
      zoneLock <- workflowApi.getZoneLock(zoneLockId).toSeq
      region <- docStore.getZone(zoneLock.zone).regions
    } yield region

    val pageOnes = targetRegions.map{ pageTargetRegion =>
      LW.pad(
        LW.targetOverlay(pageTargetRegion, overlays=List()),
        Padding.Ints(2),
        Colors.DarkSlateBlue
      )
    }

    val placeholders = Stream.continually(LW.textbox("<empty page>"))
    val widgets = (pageOnes.toStream ++ placeholders).take(BATCH_SIZE)

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
    (LB.Author, Colors.Orange),
    (LB.NoteMarkers, Colors.OliveDrab)
  )
}






class PageOneLabeler(
  corpusAccessApi: CorpusAccessApi,
  workflowId: String@@WorkflowID
) extends LabelerBuilder {

  val docStore: DocumentZoningApi = corpusAccessApi.docStore
  val workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  val userbaseApi: UserbaseApi = corpusAccessApi.userbaseApi

  val BATCH_SIZE = 12
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
      BATCH_SIZE
    )
    println(s"createLabeler: acquired zone locks ${zoneLocks}")

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
    val widgets = (pageOnes.toStream ++ placeholders).take(BATCH_SIZE)

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
