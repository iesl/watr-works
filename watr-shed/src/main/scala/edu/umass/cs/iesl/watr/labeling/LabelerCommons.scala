package edu.umass.cs.iesl.watr
package labeling

import workflow._
import corpora._
import watrmarks.Label

import geometry._
trait ServerLabelerBuilder extends LabelerBuilder {
  def corpusAccessApi: CorpusAccessApi

  def docStore: DocumentZoningApi = corpusAccessApi.docStore
  def workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  def userbaseApi: UserbaseApi = corpusAccessApi.userbaseApi

  def queryLabel: Label
  def batchSize: Int

  def createLabelerForUser(userId: Int@@UserID): LabelWidgetConfig = {
    lockTargetZones(userId)
  }

  private def lockTargetZones(userId: Int@@UserID): LabelWidgetConfig = {
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
      docStore.ensureLabel(queryLabel),
      batchSize
    )
    println(s"createLabeler: acquired zone locks ${zoneLocks} ")


    val zones = for {
      zoneLockId <- zoneLocks.toSeq
      zoneLock <- workflowApi.getZoneLock(zoneLockId).toSeq
    } yield {
      docStore.getZone(zoneLock.zone)
    }

    createLabeler(zones)

  }

  def ensureTargetRegion(pageRegion: PageRegion): TargetRegion = {
    docStore.getTargetRegion(
      docStore.addTargetRegion(pageRegion.page.pageId, pageRegion.bbox)
    )
  }

}
