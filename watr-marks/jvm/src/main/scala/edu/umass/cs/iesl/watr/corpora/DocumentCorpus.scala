package edu.umass.cs.iesl.watr
package corpora

import geometry._
import watrmarks._
import textreflow._
import TextReflowF._

import watrmarks.{StandardLabels => LB}

trait DocumentCorpus {
  val Rel = RelationModel

  def getDocuments(n: Int=Int.MaxValue, skip: Int=0): Seq[String@@DocumentID]
  def getDocumentCount(): Int
  def addDocument(stableId: String@@DocumentID): Int@@DocumentID
  def getDocument(stableId: String@@DocumentID): Option[Int@@DocumentID]

  def addPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Int@@PageID
  def getPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID]
  def getPageDef(pageId: Int@@PageID): Option[Rel.Page]
  def getPages(docId: Int@@DocumentID): Seq[Int@@PageID]
  def getPageGeometry(pageId: Int@@PageID): LTBounds
  def setPageGeometry(pageId: Int@@PageID, geom: LTBounds): Unit
  def setPageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit
  def getPageImage(pageId: Int@@PageID): Option[Array[Byte]]

  def addTargetRegion(pageId: Int@@PageID, bbox:LTBounds): Int@@RegionID
  def getTargetRegion(regionId: Int@@RegionID): TargetRegion

  def setTargetRegionImage(regionId: Int@@RegionID, bytes: Array[Byte]): Unit
  def getTargetRegionImage(regionId: Int@@RegionID): Option[Array[Byte]]
  def deleteTargetRegionImage(regionId: Int@@RegionID): Unit

  def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID]


  // Construct
  def createZone(regionId: Int@@RegionID, label: Label): Int@@ZoneID
  def addZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit
  def removeZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Option[Int@@ZoneID]

  // Destroy
  def deleteZone(zoneId: Int@@ZoneID): Unit

  // Locate
  def getZone(zoneId: Int@@ZoneID): Zone
  def getZoneLabelsForDocument(docId: Int@@DocumentID): Seq[Label]
  def getZonesForDocument(docId: Int@@DocumentID, label: Label): Seq[Int@@ZoneID]
  def getZoneForRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID]

  // Get Text
  def getModelTextReflowForZone(zoneId: Int@@ZoneID): Option[Rel.TextReflow]
  def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow]
  def setTextReflowForZone(zoneId: Int@@ZoneID, textReflow: TextReflow): Unit



  ///////////////////////
  /// Derived operations

  def getPageVisualLines(stableId: String@@DocumentID, pageNum: Int@@PageNum): Seq[Zone] = for {
    docId     <- getDocument(stableId).toSeq
    pageId    <- getPage(docId, pageNum).toSeq
    zone      <- getPageVisualLines(pageId)
  } yield zone

  def getPageVisualLines(pageId: Int@@PageID): Seq[Zone] = for {
    regionId  <- getTargetRegions(pageId)
    zoneId    <- getZoneForRegion(regionId, LB.VisualLine)
  } yield { getZone(zoneId) }

  def getTextReflowForTargetRegion(regionId: Int@@RegionID): Option[TextReflow] = for {
    zoneId  <- getZoneForRegion(regionId, LB.VisualLine)
    reflow <- getTextReflowForZone(zoneId)
  } yield reflow


}
