package edu.umass.cs.iesl.watr
package corpora

trait DocumentAnnotationApi {
  val Rel = RelationModel

  def createAnnotation(userId: Int@@UserID, docId: Int@@DocumentID, workflowId: String@@WorkflowID): Int@@AnnotationID

  def deleteAnnotation(annotId: Int@@AnnotationID): Unit

  def getAnnotation(annotId: Int@@AnnotationID): Rel.AnnotationRec

  // def updateAnnotationStatus(annotId: Int@@AnnotationID, status: String@@StatusCode): Unit

  def updateAnnotationBody(annotId: Int@@AnnotationID, jsonRec: String): Unit

  def getAnnotations(): Seq[Int@@AnnotationID]


  // def setPageText(pageId: Int@@PageID, text: TextGrid): Unit
  // def getPageText(pageId: Int@@PageID): Option[TextGrid]

  // def addTargetRegion(pageId: Int@@PageID, bbox:LTBounds): Int@@RegionID
  // def getTargetRegion(regionId: Int@@RegionID): PageRegion

  // def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID]

  // // Construct
  // def createZone(regionId: Int@@RegionID, label: Int@@LabelID): Int@@ZoneID
  // def addZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit
  // def removeZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Option[Int@@ZoneID]

  // // Destroy
  // def deleteZone(zoneId: Int@@ZoneID): Unit

  // // Locate
  // def getZone(zoneId: Int@@ZoneID): Zone
  // def getZoneLabelsForDocument(docId: Int@@DocumentID): Seq[Int@@LabelID]
  // def getZonesForDocument(docId: Int@@DocumentID, label: Int@@LabelID): Seq[Int@@ZoneID]
  // def getZoneForRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID]

  // def ensureLabel(label: Label): Int@@LabelID
  // def getLabel(labelId: Int@@LabelID): Label

  // def getZoneTextAsJsonStr(zoneId: Int@@ZoneID): Option[String]
  // def setZoneText(zoneId: Int@@ZoneID, textgrid: TextGrid): Unit
  // ///////////////////////
  // /// Derived operations

  // def getDocumentZones(docId: Int@@DocumentID, label: Label): Seq[Zone] =
  //   getZonesForDocument(docId, ensureLabel(label)).map(getZone(_))

  // def getDocumentZones(docId: Int@@DocumentID): Seq[Int@@ZoneID] = {
  //   for {
  //     labelId <- getZoneLabelsForDocument(docId)
  //     zoneId <- getZonesForDocument(docId, labelId)
  //   } yield zoneId
  // }


  // def getPageZones(stableId: String@@DocumentID, pageNum: Int@@PageNum, label: Label): Seq[Zone] = for {
  //   docId     <- getDocument(stableId).toSeq
  //   pageId    <- getPage(docId, pageNum).toSeq
  //   zone      <- getPageZones(pageId, label)
  // } yield zone

  // def getPageZones(pageId: Int@@PageID, label: Label): Seq[Zone] = {

  //   val zones = for {
  //     regionId  <- getTargetRegions(pageId)
  //     zoneId    <- getZoneForRegion(regionId, label)
  //   } yield { getZone(zoneId) }

  //   zones
  //     .sortBy(_.order)
  //     .foldLeft(Seq[Zone]()){ case (acc, e) =>
  //       if (acc.exists(_.id==e.id)) acc
  //       else acc :+ e
  //     }

  // }

  // def getPageVisualLines(stableId: String@@DocumentID, pageNum: Int@@PageNum): Seq[Zone] =
  //   getPageZones(stableId, pageNum, LB.VisualLine)

  // def getPageVisualLines(pageId: Int@@PageID): Seq[Zone] =
  //   getPageZones(pageId, LB.VisualLine)


  // def ensurePageRegion(pageRegion: PageRegion): Option[Int@@RegionID] = {
  //   val stableId = pageRegion.page.stableId
  //   val pageNum =  pageRegion.page.pageNum
  //   ensureTargetRegion(stableId, pageNum, pageRegion.bbox)
  // }

  // def ensureTargetRegion(stableId: String@@DocumentID, pageNum: Int@@PageNum, bbox:LTBounds): Option[Int@@RegionID] = {
  //   for {
  //     docId     <- getDocument(stableId)
  //     pageId    <- getPage(docId, pageNum)
  //   } yield {
  //     addTargetRegion(pageId, bbox)
  //   }
  // }

  // def labelRegions(label: Label, regions: Seq[PageRegion]): Option[Int@@ZoneID] = {
  //   val labelId = ensureLabel(label)

  //   for {
  //     pageRegion <- regions.headOption
  //     headRegion <- ensurePageRegion(pageRegion)
  //   } yield {
  //     val zoneId = createZone(headRegion, labelId)
  //     for {
  //       tailRegion <- regions.tail
  //       tailRegionId <- ensurePageRegion(tailRegion)
  //     }  {
  //       addZoneRegion(zoneId, tailRegionId)
  //     }
  //     zoneId
  //   }
  // }


  // def exportDocumentZones(labelFilters: Seq[Label]=List()): DocumentZoneExport = {}
  // def importDocumentZones(zoneExport: DocumentZoneExport): Unit = {}
}
