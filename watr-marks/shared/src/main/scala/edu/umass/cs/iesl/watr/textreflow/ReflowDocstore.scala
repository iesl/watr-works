package edu.umass.cs.iesl.watr
package textreflow

import geometry._
import watrmarks._
import TextReflowF._

trait ReflowDocstore {
  def getDocuments(): Seq[String@@DocumentID]
  def addDocument(docId: String@@DocumentID): Int@@DocumentID
  def getDocument(docId: String@@DocumentID): Option[Int@@DocumentID]

  def addPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Int@@PageID
  def getPages(docId: Int@@DocumentID): Seq[Int@@PageID]
  def getPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID]
  def getPageGeometry(pageId: Int@@PageID): Option[LTBounds]
  def updatePageGeometry(pageId: Int@@PageID, geom: LTBounds): Unit
  def updatePageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit

  def addTargetRegion(pageId: Int@@PageID, bbox:LTBounds): Int@@RegionID
  def getTargetRegion(regionId: Int@@RegionID): TargetRegion
  def addTargetRegionImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit
  def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID]

  def createZone(docId: Int@@DocumentID): Int@@ZoneID
  def getZone(zoneId: Int@@ZoneID): Zone
  def setZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[TargetRegion]): Unit
  def addZoneLabel(zoneId: Int@@ZoneID, label: Label): Unit
  def getZonesForDocument(docId: Int@@DocumentID): Seq[Int@@ZoneID]
  def getZonesForTargetRegion(regionId: Int@@RegionID): Seq[Int@@ZoneID]
  def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow]
  def mergeZones(zoneIds: Seq[Int@@ZoneID]): Int@@ZoneID
  def deleteZone(zoneId: Int@@ZoneID): Unit


  //  def getPageVisualLines(pageId: Int@@PageID): Seq[Component]  = for {
  //  def getDocumentVisualLines(): Seq[Seq[Component]] = for {



  //  def dbgFilterComponents(pg: Int@@PageID, include: LTBounds): Unit ={
  //  def dbgFilterPages(pg: Int@@PageID): Unit ={
  //  def getZoneForComponent(cc: Int@@ComponentID): Option[Int@@ZoneID] = {
  //  def setTextReflow(cc: Component, r: TextReflow): Zone = {
  //  def getTextReflowForComponent(ccId: Int@@ComponentID): Option[TextReflow] = {
  //  def getTextReflow(zoneId: Int@@ZoneID): Option[TextReflow] = {
  //  def getPageVisualLines(pageId: Int@@PageID): Seq[Component]  = for {
  //  def getDocumentVisualLines(): Seq[Seq[Component]] = for {
  //  def getVisualLineTextReflows(): Seq[TextReflow] = for {
  //  def getTextReflows(labels: Label*): Seq[TextReflow]  = {
  //  def getZones(): Seq[Zone] = {
  //  def addRelations(rs: Seq[Relation.Record]): Unit = {
  //  def addProps(rs: Seq[Prop.PropRec]): Unit = {
  //  def bioLabeling(name: String): BioLabeling = {
  //  def addLabel(c: Component, l: Label): Component = {
  //  def removeLabel(c: Component, l: Label): Component = {
  //  def getLabels(c: Component): Set[Label] = {
  //  def getPageIndex(pageId: Int@@PageID) = pageIndexes(pageId)
  //  def removeComponent(c: Component): Unit = {
  //  def labelRegion(components: Seq[Component], role: Label): Option[RegionComponent] = {
  //  def createRegionComponent(tr: TargetRegion, role: Label): RegionComponent = {
  //  def addPageAtom(pageAtom: PageAtom): AtomicComponent = {
  //  def getComponent(id: Int@@ComponentID, pageId: Int@@PageID): Component = {
  //  def addComponent(c: Component): Component = {
  //  def addBioLabels(label: Label, node: BioNode): Unit = {
  //  def addBioLabels(label: Label, nodes: Seq[BioNode]): Unit = {
}
