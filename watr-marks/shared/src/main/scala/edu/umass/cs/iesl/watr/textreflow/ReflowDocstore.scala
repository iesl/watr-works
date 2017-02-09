package edu.umass.cs.iesl.watr
package textreflow

import geometry._
import watrmarks._

trait ReflowDocstore {

  def createZone(docId: String@@DocumentID): Zone
  def getZone(zoneId: Int@@ZoneID): Zone
  def addZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[TargetRegion]): Zone
  def addZoneLabel(zoneId: Int@@ZoneID, label: Label): Zone

  def getDocumentZones(docId: String@@DocumentID): Seq[Zone]
  def addDocument(docId: String@@DocumentID): Unit

  def addPage(docId: String@@DocumentID, pageNum: Int@@PageNum): Int@@PageID
  def updatePageGeometry(docId: String@@DocumentID, pageNum: Int@@PageNum, geom: LTBounds): Unit
  // def updatePageImage(docId: String@@DocumentID, pageNum: Int@@PageNum, geom: LTBounds): Unit

  def addTargetRegion(docId: String@@DocumentID, pageId: Int@@PageNum, bbox:LTBounds): TargetRegion


  // Multipage Index
  //    val pageIndexes = mutable.HashMap[Int@@PageID, PageIndex]()
  //    val zoneMap = mutable.HashMap[Int@@ZoneID, Zone]()
  //    val labelToZones: mutable.HashMap[Label, mutable.ArrayBuffer[Int@@ZoneID]] = mutable.HashMap()
  //    val zoneToTextReflow: mutable.HashMap[Int@@ZoneID, TextReflow] = mutable.HashMap()
  //    val componentIdToZoneId: mutable.HashMap[Int@@ComponentID, Int@@ZoneID] = mutable.HashMap()
  //    private def getLabelToZoneBuffer(l: Label): mutable.ArrayBuffer[Int@@ZoneID] = {
  //    val relations = mutable.ArrayBuffer[Relation.Record]()
  //    val props = mutable.ArrayBuffer[Prop.PropRec]()
  //    type BioLabeling = mutable.MutableList[BioNode]
  //    val bioLabelings = mutable.Map[String, BioLabeling]()
  //  def getDocumentID(): String@@DocumentID = docId
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
  //  private def getLabelToZoneBuffer(l: Label): mutable.ArrayBuffer[Int@@ZoneID] = {
  //  def addZone(z: Zone): Zone =  {
  //  def addRelations(rs: Seq[Relation.Record]): Unit = {
  //  def addProps(rs: Seq[Prop.PropRec]): Unit = {
  //  def bioLabeling(name: String): BioLabeling = {
  //  def setChildrenWithLabel(c: Component, l: Label, tree: Seq[Int@@ComponentID]):Unit = {
  //  def getChildrenWithLabel(c: Component, l: Label): Option[Seq[Int@@ComponentID]] = {
  //  def getChildren(c: Component, l: Label): Option[Seq[Component]] = {
  //  def getPageForComponent(c: Component): Int@@PageID = {
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
  //  def getPageGeometry(p: Int@@PageID) = pageIndexes(p).pageGeometry
  //  def getPages(): List[Int@@PageID] = {
  //  def addPage(pageGeometry: PageGeometry): PageIndex = {
  //  def addBioLabels(label: Label, node: BioNode): Unit = {
  //  def addBioLabels(label: Label, nodes: Seq[BioNode]): Unit = {
  //  def loadTextReflows(
  //  def loadSpatialIndices(
}
