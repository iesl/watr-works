package edu.umass.cs.iesl.watr
package docstore

import edu.umass.cs.iesl.watr.{geometry => G}
import edu.umass.cs.iesl.watr.{watrmarks => W}
import scala.collection.mutable
import utils.EnrichNumerics._
import TypeTags._

trait DBModelType[IDType] {
  def id: Int@@IDType
}

trait DBResults[IDType, ModelType] {
  // def id: Int@@IDType

  def option(): Option[ModelType]

  def unique(): Some[ModelType] = {
    option().fold(
      sys.error(s"Expected unique result; got None")
    )(Some(_))

  }
}
object DBResults {
  def apply[IDType, ModelType](maybeM: Option[ModelType]): DBResults[IDType, ModelType] =
    new DBResults[IDType, ModelType] {
      // def id = idKey
      def option(): Option[ModelType] = maybeM
    }

}



class DBRelation[IDType, ModelType](
  table: mutable.HashMap[Int@@IDType, ModelType] = mutable.HashMap[Int@@IDType, ModelType]()
) {

  def option(id: Int@@IDType): Option[ModelType] = table.get(id)

  def unique(id: Int@@IDType): Some[ModelType] = {
    option(id).fold(
      sys.error(s"Expected unique ${id}; got None")
    )(Some(_))
  }

  def truncate(): Unit = {}

  lazy val idGen = utils.IdGenerator[IDType]()
  def nextId(): Int@@IDType = idGen.nextId

  def insert(id: Int@@IDType, m: ModelType): Unit = {
    val oldValue = table.put(id, m)
    // TODO make sure oldValue is None
  }
}



object Model {

  case class Document(
    prKey: Int@@DocumentID,
    docId: String@@DocumentID
  )

  case class Page(
    prKey    : Int@@PageID,
    document : Int@@DocumentID,
    pagenum  : Int,
    pageimg  : Option[Array[Byte]],
    bleft    : Int,
    btop     : Int,
    bwidth   : Int,
    bheight  : Int
  )


  case class Zone(
    prKey    : Int@@ZoneID,
    zoneId  : Int@@ZoneID, // TODO this is a legacy value, can be removed
    document : Int@@DocumentID
  )

  case class Label(
    prKey    : Int@@LabelID,
    document : Int@@DocumentID
  )
}


object MemDocstore extends Docstore {
  object tables  {

    object documents extends DBRelation[DocumentID, Model.Document] {
      val stableId = mutable.HashMap[String@@DocumentID, Int@@DocumentID]()

      def forStableId(docId: String@@DocumentID): DBResults[DocumentID, Model.Document] = {
        val res = for {
          docId <- stableId.get(docId)
          doc <- option(docId)
        } yield doc
        DBResults[DocumentID, Model.Document](res)
      }

      def add(docId: String@@DocumentID): Model.Document= {
        val newId = nextId()
        val rec = Model.Document(newId, docId)
        insert(newId, rec)
        rec
      }
    }

    object pages extends DBRelation[PageID, Model.Page] {
      val documentFKey = mutable.HashMap[Int@@DocumentID, Int@@PageID]()

      def add(m: Model.Page): Model.Page = {
        val newId = nextId()
        val rec = m.copy(prKey=newId)
        insert(newId, rec)
        rec
      }
    }

    object zones extends DBRelation[ZoneID, Model.Zone] {
      val documentFK = mutable.HashMap[Int@@DocumentID, Int@@ZoneID]()

    }

    object labels extends DBRelation[LabelID, Model.Label]{
      val forKey = mutable.HashMap[String, Int@@LabelID]()
    }

    object zoneToLabel {
      val table = mutable.HashMap[Int@@ZoneID, mutable.HashSet[Int@@LabelID]]()

      def forZone(zoneId: Int@@ZoneID): Seq[Int@@LabelID]= {
        table.get(zoneId)
          .getOrElse(Seq())
          .toSeq
      }

    }

  }

  // select all documents+zone+labels for documents having any zones labeled "author-surname"
  //   returns Model.Document :: List[Model.Zone :: List[Model.TargetRegion] :: List[Label]]
  def getDocumentZones(docId: String@@DocumentID): Seq[G.Zone] = {
    for {
      doc  <- tables.documents.forStableId(docId).unique
      zoneId <- tables.zones.documentFK.get(doc.prKey)
      zone   <- tables.zones.option(zoneId)
      // get all labels for this zone
      // labelIds <- tables.zoneToLabel.forZone(zoneId)
      // get all targetRegions for this zone

    } yield {

      val targetRegions = List[G.TargetRegion]()
      val labels = List[W.Label]()
      G.Zone(zoneId, targetRegions, labels)
    }
    ???
  }

  def getDocument(docId: String@@DocumentID): Option[Model.Document] = {
    for {
      doc  <- tables.documents.forStableId(docId).unique
    } yield {
      doc
    }
  }

  def addDocument(docId: String@@DocumentID): Model.Document = {
    tables.documents.add(docId)
  }

  def addPage(docId: String@@DocumentID, pageGeometry: G.PageGeometry): Model.Page = {
    // val (bl, bt, bw, bh) = (itod(l), itod(t), itod(w), itod(h))
    // LTBounds(bl, bt, bw, bh)
    val G.LTBounds(l, t, w, h) = pageGeometry.bounds
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    val wer = for {
      doc <- getDocument(docId)
    } yield {
      val rec0 = Model.Page(
        PageID(0),
        doc.prKey,
        pageGeometry.id.unwrap,
        None,
        bl, bt, bw, bh
      )
      tables.pages.add(rec0)
    }

    ???
  }
}

trait Docstore {

  def getDocumentZones(docId: String@@DocumentID): Seq[G.Zone]
  def addDocument(docId: String@@DocumentID): Model.Document
  def addPage(docId: String@@DocumentID, pageGeometry: G.PageGeometry): Model.Page


  // TextReflowDB
  //  def addSegmentation(ds: DocumentSegmentation): Unit = {
  //  def insertPagesInfo(docPk: Int) = for {
  //  def insertMultiPageIndex(docId: String@@DocumentID, mpageIndex: MultiPageIndex): ConnectionIO[List[Unit]] = {
  //  def selectTargetRegions(docId: String@@DocumentID, pageId: Int@@PageID): List[TargetRegion] = {
  //  def selectZones(docId: String@@DocumentID, pageId: Int@@PageID, label: Label): List[Zone] = {
  //  def insertZone(docId: String@@DocumentID, zoneId: Int@@ZoneID): ConnectionIO[Int] = {
  //  def insertTextReflow(zonePk: Int, textReflow: TextReflow): ConnectionIO[Int] = {
  //  def getTextReflowForZone(zone: Zone): Option[TextReflow] = {
  //  def getTextReflowsForTargetRegion(targetRegion: TargetRegion): List[(Zone, TextReflow)] = {
  //  def selectZonesForTargetRegion(targetRegion: TargetRegion, label: Label): ConnectionIO[List[Zone]] = {
  //  def selectTextReflowForZone(zone: Zone): ConnectionIO[Option[TextReflow]] = {
  //  def insertPageGeometry(docPrKey: Int, pageGeometry: PageGeometry, pageImage: Image): ConnectionIO[Int] = {
  //  def insertDocumentID(docId: String@@DocumentID): ConnectionIO[Int] = {
  //  def getDocuments(): List[String@@DocumentID] = {
  //  def selectDocumentID(docId: String@@DocumentID): ConnectionIO[Int] = {
  //  def hasDocumentID(docId: String@@DocumentID): Boolean = {
  //  def getOrInsertDocumentID(docId: String@@DocumentID): ConnectionIO[Int] = {
  //  def selectPageGeometry(docId: String@@DocumentID, pageId: Int@@PageID): ConnectionIO[Option[PageGeometry]] = {
  //  def selectPageImage(docId: String@@DocumentID, pageId: Int@@PageID): ConnectionIO[Option[Image]] = {
  //  def deleteTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Int] = {
  //  def selectTargetRegionImageBytes(targetRegion: TargetRegion): ConnectionIO[Option[Array[Byte]]] = {
  //  def selectTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Option[Image]] = {
  //  def ensureLabel(label: Label): ConnectionIO[Int] = for {
  //  def linkZoneToLabel(zonePk: Int, label: Label): ConnectionIO[Unit] = {
  //  def linkZoneToTargetRegion(zonePk: Int, targetRegionPk: Int): ConnectionIO[Unit] = {
  //  def selectPage(docPk: Int, pageId: Int@@PageID): ConnectionIO[Option[Int]] = {
  //  def ensureTargetRegion(targetRegion: TargetRegion): ConnectionIO[Int] = {
  //  def insertTargetRegion(targetRegion: TargetRegion): ConnectionIO[Int] = {
  //  def insertTargetRegionImage(targetRegion: TargetRegion, image: Image): ConnectionIO[Unit] = {
  //  def getPageGeometry(docId: String@@DocumentID, pageId: Int@@PageID): Option[PageGeometry]= {
  //  def getPageImageAndGeometry(docId: String@@DocumentID, pageId: Int@@PageID): Option[(Image, PageGeometry)] = {
  //  def overwriteTargetRegionImage(targetRegion: TargetRegion, image: Image): Unit = {
  //  def putTargetRegionImage(targetRegion: TargetRegion, image: Image): Unit = {
  //  def getOrCreateTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Image] = {
  //  def getOrCreateTargetRegionImageBytes(targetRegion: TargetRegion): ConnectionIO[Array[Byte]] = {
  //  def serveImageWithURI(targetRegion: TargetRegion): Array[Byte] = {







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
