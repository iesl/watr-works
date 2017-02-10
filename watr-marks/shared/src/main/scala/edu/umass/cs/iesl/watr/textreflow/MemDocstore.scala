package edu.umass.cs.iesl.watr
package textreflow

import edu.umass.cs.iesl.watr.{geometry => G}
import edu.umass.cs.iesl.watr.{watrmarks => W}
import scala.collection.mutable
import utils.EnrichNumerics._
import TypeTags._
import TextReflowF._


class DBRelation[IDType, ModelType](
  table: mutable.HashMap[Int@@IDType, ModelType] = mutable.HashMap[Int@@IDType, ModelType]()
)(implicit O: Ordering[Int@@IDType]) {

  def all(): Seq[ModelType] = {
    val keys = table.keys.toSeq
    keys.sorted.map(table(_))
  }

  def option(id: Int@@IDType): Option[ModelType] = table.get(id)

  def unique(id: Int@@IDType): ModelType = {
    option(id).fold(
      sys.error(s"Expected unique ${id}; got None")
    )(x => x)
  }

  lazy val idGen = utils.IdGenerator[IDType]()
  def nextId(): Int@@IDType = idGen.nextId

  def insert(id: Int@@IDType, m: ModelType): Unit = {
    table.put(id, m).foreach { existing =>
      sys.error(s"failed on duplicate insert(${id}), ${m}")
    }
  }
  def update(id: Int@@IDType, m: ModelType): Option[ModelType] = {
    table.put(id, m)
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
    pagenum  : Int@@PageNum,
    pageimg  : Option[Array[Byte]],
    bleft    : Int,
    btop     : Int,
    bwidth   : Int,
    bheight  : Int
  )

  case class TargetRegion(
    prKey    : Int@@RegionID,
    page     : Int@@PageID,
    bleft    : Int,
    btop     : Int,
    bwidth   : Int,
    bheight  : Int,
    uri      : String
  )


  case class Zone(
    prKey    : Int@@ZoneID,
    document : Int@@DocumentID
  )

  case class Label(
    prKey    : Int@@LabelID,
    document : Int@@DocumentID
  )
}


class MemDocstore extends ReflowDocstore {
  object tables  {

    object documents extends DBRelation[DocumentID, Model.Document] {
      val stableId = mutable.HashMap[String@@DocumentID, Int@@DocumentID]()

      def forStableId(docId: String@@DocumentID): Option[Int@@DocumentID] = {
        stableId.get(docId)
      }

      def add(docId: String@@DocumentID): Model.Document= {
        val rec = Model.Document(nextId(), docId)
        insert(rec.prKey, rec)
        stableId.put(docId, rec.prKey)
        rec
      }
    }

    object pages extends DBRelation[PageID, Model.Page] {
      val documentFKey = mutable.HashMap[Int@@DocumentID, Int@@PageID]()
      val docIdPageNumKey = mutable.HashMap[(Int@@DocumentID, Int@@PageNum), Int@@PageID]()

      def forDocAndPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
        for {
          pageId <- docIdPageNumKey.get((docId, pageNum))
        } yield pageId
      }

      def add(docId: Int@@DocumentID, pageNum: Int@@PageNum): Model.Page = {
        val rec = Model.Page(nextId(), docId, pageNum, None, 0, 0, 0, 0)
        insert(rec.prKey, rec)
        documentFKey.put(docId, rec.prKey)
        docIdPageNumKey.put((docId, pageNum), rec.prKey)
        rec
      }

      def updateGeometry(pageId: Int@@PageID, geom:G.LTBounds): Model.Page = {
        val curr = unique(pageId)
        val G.LTBounds(l, t, w, h) = geom
        val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
        val up = curr.copy(bleft = bl, btop = bt , bwidth =bw , bheight = bh)
        update(pageId, up)
        up
      }
    }

    object zones extends DBRelation[ZoneID, Model.Zone] {
      val documentFK = mutable.HashMap[Int@@DocumentID, Int@@ZoneID]()

      def add(docId: String@@DocumentID): Model.Zone = {
        val res = for {
          docPk <- documents.forStableId(docId)
          rec = Model.Zone(nextId(), docPk)
        } yield {
          insert(rec.prKey, rec)
          rec
        }
        res.head
      }
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

    object targetregions extends DBRelation[RegionID, Model.TargetRegion] {

      def add(pageId: Int@@PageID, geom: G.LTBounds): Model.TargetRegion = {
        val G.LTBounds(l, t, w, h) = geom
        val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
        val rec = Model.TargetRegion(nextId(), pageId, bl, bt, bw, bh, "")
        insert(rec.prKey, rec)
        rec
      }
    }
  }



  import tables._

  def getDocuments(): Seq[String@@DocumentID] = {
    documents.all().map(_.docId)
  }

  def addDocument(docId: String@@DocumentID): Int@@DocumentID = {
    documents.add(docId).prKey
  }
  def getDocument(stableId: String@@DocumentID): Option[Int@@DocumentID] = {
    documents
      .forStableId(stableId)
      .map(docId => documents.unique(docId).prKey)
  }

  def addPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Int@@PageID = {
    pages.add(docId, pageNum).prKey
  }

  def getPages(docId: Int@@DocumentID): Seq[Int@@PageID] = {
    ???
  }

  def getPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
    ???
  }

  def getPageGeometry(pageId: Int@@PageID): Option[G.PageGeometry] = {
    ???
  }

  def updatePageGeometry(pageId: Int@@PageID, pageBounds: G.LTBounds): Unit = {
    pages.updateGeometry(pageId, pageBounds)
  }

  def updatePageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
    ???
  }

  def addTargetRegion(pageId: Int@@PageID, bbox: G.LTBounds): Int@@RegionID = {
    val model = targetregions.add(pageId, bbox)
    model.prKey
    // val maybeResult = for {
    //   docId <- tables.documents.forStableId(stableId)
    //   pageId <- tables.pages.forDocAndPage(docId, pageNum)
    // } yield {
    //   val tr = tables.targetregions.add(pageId, bbox)
    //   G.TargetRegion(tr.prKey, stableId, pageNum, bbox)
    // }
    // maybeResult.getOrElse {
    //   sys.error(s"addTargetRegion(${stableId})")
    // }
    // ???
  }

  def getTargetRegion(regionId: Int@@RegionID): G.TargetRegion = {
    val model = targetregions.unique(regionId)
    val modelPage = pages.unique(model.page)
    val mDocument = documents.unique(modelPage.document)
    val (l, t, w, h) = (modelPage.bleft, modelPage.btop, modelPage.bwidth, modelPage.bheight)
    val (bl, bt, bw, bh) = (itod(l), itod(t), itod(w), itod(h))
    val bbox = G.LTBounds(bl, bt, bw, bh)
    G.TargetRegion(model.prKey, mDocument.docId, modelPage.pagenum, bbox)
  }

  def addTargetRegionImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
    ???
  }

  def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID] = {
    ???
  }

  def createZone(docId: Int@@DocumentID): Int@@ZoneID = {
    ???
  }

  def getZone(zoneId: Int@@ZoneID): G.Zone = {
    ???
  }

  def setZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[G.TargetRegion]): Unit = {
    ???
  }

  def addZoneLabel(zoneId: Int@@ZoneID, label: W.Label): Unit = {
    ???
  }

  def getZonesForDocument(docId: Int@@DocumentID): Seq[Int@@ZoneID] = {
    ???
  }

  def getZonesForTargetRegion(regionId: Int@@RegionID): Seq[Int@@ZoneID] = {
    ???
  }

  def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow] = {
    ???
  }

  def mergeZones(zoneIds: Seq[Int@@ZoneID]): Int@@ZoneID = {
    ???
  }

  def deleteZone(zoneId: Int@@ZoneID): Unit = {
    ???
  }



  // select all documents+zone+labels for documents having any zones labeled "author-surname"
  //   returns Model.Document :: List[Model.Zone :: List[Model.TargetRegion] :: List[Label]]
  def getDocumentZones(docId: String@@DocumentID): Seq[G.Zone] = {
    for {
      docId  <- tables.documents.forStableId(docId)
      zoneId <- tables.zones.documentFK.get(docId)
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


  def getPage(stableId: String@@DocumentID, pageNum: Int@@PageNum): Int@@PageID = {
    val maybePage = for {
      docId <- tables.documents.forStableId(stableId)
      pageId <- tables.pages.forDocAndPage(docId, pageNum)
    } yield {
      pageId
    }
    maybePage.getOrElse {
      sys.error(s"getPage(${stableId})")
    }
  }


  def updatePageGeometry(stableId: String@@DocumentID, pageNum: Int@@PageNum, geom: G.LTBounds): Unit = {
    for {
      docId <- tables.documents.forStableId(stableId)
      pageId <- tables.pages.forDocAndPage(docId, pageNum)
    } yield {
      tables.pages.updateGeometry(pageId, geom)
    }
  }

  def createZone(docId: String@@DocumentID): G.Zone = {
    val mzone = tables.zones.add(docId)
    G.Zone(mzone.prKey, List(), List())
  }


  def addZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[G.TargetRegion]): G.Zone = {
    ???
  }


  def addTargetRegion(stableId: String@@DocumentID, pageNum: Int@@PageNum, bbox: G.LTBounds): G.TargetRegion = {
    val maybeResult = for {
      docId <- tables.documents.forStableId(stableId)
      pageId <- tables.pages.forDocAndPage(docId, pageNum)
    } yield {
      val tr = tables.targetregions.add(pageId, bbox)
      G.TargetRegion(tr.prKey, stableId, pageNum, bbox)
    }
    maybeResult.getOrElse {
      sys.error(s"addTargetRegion(${stableId})")
    }
  }

}
