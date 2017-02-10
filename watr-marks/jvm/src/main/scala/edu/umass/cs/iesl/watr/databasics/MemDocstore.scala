package edu.umass.cs.iesl.watr
package textreflow

import edu.umass.cs.iesl.watr.{geometry => G}
import edu.umass.cs.iesl.watr.{watrmarks => W}
import scala.collection.mutable
import utils.EnrichNumerics._
import TypeTags._
import TextReflowF._

import databasics._


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
    prKey : Int@@LabelID,
    key   : String
  )

  case class LabelingWidget(
    prKey   : Int@@LabelerID,
    widget  : String
  )

  case class LabelingTask(
    prKey     : Int@@LabelingTaskID,
    taskName  : String,
    taskState : String,
    assignee  : String
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
        docIdPageNumKey.get((docId, pageNum))
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
        val up = curr.copy(bleft=bl, btop=bt , bwidth=bw , bheight=bh)
        update(pageId, up)
        up
      }

      def getGeometry(pageId: Int@@PageID): Option[G.LTBounds] = {
        option(pageId).map({page =>
          val (l, t, w, h) = (page.bleft, page.btop, page.bwidth, page.bheight)
          val (bl, bt, bw, bh) = (itod(l), itod(t), itod(w), itod(h))
          G.LTBounds(bl, bt, bw, bh)
        })
      }
    }

    object zones extends DBRelation[ZoneID, Model.Zone] with EdgeTableOneToMany[DocumentID, ZoneID]{
      val documentFK = mutable.HashMap[Int@@DocumentID, Int@@ZoneID]()


      def forDocument(docId: Int@@DocumentID): Seq[Int@@ZoneID] = {
        getEdges(docId)
      }

      def addTargetRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit = {
        zoneToTargetRegion.addEdge(zoneId, regionId)
      }

      def getTargetRegions(zoneId: Int@@ZoneID): Seq[Model.TargetRegion] = {
        zoneToTargetRegion
          .getEdges(zoneId)
          .map(targetregions.unique(_))
      }

      def addLabel(zoneId: Int@@ZoneID, labelKey: String): Unit = {
        zoneToLabel.addEdge(zoneId, labels.ensureLabel(labelKey))
      }

      def getLabels(zoneId: Int@@ZoneID): Seq[Model.Label] = {
        zoneToLabel
          .getEdges(zoneId)
          .map(labels.unique(_))
      }

      def add(docId: Int@@DocumentID): Model.Zone = {
        val rec = Model.Zone(nextId(), docId)
        insert(rec.prKey, rec)
        rec
      }
    }

    object labels extends DBRelation[LabelID, Model.Label]{
      val forKey = mutable.HashMap[String, Int@@LabelID]()

      def ensureLabel(key: String): Int@@LabelID = {
        forKey.get(key).getOrElse({
          val rec = Model.Label(nextId(), key)
          insert(rec.prKey, rec)
          rec.prKey
        })
      }
    }

    object zoneToLabel extends EdgeTableOneToMany[ZoneID, LabelID]
    object zoneToTargetRegion extends EdgeTableOneToMany[ZoneID, RegionID]

    object targetregions extends DBRelation[RegionID, Model.TargetRegion] with EdgeTableOneToMany[ZoneID, RegionID] {

      def forZone(zoneId: Int@@ZoneID): Seq[Int@@RegionID] = {
        getEdges(zoneId)
      }

      def add(pageId: Int@@PageID, geom: G.LTBounds): Model.TargetRegion = {
        val G.LTBounds(l, t, w, h) = geom
        val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
        val rec = Model.TargetRegion(nextId(), pageId, bl, bt, bw, bh, "")
        insert(rec.prKey, rec)
        rec
      }
    }

    object labelers extends DBRelation[LabelerID, Model.LabelingWidget] {

    }
    object labelingTasks extends DBRelation[LabelingTaskID, Model.LabelingTask] {

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
    pages.all().map(_.prKey)
  }

  def getPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
    pages.forDocAndPage(docId, pageNum)
  }

  def getPageGeometry(pageId: Int@@PageID): Option[G.LTBounds] = {
    pages.getGeometry(pageId)
  }

  def updatePageGeometry(pageId: Int@@PageID, pageBounds: G.LTBounds): Unit = {
    pages.updateGeometry(pageId, pageBounds)
  }

  def updatePageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
    sys.error("unsupported operation")
  }

  def addTargetRegion(pageId: Int@@PageID, bbox: G.LTBounds): Int@@RegionID = {
    val model = targetregions.add(pageId, bbox)
    model.prKey
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
    sys.error("unsupported operation")
  }

  def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID] = {
    targetregions.all().map(_.prKey)
  }

  def createZone(docId: Int@@DocumentID): Int@@ZoneID = {
    zones.add(docId).prKey
  }

  def getZone(zoneId: Int@@ZoneID): G.Zone = {
    val zs = for {
      mzone   <- zones.option(zoneId)
      doc   <- documents.option(mzone.document)
    } yield {
      val labels = zones
        .getLabels(mzone.prKey)
        .map(m => W.Label("", m.key, None, m.prKey))

      val targetRegions = zones
        .getTargetRegions(mzone.prKey)
        .map(tr => getTargetRegion(tr.prKey))

      G.Zone(zoneId, targetRegions, labels)
    }

    zs.getOrElse(sys.error(s"getZone(${zoneId}) error"))
  }

  def setZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[G.TargetRegion]): Unit = {
    targetRegions
      .foreach(tr => zones.addTargetRegion(zoneId, tr.id))
  }

  def addZoneLabel(zoneId: Int@@ZoneID, label: W.Label): Unit = {
    zones.addLabel(zoneId, label.fqn)
  }

  def getZonesForDocument(docId: Int@@DocumentID): Seq[Int@@ZoneID] = {
    zones.forDocument(docId)
  }

  def getZonesForTargetRegion(regionId: Int@@RegionID): Seq[Int@@ZoneID] = {
    // zones.forT
    // targetregions.forZone()
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



}
