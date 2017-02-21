package edu.umass.cs.iesl.watr
package corpora


import edu.umass.cs.iesl.watr.{geometry => G}
import edu.umass.cs.iesl.watr.{watrmarks => W}
import scala.collection.mutable
import TypeTags._
import textreflow._
import TextReflowF._
import watrmarks.Label
import geometry._

object Model {

  case class Document(
    prKey   : Int@@DocumentID,
    stableId: String@@DocumentID
  )

  case class Page(
    prKey      : Int@@PageID,
    document   : Int@@DocumentID,
    pagenum    : Int@@PageNum,
    imageclip  : Option[Int@@ImageID],
    bounds     : G.LTBounds
  )

  case class TargetRegion(
    prKey      : Int@@RegionID,
    page       : Int@@PageID,
    imageclip  : Option[Int@@ImageID],
    bounds     : G.LTBounds,
    uri        : String
  )

  case class ImageClip(
    prKey    : Int@@ImageID,
    image    : Array[Byte]
  )

  case class Zone(
    prKey    : Int@@ZoneID,
    document : Int@@DocumentID
  )

  case class TextReflow(
    prKey    : Int@@TextReflowID,
    reflow   : String,
    zone     : Int@@ZoneID
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


class MemDocstore extends DocumentCorpus {
  object tables  {

    object documents extends DBRelation[DocumentID, Model.Document] {
      val stableIds = mutable.HashMap[String@@DocumentID, Int@@DocumentID]()

      def forStableId(stableId: String@@DocumentID): Option[Int@@DocumentID] = {
        stableIds.get(stableId)
      }

      def add(stableId: String@@DocumentID): Model.Document= {
        val rec = Model.Document(nextId(), stableId)
        insert(rec.prKey, rec)
        stableIds.put(stableId, rec.prKey)
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
        val rec = Model.Page(nextId(), docId, pageNum, None, G.LTBounds(0, 0, 0, 0))
        insert(rec.prKey, rec)
        documentFKey.put(docId, rec.prKey)
        docIdPageNumKey.put((docId, pageNum), rec.prKey)
        rec
      }

      def setGeometry(pageId: Int@@PageID, bbox:G.LTBounds): Model.Page = {
        val curr = unique(pageId)
        val up = curr.copy(bounds=bbox)
        update(pageId, up)
        up
      }

      def getGeometry(pageId: Int@@PageID): G.LTBounds = {
        unique(pageId).bounds
      }
    }

    object zones extends DBRelation[ZoneID, Model.Zone] {
      val documentFK = mutable.HashMap[Int@@DocumentID, Int@@ZoneID]()

      object toTargetRegion extends EdgeTableOneToMany[ZoneID, RegionID]
      object forDocument extends EdgeTableOneToMany[DocumentID, ZoneID]
      object zoneToLabel extends EdgeTableOneToMany[ZoneID, LabelID]

      // object documentToZone extends EdgeTableOneToMany[DocumentID, ZoneID]


      def addTargetRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit = {
        toTargetRegion.addEdge(zoneId, regionId)
      }

      def getTargetRegions(zoneId: Int@@ZoneID): Seq[Model.TargetRegion] = {
        toTargetRegion
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
        forDocument.addEdge(docId, rec.prKey)
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

    object targetRegionImages extends EdgeTableOneToOne[RegionID, ImageID]

    object pageImages extends EdgeTableOneToOne[PageID, ImageID]

    object imageclips extends DBRelation[ImageID, Model.ImageClip]

    object targetregions extends DBRelation[RegionID, Model.TargetRegion]  {

      object forZone extends EdgeTableOneToMany[RegionID, ZoneID]
      object forPage extends EdgeTableOneToMany[PageID, RegionID]

      def add(pageId: Int@@PageID, bbox: G.LTBounds): Model.TargetRegion = {
        val rec = Model.TargetRegion(nextId(), pageId, None, bbox, "")
        insert(rec.prKey, rec)
        forPage.addEdge(pageId, rec.prKey)
        rec
      }
    }

    object textreflows extends DBRelation[TextReflowID, Model.TextReflow] {
      object forZone extends EdgeTableOneToOne[ZoneID, TextReflowID]


      def add(zoneId: Int@@ZoneID, t: TextReflow): Model.TextReflow = {
        import TextReflowJsonCodecs._
        import play.api.libs.json
        val asJson = t.toJson()
        val asStr = json.Json.stringify(asJson)
        val rec = Model.TextReflow(nextId(), asStr, zoneId)
        this.insert(rec.prKey, rec)
        rec
      }

    }

    object labelers extends DBRelation[LabelerID, Model.LabelingWidget] {

    }
    object labelingTasks extends DBRelation[LabelingTaskID, Model.LabelingTask] {

    }

    object charatoms extends DBRelation[CharID, CharAtom] {
      object forPage extends EdgeTableOneToMany[PageID, CharID]

      def add(pageId: Int@@PageID, charAtom: CharAtom): Unit = {
        insert(charAtom.id, charAtom)
        forPage.addEdge(pageId, charAtom.id)
      }
    }
  }



  import tables._

  def getDocuments(): Seq[String@@DocumentID] = {
    documents.all().map(_.stableId)
  }

  def addDocument(stableId: String@@DocumentID): Int@@DocumentID = {
    val prKey = documents.add(stableId).prKey
    prKey
  }
  def getDocument(stableId: String@@DocumentID): Option[Int@@DocumentID] = {
    documents
      .forStableId(stableId)
      .map(stableId => documents.unique(stableId).prKey)
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

  def getPageGeometry(pageId: Int@@PageID): G.LTBounds = {
    pages.getGeometry(pageId)
  }

  def setPageGeometry(pageId: Int@@PageID, pageBounds: G.LTBounds): Unit = {
    pages.setGeometry(pageId, pageBounds)
  }

  def setPageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
    val rec = imageclips.create(Model.ImageClip(_, bytes))
    pageImages.addEdge(pageId, rec.prKey)
  }

  def getPageImage(pageId: Int@@PageID): Option[Array[Byte]] = {
    pageImages
      .getRhs(pageId)
      .map(imageclips.unique(_).image)
  }

  def addCharAtom(pageId: Int@@PageID, charAtom: CharAtom): Unit = {
    charatoms.add(pageId, charAtom)
  }

  def getCharAtoms(pageId: Int@@PageID): Seq[CharAtom] = {
    charatoms.forPage.getEdges(pageId)
      .map(charatoms.unique(_))
  }

  def addTargetRegion(pageId: Int@@PageID, bbox: G.LTBounds): Int@@RegionID = {
    val model = targetregions.add(pageId, bbox)
    model.prKey
  }


  def getTargetRegion(regionId: Int@@RegionID): G.TargetRegion = {
    // println(s"getTargetRegion: ${regionId}, ${modelPage} ${mDocument}")
    val model = targetregions.unique(regionId)
    val modelPage = pages.unique(model.page)
    val mDocument = documents.unique(modelPage.document)
    G.TargetRegion(model.prKey, mDocument.stableId, modelPage.pagenum, model.bounds)
  }

  def setTargetRegionImage(regionId: Int@@RegionID, bytes: Array[Byte]): Unit = {
    val rec = imageclips.create(Model.ImageClip(_, bytes))
    targetRegionImages.addEdge(regionId, rec.prKey)
  }

  def getTargetRegionImage(regionId: Int@@RegionID): Option[Array[Byte]] = {
    targetRegionImages
      .getRhs(regionId)
      .map(imageclips.unique(_).image)
  }

  def deleteTargetRegionImage(regionId: Int@@RegionID): Unit = {
    ???
  }

  def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID] = {
    targetregions.forPage.getEdges(pageId)
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
        .map(m =>
            W.Labels.fromString(m.key).copy(id=m.prKey)
        )

      val targetRegions = zones
        .getTargetRegions(mzone.prKey)
        .map(tr => getTargetRegion(tr.prKey))

      G.Zone(zoneId, targetRegions, labels)
    }

    zs.getOrElse(sys.error(s"getZone(${zoneId}) error"))
  }

  def setZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[G.TargetRegion]): Unit = {
    targetRegions
      .foreach { tr =>
        zones.addTargetRegion(zoneId, tr.id)
        targetregions.forZone.addEdge(tr.id, zoneId)
      }
  }

  def addZoneLabel(zoneId: Int@@ZoneID, label: W.Label): Unit = {
    zones.addLabel(zoneId, label.fqn)
  }

  def getZonesForDocument(docId: Int@@DocumentID, label: Option[Label]=None): Seq[Int@@ZoneID] = {
    zones.forDocument.getEdges(docId)
  }

  def getZoneForTargetRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID] = {
    targetregions.forZone.getEdgeOption(regionId)
  }


  def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow] = {
    textreflows.forZone
      .getRhs(zoneId)
      .map({reflowId =>
        import play.api.libs.json
        TextReflowJsonCodecs.jsonToTextReflow(
          json.Json.parse(textreflows.unique(reflowId).reflow)
        )
      })
  }

  def setTextReflowForZone(zoneId: Int@@ZoneID, textReflow: TextReflow): Unit = {
    val model = textreflows.add(zoneId, textReflow)
    textreflows.forZone.addEdge(zoneId, model.prKey)
  }

  def deleteZone(zoneId: Int@@ZoneID): Unit = {
    ???
  }



}
