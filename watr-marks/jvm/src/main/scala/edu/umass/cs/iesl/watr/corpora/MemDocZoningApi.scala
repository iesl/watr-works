package edu.umass.cs.iesl.watr
package corpora

import edu.umass.cs.iesl.watr.{geometry => G}
import edu.umass.cs.iesl.watr.{watrmarks => W}
import scala.collection.mutable
import TypeTags._
import textreflow._
import textreflow.data._
import TextReflowF._
import watrmarks.Label
import geometry._
import PageComponentImplicits._


class MemDocZoningApi extends DocumentZoningApi {

  // def workflowApi: WorkflowApi = ???
  // def userbaseApi: UserbaseApi = ???

  object tables  {

    object documents extends DBRelation[DocumentID, Rel.Document] {
      val stableIds = mutable.HashMap[String@@DocumentID, Int@@DocumentID]()

      def forStableId(stableId: String@@DocumentID): Option[Int@@DocumentID] = {
        stableIds.get(stableId)
      }

      def add(stableId: String@@DocumentID): Rel.Document= {
        val rec = Rel.Document(nextId(), stableId)
        insert(rec.prKey, rec)
        stableIds.put(stableId, rec.prKey)
        rec
      }
    }

    object pages extends DBRelation[PageID, Rel.Page] {
      val documentFKey = mutable.HashMap[Int@@DocumentID, Int@@PageID]()
      val docIdPageNumKey = mutable.HashMap[(Int@@DocumentID, Int@@PageNum), Int@@PageID]()

      def forDocAndPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
        docIdPageNumKey.get((docId, pageNum))
      }

      def add(docId: Int@@DocumentID, pageNum: Int@@PageNum): Rel.Page = {
        val rec = Rel.Page(nextId(), docId, pageNum, None, G.LTBounds.zero)
        insert(rec.prKey, rec)
        documentFKey.put(docId, rec.prKey)
        docIdPageNumKey.put((docId, pageNum), rec.prKey)
        rec
      }

      def setGeometry(pageId: Int@@PageID, bbox:G.LTBounds): Rel.Page = {
        val curr = unique(pageId)
        val up = curr.copy(bounds=bbox)
        update(pageId, up)
        up
      }

      def getGeometry(pageId: Int@@PageID): G.LTBounds = {
        unique(pageId).bounds
      }
    }


    object zones extends DBRelation[ZoneID, Rel.Zone] {

      object toTargetRegion extends EdgeTableOneToMany[ZoneID, RegionID]
      object forDocument extends EdgeTableOneToMany[DocumentID, ZoneID]
      // object zoneToLabel extends EdgeTableOneToMany[ZoneID, LabelID]
      type ZoneOrderKey = (Int@@DocumentID, Int@@LabelID)
      val zoneOrdering = mutable.HashMap[ZoneOrderKey, mutable.ArrayBuffer[Int@@ZoneID]]()

      object regionToZone extends EdgeTableOneToMany[RegionID, ZoneID]

      def getZoneLabelsForDocument(docId: Int@@DocumentID): Seq[Int@@LabelID] = {
        // forDocument.debugPrint()
        val allLabels = for {
          zoneId <- forDocument.getEdges(docId)
        } yield {
          unique(zoneId).label
        }
        allLabels.toSet.toList
      }

      def getZonesForDocument(docId: Int@@DocumentID, labelId: Int@@LabelID): Seq[Int@@ZoneID] = {
        for {
          zoneId <- forDocument.getEdges(docId)
          zone    = unique(zoneId)
          zlabel  = labels.getLabel(zone.label)
          label  = labels.getLabel(labelId)
          if zlabel == label
        } yield { zoneId }
      }

      def getZoneForRegion(regionId: Int@@RegionID, label: W.Label): Option[Rel.Zone] = {

        zeroOrOne {
          for {
            zoneId <- regionToZone.getEdges(regionId)
            // labelId <- zoneToLabel.getEdges(zoneId)
            zone = unique(zoneId)
            if labels.getLabel(zone.label) == label
          } yield { unique(zoneId) }
        }
      }


      def removeTargetRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Option[Int@@ZoneID] = {
        toTargetRegion.removeEdge(zoneId, regionId)
        toTargetRegion.getEdges(zoneId).headOption match {
          case Some(_) => Some(zoneId)
          case None =>
            // zone is effectively deleted by removing the last region
            forDocument.removeEdgesTo(zoneId)
            ??? // delete zoneId
            None
        }

      }
      def addTargetRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit = {
        toTargetRegion.addEdge(zoneId, regionId)
        regionToZone.addEdge(regionId, zoneId)
      }

      def getTargetRegions(zoneId: Int@@ZoneID): Seq[Rel.TargetRegion] = {
        toTargetRegion
          .getEdges(zoneId)
          .map(targetregions.unique(_))
      }


      def createZone(regionId: Int@@RegionID, labelId: Int@@LabelID): Rel.Zone = {
        val zoneId = nextId()

        // link zone -> document
        val pageId = targetregions.unique(regionId).page
        val docId = pages.unique(pageId).document
        forDocument.addEdge(docId, zoneId)

        // add label
        // val labelId = labels.ensureLabel(label.fqn)

        // ensure ordering
        val orderedZones = zoneOrdering.getOrElseUpdate((docId, labelId), mutable.ArrayBuffer[Int@@ZoneID]())
        orderedZones += zoneId
        val rank = orderedZones.length


        // link target region
        addTargetRegion(zoneId, regionId)

        forDocument

        val rec = Rel.Zone(zoneId, docId, labelId, rank)
        insert(zoneId, rec)
        rec
      }

    }

    object labels extends DBRelation[LabelID, Rel.Label]{
      val forKey = mutable.HashMap[String, Int@@LabelID]()

      def getLabel(labelId: Int@@LabelID): W.Label = {
        val m = unique(labelId)
        W.Labels.fromString(m.key).copy(id=m.prKey)
      }

      def ensureLabel(key: String): Int@@LabelID = {
        forKey.getOrElseUpdate(key, {
          val rec = Rel.Label(nextId(), key)
          insert(rec.prKey, rec)
          forKey.put(key, rec.prKey)
          rec.prKey
        })
      }
    }

    object targetRegionImages extends EdgeTableOneToOne[RegionID, ImageID]

    object pageImages extends EdgeTableOneToOne[PageID, ImageID]

    object imageclips extends DBRelation[ImageID, Rel.ImageClip]

    object targetregions extends DBRelation[RegionID, Rel.TargetRegion]  {
      // object forZone extends EdgeTableOneToMany[RegionID, ZoneID]

      object forPage extends EdgeTableOneToMany[PageID, RegionID]
      val forUriKey = mutable.HashMap[String, Int@@RegionID]()

      def ensure(pageId: Int@@PageID, bbox: G.LTBounds): Int@@RegionID = {
        val page = pages.unique(pageId)
        val pageNum = page.pagenum
        val doc = documents.unique(page.document)
        val uriKey = createTargetRegionUri(doc.stableId, pageNum, bbox)

        // val uriKey = bbox.uriString
        forUriKey.getOrElseUpdate(uriKey,{
          // FIXME: correct rank
          val rec = Rel.TargetRegion(nextId(), pageId, rank=0, None, bbox)
          insert(rec.prKey, rec)
          forUriKey.put(uriKey, rec.prKey)
          forPage.addEdge(pageId, rec.prKey)
          rec.prKey
        })

      }
    }

    object textreflows extends DBRelation[TextReflowID, Rel.TextReflow] {
      object forZone extends EdgeTableOneToOne[ZoneID, TextReflowID]


      def add(zoneId: Int@@ZoneID, t: TextReflow): Rel.TextReflow = {
        import TextReflowJsonCodecs._
        import play.api.libs.json
        val asJson = t.toJson()
        val asText = t.toText()
        val jsstr = json.Json.stringify(asJson)
        val rec = Rel.TextReflow(nextId(), jsstr, asText, zoneId)
        this.insert(rec.prKey, rec)
        rec
      }

    }

    // object labelers extends DBRelation[LabelerID, Rel.LabelingWidget] {}
    // object labelingTasks extends DBRelation[LabelingTaskID, Rel.LabelingTask] {}

    object charatoms extends DBRelation[CharID, CharAtom] {
      object forPage extends EdgeTableOneToMany[PageID, CharID]

      def add(pageId: Int@@PageID, charAtom: CharAtom): Unit = {
        insert(charAtom.id, charAtom)
        forPage.addEdge(pageId, charAtom.id)
      }
    }
  }



  import tables._

  def getDocuments(n: Int=Int.MaxValue, skip: Int=0): Seq[String@@DocumentID] = {
    documents.all().map(_.stableId)
  }
  def getDocumentCount(): Int =  {
    documents.all().length
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

  def getPageIdentifier(pageId: Int@@PageID): RecordedPageID = {
    val p = pages.unique(pageId)
    val d = documents.unique(p.document)
    RecordedPageID(
      pageId,
      StablePageID(d.stableId, p.pagenum)
    )
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

  def getPageDef(pageId: Int@@PageID): Option[Rel.Page] = {
    pages.option(pageId)
  }

  def getPageGeometry(pageId: Int@@PageID): G.LTBounds = {
    pages.getGeometry(pageId)
  }

  def setPageGeometry(pageId: Int@@PageID, pageBounds: G.LTBounds): Unit = {
    pages.setGeometry(pageId, pageBounds)
  }

  def setPageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
    val rec = imageclips.create(Rel.ImageClip(_, bytes))
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
    targetregions.ensure(pageId, bbox)
  }


  def getTargetRegion(regionId: Int@@RegionID): G.TargetRegion = {
    val model = targetregions.unique(regionId)
    val modelPage = pages.unique(model.page)
    val mDocument = documents.unique(modelPage.document)
    val stable = G.StablePageID(mDocument.stableId, modelPage.pagenum)
    val page = G.RecordedPageID(model.page, stable)
    G.TargetRegion(model.prKey, page, model.bounds)
  }

  def setTargetRegionImage(regionId: Int@@RegionID, bytes: Array[Byte]): Unit = {
    val rec = imageclips.create(Rel.ImageClip(_, bytes))
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



  def getZoneForRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID] = {
    zones.getZoneForRegion(regionId, label).map(_.prKey)
  }

  def createZone(regionId: Int@@RegionID, role: Int@@LabelID): Int@@ZoneID = {
    zones.createZone(regionId, role).prKey
  }

  def addZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit = {
    zones.addTargetRegion(zoneId, regionId)
  }

  def removeZoneRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Option[Int@@ZoneID] = {
    zones.removeTargetRegion(zoneId, regionId)
  }

  def deleteZone(zoneId: Int@@ZoneID): Unit = {
    zones.delete(zoneId)
  }

  def getZone(zoneId: Int@@ZoneID): G.Zone = {
    val zs = for {
      mzone   <- zones.option(zoneId)
      doc     <- documents.option(mzone.document)
    } yield {
      val label = labels.getLabel(mzone.label)

      val targetRegions = zones
        .getTargetRegions(mzone.prKey)
        .map(tr => getTargetRegion(tr.prKey))

      G.Zone(zoneId, targetRegions, label)
    }
    zs.getOrElse(sys.error(s"getZone(${zoneId}) error"))
  }

  def getLabel(labelId: Int@@LabelID): Label = {
    labels.getLabel(labelId)
  }

  def getZoneLabelsForDocument(docId: Int@@DocumentID): Seq[Int@@LabelID] = {
    zones.getZoneLabelsForDocument(docId)
  }

  def getZonesForDocument(docId: Int@@DocumentID, label: Int@@LabelID): Seq[Int@@ZoneID] = {
    zones.getZonesForDocument(docId, label)
  }

  def ensureLabel(label: Label): Int@@LabelID = {
    labels.ensureLabel(label.fqn)
  }

  def getModelTextReflowForZone(zoneId: Int@@ZoneID): Option[Rel.TextReflow] = {
    textreflows.forZone
      .getRhs(zoneId)
      .flatMap(id => textreflows.option(id))
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

}
