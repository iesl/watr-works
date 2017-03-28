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
import geometry.zones._


class MemDocstore extends DocumentCorpus {

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
        val rec = Rel.Page(nextId(), docId, pageNum, None, G.LTBounds(0, 0, 0, 0))
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

    object zones extends DBRelation[ZoneID, ZoneTree] {

      // object toTargetRegion extends EdgeTableOneToMany[ZoneID, RegionID]
      object forDocument extends EdgeTableOneToMany[DocumentID, ZoneID]
      object zoneToLabel extends EdgeTableOneToMany[ZoneID, LabelID]
      object regionToZone extends EdgeTableOneToMany[RegionID, ZoneID]

      val ZT = ZoneTrees

      def createZoneTree(geoRegion: GeometricRegion): ZoneTree = {
        // ensure geoRegion is in database
        val _ = targetregions.ensure(geoRegion)

        val zoneId = nextId()
        val rec = ZT.ref(zoneId, ZT.leaf(geoRegion))
        insert(zoneId, rec)
        rec
      }

      def createZoneTree(zoneIds: Seq[Int@@ZoneID]): ZoneTree = {
        val zoneId = nextId()

        val rec = ZT.ref(zoneId,
          ZT.node(zoneIds.map(unique(_)))
        )
        insert(zoneId, rec)
        rec
      }

      def addZoneTreeLabel(zoneId: Int@@ZoneID, label: Label): ZoneTree = {
        val rec = ZT.role(label, unique(zoneId))
        val labelId = labels.ensureLabel(label.fqn)
        zoneToLabel.addEdge(zoneId, labelId)
        update(zoneId,rec)
        rec
      }

      def deleteZoneTree(zoneId: Int@@ZoneID): Unit = {
        delete(zoneId)
      }

      def getZoneTreeLabelsForDocument(docId: Int@@DocumentID): Seq[Label] = {
        for {
          zoneId <- forDocument.getEdges(docId)
          labelId <- zoneToLabel.getEdges(zoneId)
        } yield {
          labels.getLabel(labelId)
        }
      }

      def getZoneTreesForDocument(docId: Int@@DocumentID, label: Label): Seq[ZoneTree] = {
        for {
          zoneId <- forDocument.getEdges(docId)
          labelId <- zoneToLabel.getEdges(zoneId)
          zlabel = labels.getLabel(labelId)
          if zlabel == label
        } yield {
          unique(zoneId)
        }
      }

      def getZoneTreesForRegion(geoRegion: GeometricRegion, label: Label): Option[ZoneTree] = {
        val region = targetregions.ensure(geoRegion)
        zeroOrOne {
          for {
            zoneId <- regionToZone.getEdges(region.prKey)
            labelId <- zoneToLabel.getEdges(zoneId)
            if labels.getLabel(labelId) == label
          } yield { unique(zoneId) }
        }
      }

    }

    object labels extends DBRelation[LabelID, Rel.Label]{
      val forKey = mutable.HashMap[String, Int@@LabelID]()

      def getLabel(labelId: Int@@LabelID): W.Label = {
        val m = unique(labelId)
        W.Labels.fromString(m.key).copy(id=m.prKey)
      }

      def ensureLabel(key: String): Int@@LabelID = {
        forKey.get(key).getOrElse({
          val rec = Rel.Label(nextId(), key)
          insert(rec.prKey, rec)
          rec.prKey
        })
      }
    }

    object targetRegionImages extends EdgeTableOneToOne[RegionID, ImageID]

    object pageImages extends EdgeTableOneToOne[PageID, ImageID]

    object imageclips extends DBRelation[ImageID, Rel.ImageClip]

    object targetregions extends DBRelation[RegionID, Rel.TargetRegion]  {

      object forZone extends EdgeTableOneToMany[RegionID, ZoneID]
      object forPage extends EdgeTableOneToMany[PageID, RegionID]

      def ensure(geoRegion: GeometricRegion): Rel.TargetRegion = geoRegion match {
        case r: PageRegion   => r.regionId.map(unique(_)).getOrElse(sys.error(""))
        case r: TargetRegion => unique(r.id)
      }

      def add(pageId: Int@@PageID, bbox: G.LTBounds): Rel.TargetRegion = {
        // FIXME: correct rank
        val rec = Rel.TargetRegion(nextId(), pageId, rank=0, None, bbox)
        insert(rec.prKey, rec)
        forPage.addEdge(pageId, rec.prKey)
        rec
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

    object labelers extends DBRelation[LabelerID, Rel.LabelingWidget] {

    }
    object labelingTasks extends DBRelation[LabelingTaskID, Rel.LabelingTask] {

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
    val model = targetregions.add(pageId, bbox)
    model.prKey
  }


  def getTargetRegion(regionId: Int@@RegionID): G.TargetRegion = {
    val model = targetregions.unique(regionId)
    val modelPage = pages.unique(model.page)
    val mDocument = documents.unique(modelPage.document)
    G.TargetRegion(model.prKey, mDocument.stableId, modelPage.pagenum, model.bounds)
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

  def getZoneForRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID] = {
    targetregions.forZone.getEdgeOption(regionId)
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

  def deleteZone(zoneId: Int@@ZoneID): Unit = {
    ???
  }


  def createZoneTree(geoRegion: GeometricRegion): ZoneTree = {
    zonetrees.createZoneTree(geoRegion)
  }

  def createZoneTree(zoneIds: Seq[Int@@ZoneID]): ZoneTree = {
    zonetrees.createZoneTree(zoneIds)
  }

  def addZoneTreeLabel(zoneId: Int@@ZoneID, label: Label): ZoneTree = {
    zonetrees.addZoneTreeLabel(zoneId, label)
  }
  def deleteZoneTree(zoneId: Int@@ZoneID): Unit = {
    zonetrees.delete(zoneId)
  }
  def getZoneTree(zoneId: Int@@ZoneID): ZoneTree = {
    zonetrees.unique(zoneId)
  }

  def getZoneTreeLabelsForDocument(docId: Int@@DocumentID): Seq[Label] = {
    zonetrees.getZoneTreeLabelsForDocument(docId)
  }
  def getZoneTreesForDocument(docId: Int@@DocumentID, label: Label): Seq[ZoneTree] = {
    zonetrees.getZoneTreesForDocument(docId, label)
  }

  def getZoneTreesForRegion(geoRegion: GeometricRegion, label: Label): ZoneTree = {
    ???
  }
  def getModelTextReflowForZoneTree(zoneId: Int@@ZoneID): Option[Rel.TextReflow] = {
    ???
  }
  def getTextReflowForZoneTree(zoneId: Int@@ZoneID): Option[TextReflow] = {
    ???
  }
  def setTextReflowForZoneTree(zoneId: Int@@ZoneID, textReflow: TextReflow): Unit = {
    ???
  }



}

    // object zones extends DBRelation[ZoneID, Rel.Zone] {
    //   // val documentFK = mutable.HashMap[Int@@DocumentID, Int@@ZoneID]()

    //   object toTargetRegion extends EdgeTableOneToMany[ZoneID, RegionID]
    //   object forDocument extends EdgeTableOneToMany[DocumentID, ZoneID]
    //   object zoneToLabel extends EdgeTableOneToMany[ZoneID, LabelID]

    //   // object documentToZone extends EdgeTableOneToMany[DocumentID, ZoneID]


    //   def addTargetRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): Unit = {
    //     toTargetRegion.addEdge(zoneId, regionId)
    //   }

    //   def getTargetRegions(zoneId: Int@@ZoneID): Seq[Rel.TargetRegion] = {
    //     toTargetRegion
    //       .getEdges(zoneId)
    //       .map(targetregions.unique(_))
    //   }

    //   def addLabel(zoneId: Int@@ZoneID, labelKey: String): Unit = {
    //     zoneToLabel.addEdge(zoneId, labels.ensureLabel(labelKey))
    //   }

    //   def getLabels(zoneId: Int@@ZoneID): Seq[Rel.Label] = {
    //     zoneToLabel
    //       .getEdges(zoneId)
    //       .map(labels.unique(_))
    //   }

    //   def add(docId: Int@@DocumentID): Rel.Zone = {
    //     // FIXME: correct rank
    //     val rec = Rel.Zone(nextId(), docId, rank=0)
    //     insert(rec.prKey, rec)
    //     forDocument.addEdge(docId, rec.prKey)
    //     rec
