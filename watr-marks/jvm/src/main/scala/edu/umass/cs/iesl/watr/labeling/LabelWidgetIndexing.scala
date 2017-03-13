package edu.umass.cs.iesl.watr
package labeling

import scala.collection.mutable

import textreflow.data._
import geometry._
import geometry.syntax._
import LabelWidgetF._
import corpora._
import rindex._
import watrmarks._
// import utils.Debugging

// Provide a caching wrapper around TextReflow + precomputed page bbox
// Only valid for TextReflow that occupy a single Bbox (e.g., VisualLine)
case class IndexableTextReflow(
  id: Int@@TextReflowID,
  textReflow: TextReflow,
  pageRegion: PageRegion
)

case class QueryHit(
  positioned: WidgetPositioning,
  pageId: Int@@PageID,
  pageSpaceBounds: LTBounds,
  iTextReflows: Seq[IndexableTextReflow]
)

object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object TextReflowIndexable extends SpatialIndexable[IndexableTextReflow] {
    def id(t: IndexableTextReflow): Int = t.id.unwrap
    def ltBounds(t: IndexableTextReflow): LTBounds = t.pageRegion.bbox
  }

  implicit object LabelWidgetIndexable extends SpatialIndexable[WidgetPositioning] {
    def id(t: WidgetPositioning): Int = t.id.unwrap
    def ltBounds(t: WidgetPositioning): LTBounds = t.widgetBounds
  }


  import textreflow.TextReflowJsonCodecs._

  def create(docStore0: DocumentCorpus, lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[WidgetPositioning]()

    val layout0 = layoutWidgetPositions(lwidget)

    layout0.positioning.foreach({pos =>
      println(s"adding pos widget ${pos.widget} @ ${pos.widgetBounds}")
      lwIndex.add(pos)
    })

    val targetPageRIndexes = mutable.HashMap[Int@@PageID, SpatialIndex[IndexableTextReflow]]()

    layout0.positioning.foreach({pos => pos.widget match {

      case l @ TargetOverlay(under, overs) =>
        val pageId = under.pageId
        // under.regionId
        // under.bbox
        if (!targetPageRIndexes.contains(pageId)) {
          val pageIndex = SpatialIndex.createFor[IndexableTextReflow]()
          targetPageRIndexes.put(pageId, pageIndex)

          for {
            vline <- docStore0.getPageVisualLines(pageId)
            reflow <- docStore0.getModelTextReflowForZone(vline.id)
          } {
            val textReflow = jsonStrToTextReflow(reflow.reflow)
            val indexable = IndexableTextReflow(
              reflow.prKey,
              textReflow,
              PageRegion(
                pageId,
                textReflow.targetRegion.bbox
              )
            )
            pageIndex.add(indexable)
          }
        }

      case _ =>


    }})


    new LabelWidgetIndex {
      def docStore: DocumentCorpus = docStore0
      def layout: WidgetLayout = layout0
      def index: SpatialIndex[WidgetPositioning] = lwIndex
      def pageIndexes: Map[Int@@PageID, SpatialIndex[IndexableTextReflow]] = targetPageRIndexes.toMap
    }
  }
}


trait LabelWidgetIndex {

  def docStore: DocumentCorpus
  def layout: WidgetLayout
  def index: SpatialIndex[WidgetPositioning]
  def pageIndexes: Map[Int@@PageID, SpatialIndex[IndexableTextReflow]]


  def select(queryBounds: LTBounds): Seq[QueryHit] = {
    val hits = index.queryForIntersects(queryBounds)
      .map { pos => pos.widget match {
        case TargetOverlay(under, over) =>
          println(s"select hit ${under}")
          val maybeIntersect = pos.widgetBounds.intersection(queryBounds)
          maybeIntersect.map { ibbox =>
            println(s"   intersected TargetOverlay @ ${ibbox}")
            val pageSpaceBounds = ibbox.translate(pos.translation)
            println(s"   translates to ${pageSpaceBounds} as page query")
            val pageIndex = pageIndexes(under.pageId)
            val pageHits = pageIndex.queryForIntersects(pageSpaceBounds)
            println(s"""   page query hit: ${pageHits.map(_.textReflow.toText).mkString("\n      ")}""")
            QueryHit(pos, under.pageId, pageSpaceBounds, pageHits)
          }

        case _ => None
      }}

    // debugPrint(Some(queryBounds))
    hits.flatten
  }
  // Seq[(DoubleInterval, Label, Add/Remove)]


  def labelConstrained(constraint: Constraint, queryHits: Seq[QueryHit], label: Label): GeometricGroup = {
    var change = GeometricGroup(List())

    val pageRegionsToBeLabeled = for {
      qhit <- queryHits
    } yield constraint match {
      case ByLine =>

        val regions = qhit.iTextReflows.map(_.pageRegion)

        change = GeometricGroup(
          regions.map(_.bbox.translate(-qhit.positioned.translation)).toList
        )

        regions

      case ByRegion =>
        val regions = Seq(PageRegion(qhit.pageId, qhit.pageSpaceBounds, None))

        change = GeometricGroup(
          regions.map(_.bbox.translate(-qhit.positioned.translation)).toList
        )

        regions

      case ByChar =>
        val regionss = for {
          iReflow <- qhit.iTextReflows
        } yield {

          iReflow.textReflow
            .clipToBoundingRegion(qhit.pageSpaceBounds)
            .map { case (clipped, _) =>
              val bbox = clipped.targetRegion().bbox
              PageRegion(qhit.pageId, bbox, None)
            }

        }
        val regions = regionss.flatten
        // println(s"ByChar labeling")
        // println(s"    widget translation: ${qhit.positioned.translation}")
        val debugR = regions.map(_.bbox).mkString("\n  ", "\n  ", "\n")
        val debugTR = regions.map(_.bbox.translate(-qhit.positioned.translation)).toList
        // println("orig")
        // println(debugR)
        // println("translated")
        // println(debugTR)

        change = GeometricGroup(
          regions.map(_.bbox.translate(-qhit.positioned.translation)).toList
        )
        regions
    }

    if (pageRegionsToBeLabeled.isEmpty) Seq() else {
      // Ensure pageRegions are all cataloged in database
      val targetRegions = for {
        pageRegion <- pageRegionsToBeLabeled.flatten
      } yield {
        val regionId = docStore.addTargetRegion(pageRegion.pageId, pageRegion.bbox)
        docStore.getTargetRegion(regionId)
      }

      val docId = docStore.getDocument(targetRegions.head.stableId).get

      val newZone = docStore.createZone(docId)

      docStore.setZoneTargetRegions(newZone, targetRegions)

      docStore.addZoneLabel(newZone, label)

      Some(docStore.getZone(newZone))
    }

    change
  }

  def addLabel(bbox: LTBounds, constraint: Constraint, label: Label): GeometricGroup = {
    val queryHits = select(bbox)
    labelConstrained(constraint, queryHits, label)
  }

  // def extendLabel(bbox: LTBounds, constraint: Constraint, label: Label, zone: Int@@ZoneID)
  // def removeLabel(label: Label, zone: Int@@ZoneID)

  def debugPrint(query: Option[LTBounds] = None): Unit = {
    val w: Int = (layout.layoutBounds.width).intValue()+1
    val h: Int = (layout.layoutBounds.height).intValue()+1

    val gridPaper = GridPaper.create(w, h)

    layout.positioning.foreach { pos =>
      val gridbox = GridPaper.ltb2box(pos.widgetBounds)

      pos.widget match {
        case TargetOverlay(under, over) =>
          val regionId = under.regionId.get.unwrap
          val fill = (regionId + '0'.toInt).toChar

          gridPaper.fillFg(fill, gridbox)

        case _ =>
      }
    }
    layout.positioning.foreach { pos =>
      val gridbox = GridPaper.ltb2box(pos.widgetBounds)
      pos.widget match {
        case Col(as) => gridPaper.borderLeftRight(gridbox)
        case Row(as) => gridPaper.borderTopBottom(gridbox)
        case _ =>
      }
    }
    query foreach {q =>
      gridPaper.shadeBackground(GridPaper.ltb2box(q))
    }

    val pp = gridPaper.asString()
    println(pp)
  }
}
