package edu.umass.cs.iesl.watr
package labeling

import scala.collection.mutable

import textreflow.data._
import geometry._
import geometry.syntax._
import LabelWidgetF._
import corpora._
import rindex._


// Provide a caching wrapper around TextReflow + precomputed page bbox
// Only valid for TextReflow that occupy a single Bbox (e.g., VisualLine)
case class IndexableTextReflow(
  id: Int@@TextReflowID,
  textReflow: TextReflow,
  pageRegion: PageRegion
)

case class QueryHit(
  positioned: WidgetPositioning,
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

  def applyConstraint2(constraint: Constraint, queryHits: Seq[QueryHit]): Seq[PageRegion] = {
    queryHits.map { qhit =>
      constraint match {
        case Constraint.ByLine =>

        case Constraint.ByChar =>
          qhit.iTextReflows.map { iReflow =>
            val clippedReflows = iReflow.textReflow
              .clipToBoundingRegion(qhit.pageSpaceBounds)
              .map { case (clipped, interval)  => clipped }

            if (clippedReflows.length != 1) {
              println(s"Error: applyConstraint: clippedReflows produced ${clippedReflows.length} reflows (1 required)")
            }

            val clipped = clippedReflows.head
            val tr = clipped.targetRegion()
            val regionId = tr.id
            val bbox = tr.bbox
            val pageRegion = PageRegion(iReflow.pageRegion.pageId, bbox, Some(regionId))

            // val reflow = clippedReflows.map { case (cr, interval)  => cr }
            // iReflow.copy(
            //   textReflow = clipped,
            //   pageRegion = pageRegion
            // )
            pageRegion
          }

        case Constraint.ByRegion =>
          qhit.pageSpaceBounds
      }
    }

    ???
  }
  def applyConstraint(constraint: Constraint, queryHits: Seq[QueryHit]): Seq[PageRegion] = {
    queryHits.map { qhit =>
      qhit.iTextReflows.map { iReflow =>
        constraint match {
          case Constraint.ByLine =>
            iReflow.pageRegion

          case Constraint.ByChar =>
            val clippedReflows = iReflow.textReflow
              .clipToBoundingRegion(qhit.pageSpaceBounds)
              .map { case (clipped, interval)  => clipped }

            if (clippedReflows.length != 1) {
              println(s"Error: applyConstraint: clippedReflows produced ${clippedReflows.length} reflows (1 required)")
            }

            val clipped = clippedReflows.head
            val tr = clipped.targetRegion()
            val regionId = tr.id
            val bbox = tr.bbox
            val pageRegion = PageRegion(iReflow.pageRegion.pageId, bbox, Some(regionId))

            // val reflow = clippedReflows.map { case (cr, interval)  => cr }
            // iReflow.copy(
            //   textReflow = clipped,
            //   pageRegion = pageRegion
            // )
            pageRegion

          case Constraint.ByRegion =>
        }
      }
    }

    ???
  }

  import textreflow.TextReflowJsonCodecs._

  def create(docStore0: DocumentCorpus, lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[WidgetPositioning]()

    val layout0 = layoutWidgetPositions(lwidget)

    layout0.positioning.foreach({pos =>
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

  def select(queryBounds: LTBounds): Seq[QueryHit] = {
    val hits = index.queryForIntersects(queryBounds)
      .map { pos => pos.widget match {
        case TargetOverlay(under, over) =>
          val maybeIntersect = pos.widgetBounds.intersection(queryBounds)
          maybeIntersect.map { ibbox =>
            val pageSpaceBounds = ibbox.translate(pos.translation)
            // println(s"PageRegion ${under.regionId.get}: ${under.bbox} @ ${pos.widgetBounds} w/trans=${pos.translation}")
            // println(s"   sel: ${pageSpaceBounds}  from ${ibbox}")

            // query pageIndex using pageSpaceBounds
            val pageIndex = pageIndexes(under.pageId)
            val pageHits = pageIndex.queryForIntersects(pageSpaceBounds)
            QueryHit(pos, pageSpaceBounds, pageHits)

          }

        case _ => None
      }}

    debugPrint(Some(queryBounds))
    hits.flatten
  }
  import LabelWidgetIndex._

  def runUIRequest(r: UIRequest): UIResponse = {
    val UIRequest(uiState, gesture) = r

    gesture match {
      case SelectRegion(bbox) =>

        val queryHits = select(bbox)
        val constrainedHits = applyConstraint(uiState.selectionConstraint, queryHits)
        // Options:
        // 1. No selected lines are part of a labeled zone
        // 2. At least one selected line is part of a labeled zone

        // a. The selected lines have exactly one guessed label
        // b. The selected lines have no guessed labels, or more than one


        if (constrainedHits.nonEmpty) {
          uiState.action match {
            case Create =>
              //====== case 1,b:

              // val targetRegions = for {
              //   qhit <- constrainedHits
              //   iReflow <- qhit.iTextReflows
              // } yield {
              //   // ensure this is a db-backed target region
              //   val pageRegion = iReflow.pageRegion
              //   val regionId = docStore.addTargetRegion(pageRegion.pageId, pageRegion.bbox)
              //   docStore.getTargetRegion(regionId)
              // }


              // val docId = docStore.getDocument(targetRegions.head.stableId).get

              // val newZone = docStore.createZone(docId)
              // println(s"newZone: ${newZone}")

              // docStore.setZoneTargetRegions(newZone, targetRegions)

              // println(s"newZone targetRegions: ${targetRegions}")

              // val label = uiState.selectedLabel.get
              // docStore.addZoneLabel(newZone, label)

              // println(s"newZone label: ${label}")

            case Delete =>
          }
        }

    }

    // response: indicate(region, )

    UIResponse(List())
  }

}
