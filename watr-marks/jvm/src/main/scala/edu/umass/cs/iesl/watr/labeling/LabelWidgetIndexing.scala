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

  def applyConstraint(constraint: Constraint, queryHits: Seq[QueryHit]): Seq[QueryHit] = {
    queryHits.map { qhit =>
      qhit.iTextReflows.map { iReflow =>
        constraint match {
          case Constraint.ByLine =>
            val clippedReflows = iReflow.textReflow.clipToBoundingRegion(qhit.pageSpaceBounds)
            clippedReflows.foreach { case (cr, interval)  =>
              val ctr = cr.targetRegion()
            }
          case Constraint.ByChar =>
            val clippedReflows = iReflow.textReflow.clipToBoundingRegion(qhit.pageSpaceBounds)
            clippedReflows.foreach { case (cr, interval)  =>
              val ctr = cr.targetRegion()
            }
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

    val cmat = mutable.ArrayBuffer
      .tabulate(h, w){ case (y, x) =>
        fansi.Color.Blue(" ")
      }

    layout.positioning.foreach { pos =>
      val LTBounds(l, t, w, h) = pos.widgetBounds
      val il = l.intValue()
      val it = t.intValue()
      val iw = w.intValue()
      val ih = h.intValue()
      // println(s"pos: ($il, $it, $iw, $ih) (${pos.widgetBounds})")
      pos.widget match {
        case TargetOverlay(under, over) =>
          val regionId = under.regionId.get.unwrap
          for {
            y <- it until (it+ih)
            x <- il until (il+iw)
          } {
            val qq = cmat(y)(x)
            cmat(y)(x) = (regionId + '0'.toInt).toChar.toString()
          }

        case _ =>
      }
    }
    layout.positioning.foreach { pos =>
      val LTBounds(l, t, w, h) = pos.widgetBounds
      val il = l.intValue()
      val it = t.intValue()
      val iw = w.intValue()
      val ih = h.intValue()
      // println(s"pos: ($il, $it, $iw, $ih) (${pos.widgetBounds})")
      pos.widget match {
        case Col(as) =>
          for {
            y <- it until (it+ih)
          } {
            val x1 = il
            val x2 = il+iw-1
            // println(s"  ($x1/$x2, $y) := col")
            val cur0 = cmat(y)(x1)
            val cur1 = cmat(y)(x2)
            val col0 = fansi.Color.Red(cur0)
            val col1 = fansi.Color.Red(cur1)

            cmat(y)(x1) = col0
            cmat(y)(x2) = col1
          }
        case Row(as) =>
          for {
            x <- il until (il+iw)
          } {
            val y1 = it
            val y2 = it+ih-1
            // println(s"  ($x, $y1/$y2) := row")
            val cur0 = cmat(y1)(x)
            val cur1 = cmat(y2)(x)
            val col0 = fansi.Color.Magenta(cur0)
            val col1 = fansi.Color.Magenta(cur1)
            cmat(y1)(x) = col0
            cmat(y2)(x) = col1
          }
        case _ =>
      }
    }
    // val LTBounds(l, t, w, h) = query
    query foreach {q =>
      val il = q.left.intValue()
      val it = q.top.intValue()
      val iw = q.width.intValue()
      val ih = q.height.intValue()
      for {
        y <- it until (it+ih)
        x <- il until (il+iw)
      } {
        val qq = cmat(y)(x)
        cmat(y)(x) = fansi.Back.True(10, 20, 200)(qq)
      }

    }

    val pp = cmat.map(_.mkString).mkString("\n")
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

    // debugPrint(Some(queryBounds))
    hits.flatten
  }

  // def querySelected(bbox: LTBounds): Seq[LabeledTarget] = {
  //   val positioned: Seq[WidgetPositioning] = index.queryForIntersects(bbox)

  //   // val selectedTargets: List[(WidgetPositioning, LabeledTarget)]
  //   val selectedTargets: List[LabeledTarget] = positioned.toList
  //     .map(p => index.getItem(p.id.unwrap))
  //     .filter(_.widget.isInstanceOf[LabeledTarget])
  //     .map(p => p.widget.asInstanceOf[LabeledTarget])

  //   selectedTargets
  // }

  def constrainedClipTargetRegions(bbox: LTBounds, constraint: Constraint, targets: Seq[TargetRegion]): Seq[TargetRegion] = { ??? }
  def queryForSelectedLines(bbox: LTBounds): Seq[Zone] = { ??? }
  def runUIRequest(r: UIRequest): UIResponse = {
    val UIRequest(uiState, gesture) = r

    gesture match {
      case SelectRegion(bbox) =>

        // val positioned: Seq[WidgetPositioning] = index.queryForIntersects(bbox)

        val selectedLines = queryForSelectedLines(bbox)
        val selectedTargetLines = selectedLines.flatMap(_.regions)

        val clippedTargets = constrainedClipTargetRegions(bbox, uiState.selectionConstraint, selectedTargetLines)

        if (selectedLines.nonEmpty) {
          uiState.action match {
            case Create =>
              uiState.selectionConstraint match {
                case Constraint.ByLine =>
                  // apply label to all lines in selected region

                case Constraint.ByChar =>
                case Constraint.ByRegion =>
                  // apply label to region w/o regard to line/chars
              }

              def mergedZone: Zone =  ???

              // docStore.getZone(docStore.mergeZones(existingZones.map(_.id)))

              // Add all target regions to merged zone
              // selectedTargets.map(tr => docStore.setZoneTargetRegions(
              //   mergedZone.id,
              //   mergedZone.regions :+ tr.target
              // ))
              // Option(mergedZone)

              // // Create a new Zone with given label
              // val stableId = selectedTargets.head.target.stableId
              // val docId = docStore
              //   .getDocument(stableId)
              //   .getOrElse(sys.error(s"onSelect() document ${stableId} not found"))

              // val targetRegions = selectedTargets.map(_.target)
              // val newZone = docStore.getZone(
              //   docStore.createZone(docId)
              // )
              // docStore.setZoneTargetRegions(newZone.id, targetRegions)
              // docStore.addZoneLabel(newZone.id, targetLabel)

            case Delete =>
          }
        }

    }



    // response: indicate(region, )

    UIResponse(List())
  }

}

// def getWidgetForTargetRegion(targetRegion: TargetRegion): WidgetPositioning = {
//   val stableId = targetRegion.stableId
//   val docId = docStore.getDocument(stableId).get
//   val pageId = docStore.getPage(docId, targetRegion.pageNum).get
//   // Map TargetRegion -> WidgetPositioning
//   layout
//     .collect({
//       case p @ WidgetPositioning(
//         LabeledTarget(bbox, label, score),
//         widgetBounds,
//         pRegionId, _, _
//       ) if targetRegion.id == targetRegion.id => p
//     }).headOption
//     .getOrElse(sys.error(s"getWidgetForTargetRegion: no entry for ${targetRegion}"))
//   ???
// }
