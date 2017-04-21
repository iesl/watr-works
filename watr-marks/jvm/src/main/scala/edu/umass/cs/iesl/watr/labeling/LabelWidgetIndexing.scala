package edu.umass.cs.iesl.watr
package labeling

import scala.collection.mutable

import textreflow.data._
import geometry._
// import geometry.zones.syntax._
import geometry.syntax._
import LabelWidgetF._
import corpora._
import rindex._
import watrmarks._
import LabelAction._

import scalaz.Free
import scalaz.~>
import scalaz.State
import shapeless._

import matryoshka._
import matryoshka.implicits._
import utils.GraphPaper
import utils.Colors

// Provide a caching wrapper around TextReflow + precomputed page bbox
// Only valid for TextReflow that occupy a single Bbox (e.g., VisualLine)
case class IndexableTextReflow(
  id: Int@@TextReflowID,
  textReflow: TextReflow,
  targetRegion: PageRegion
)

case class QueryHit(
  positioned: AbsPosWidget,
  pageId: Int@@PageID,
  pageQueryBounds: LTBounds, // The query
  iTextReflows: Seq[IndexableTextReflow]
)

object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object TextReflowIndexable extends SpatialIndexable[IndexableTextReflow] {
    def id(t: IndexableTextReflow): Int = t.id.unwrap
    def ltBounds(t: IndexableTextReflow): LTBounds = t.targetRegion.bbox
  }

  implicit object LabelWidgetIndexable extends SpatialIndexable[AbsPosWidget] {
    def id(t: AbsPosWidget): Int = t.widget.wid.unwrap
    def ltBounds(t: AbsPosWidget): LTBounds = t.strictBounds
  }


  import textreflow.TextReflowJsonCodecs._

  def create(docStore0: DocumentCorpus, lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[AbsPosWidget]()

    val layout0 = layoutWidgetPositions(lwidget)

    layout0.positioning.foreach({pos =>
      lwIndex.add(pos)
    })

    val targetPageRIndexes = mutable.HashMap[Int@@PageID, SpatialIndex[IndexableTextReflow]]()

    def addPage(targetRegion: TargetRegion): Unit= {
      val pageId = targetRegion.page.pageId
      if (!targetPageRIndexes.contains(pageId)) {
        val pageIndex = SpatialIndex.createFor[IndexableTextReflow]()
        targetPageRIndexes.put(pageId, pageIndex)

        // Put all visual lines into index
        for {
          vline <- docStore0.getPageVisualLines(pageId)
          reflow <- docStore0.getModelTextReflowForZone(vline.id)
        } {
          val textReflow = jsonStrToTextReflow(reflow.reflow)
          val indexable = IndexableTextReflow(
            reflow.prKey,
            textReflow,
            textReflow.targetRegion
          )
          pageIndex.add(indexable)
        }
      }
    }

    layout0.positioning.foreach({pos => pos.widget match {

      case l @ RegionOverlay(wid, under, overlays) =>
        addPage(under)

      case _ =>

    }})


    new LabelWidgetIndex {
      def docStore: DocumentCorpus = docStore0
      def layout: WidgetLayout = layout0
      def index: SpatialIndex[AbsPosWidget] = lwIndex
      def pageIndexes: Map[Int@@PageID, SpatialIndex[IndexableTextReflow]] = targetPageRIndexes.toMap
    }
  }
}

case class InterpState(
  uiResponse: UIResponse,
  labelWidget: LabelWidget
)
object istate {

  val uiResponseL   = lens[InterpState].uiResponse
  val uiStateL      = lens[InterpState].uiResponse.uiState
  val selectionsL   = lens[InterpState].uiResponse.uiState.selections
  val changesL      = lens[InterpState].uiResponse.changes
  val labelWidgetL  = lens[InterpState].labelWidget


  def addSelection(zoneId: Int@@ZoneID): InterpState => InterpState =
    st => selectionsL.modify(st) {
      sels => zoneId +: sels
    }
}


trait LabelWidgetIndex {

  def docStore: DocumentCorpus
  def layout: WidgetLayout
  def index: SpatialIndex[AbsPosWidget]
  def pageIndexes: Map[Int@@PageID, SpatialIndex[IndexableTextReflow]]

  def queryForPanels(queryPoint: Point): Seq[(Panel[Unit], AbsPosWidget)] = {
    val queryBox = queryPoint
      .lineTo(queryPoint.translate(1, 1))
      .bounds
    // println(s"queryForPanels: queryPoint = ${queryBox}")


    val ret = index.queryForIntersects(queryBox)
      .map ({ pos => pos.widget match {
        case p : Panel[Unit] => Option { (p, pos) }
        case _ => None
      }})
      .flatten

    // println(s"queryForPanels: found = ${ret.mkString('\n'.toString)}")
    ret
  }

  def queryPage(pos: AbsPosWidget, queryBounds: LTBounds, pageId: Int@@PageID): Option[QueryHit] = {
    pos.strictBounds
      .intersection(queryBounds)
      .map { clippedQueryBox =>
        val pageQueryBounds = clippedQueryBox.translate(pos.translation)
        val pageIndex = pageIndexes(pageId)
        val pageHits = pageIndex.queryForIntersects(pageQueryBounds)
        QueryHit(pos, pageId, pageQueryBounds, pageHits)
      }
  }

  def queryRegion(queryBounds: LTBounds): Seq[QueryHit] = {
    val hits = index
      .queryForIntersects(queryBounds)
      .map { pos => pos.widget match {

        case l @ RegionOverlay(wid, under, overlays) =>
          val pageId = under.page.pageId
          queryPage(pos, queryBounds, pageId)


        case _ => None
      }}

    hits.flatten
  }


  def applyConstraint(constraint: Constraint, queryHits: Seq[QueryHit]): Seq[QueryHit] = {
    for {
      qhit <- queryHits
    } yield constraint match {

      case ByLine   => qhit
      case ByRegion => qhit

      case ByChar =>

        val clippedReflows = for {
          iReflow <- qhit.iTextReflows
        } yield {

          val clipped = iReflow.textReflow
            .clipToBoundingRegion(qhit.pageQueryBounds)
            .map { case (clipped, range) =>
              clipped
            }

          // FIXME: This assumes that clipping the text reflow will result in a single non-empty result
          iReflow.copy(
            textReflow=clipped.head
          )

        }

        qhit.copy(iTextReflows=clippedReflows)

    }

  }

  def labelConstrained(constraint: Constraint, queryHits: Seq[QueryHit], label: Label): Unit = {

    val pageRegionsToBeLabeled = (for {
      qhit <- queryHits
    } yield constraint match {

      case ByLine =>
        qhit.iTextReflows.map(_.targetRegion)

      case ByRegion =>
        // val regionId = docStore.addTargetRegion(qhit.pageId, qhit.pageSpaceBounds)
        // val newRegion = docStore.getTargetRegion(regionId)
        val pageStableId = docStore.getPageIdentifier(qhit.pageId)
        val pageRegion = PageRegion(pageStableId, qhit.pageQueryBounds)
        Seq(pageRegion)

      case ByChar =>
        qhit.iTextReflows.map(_.textReflow.targetRegion)

    }).flatten


    val maybeZoneId = docStore.labelRegions(label, pageRegionsToBeLabeled)
  }

  def addLabel(queryBounds: LTBounds, constraint: Constraint, label: Label): Unit = {
    val queryHits = queryRegion(queryBounds)
    val constrainedHits = applyConstraint(constraint, queryHits)
    labelConstrained(constraint, constrainedHits, label)
  }


  val interpLabelAction: LabelAction ~> State[InterpState, ?] =
    new (LabelAction ~> State[InterpState, ?]) {

      def apply[A](fa: LabelAction[A]) =  {

        fa match {
          case act@ LabelAction.SelectZone(zoneId) =>

            println(s"SelectZone")
            for {
              _ <- State.modify[InterpState] { interpState =>
                // Add zoneId to client-side state
                val st0 = istate.selectionsL.modify(interpState) { sels => zoneId +: sels }

                // Traverse the widget tree, adding indicators around all of the zone boxes
                istate.labelWidgetL.modify(st0) { labelWidget =>
                  LabelWidgetTransforms.atEveryId(zoneId, labelWidget, { lw: LabelWidget =>
                    lw.project match {
                      case fa@ Figure(wid, fig) =>
                        val ff = LabelWidgets.figure(
                          composeFigures(makeFringe(fig, Padding(10)), fig)
                        )

                        // println(s"match Figure: ${ff}")
                        ff
                      case _ => lw
                    }
                  })

                  // Add a "fringe" bbox around everything Identified as id=zoneId
                  // st.copy(changes = add :: st.changes)
                  // labelWidget

                }


              }
            } yield zoneId

          case act: DeleteZone =>
            println(s"DeleteZone")
            for {
              init <- State.get[InterpState]
            } yield ()

          case act: MergeZones => 
            println(s"MergeZone")
            for {
              init <- State.get[InterpState]
            } yield ()
        }
      }

    }


  def runLabelAction[A](program: Free[LabelAction, A], uiResponse: UIResponse, widget: LabelWidget): InterpState = {
    program.foldMap(interpLabelAction)
      .apply(InterpState(uiResponse, widget))
      ._1
  }

  implicit class LabelActionOps[A](ma: Free[LabelAction, A]) {
    def exec(uiResponse: UIResponse, widget: LabelWidget): InterpState = runLabelAction(ma, uiResponse, widget)
  }



  // TODO this part really needs to be confined to WatrColors front end codebase
  // I think this should be an abstract function, then overridden per labeler type
  // map (UIState, Gesture) => (UIState, UIChanges)
  def userInteraction(uiState: UIState, gesture: Gesture): (UIResponse, LabelWidget) = {
    val UIState(constraint, maybeLabel, selections) = uiState
    val initResponse = UIResponse(uiState, List())
    gesture match {

      case MenuAction(action) =>
        val run = action.exec(initResponse, layout.labelWidget)

        (run.uiResponse, run.labelWidget)


      case Click(point) =>

        queryForPanels(point)
          .foldLeft((initResponse, layout.labelWidget)) {
            case ((accResponse, accWidget), (panel, qhit))  =>

              panel.interaction match {
                case InteractProg(prog) =>
                  println(s"Interpreting ${prog} in panel ${panel}")

                  val run = prog.exec(accResponse, accWidget)

                  (run.uiResponse, run.labelWidget)

                case _ =>
                  // TODO
                  (accResponse, accWidget)
              }
          }



      case DblClick(point) =>
        (initResponse, layout.labelWidget)

      case SelectRegion(bbox) =>
        maybeLabel.map {label =>
          println(s"adding label to bbox ${bbox}")
          addLabel(bbox, constraint, label)
          // UIAdd()
        }
        (initResponse, layout.labelWidget)

    }

  }

  def debugPrint(query: Option[LTBounds] = None): Unit = {
    val fillers = "αßΓπΣσµτΦΘΩδ∞φε∩".toList
    var _filler = -1
    def nextFiller(): Char = {
      _filler = (_filler + 1) % fillers.length
      fillers(_filler)
    }

    val w: Int = (layout.bleedBounds.width).intValue()+1
    val h: Int = (layout.bleedBounds.height).intValue()+1

    val graphPaper = GraphPaper.create(w, h)
    val graphPaper2 = GraphPaper.create(w, h)

    layout.positioning.foreach { pos =>
      val gridbox = GraphPaper.ltb2box(pos.strictBounds)

      pos.widget match {
        case l @ RegionOverlay(wid, under, overlays) =>

          val pageId = under.page.pageId
          // println(s"debugPrint: ${l} @ ${gridbox}")
          val id = pageId.unwrap
          val fill = (id + '0'.toInt).toChar
          graphPaper.fillFg(fill, gridbox)
          graphPaper.border(gridbox, Colors.Red)

        case _ =>
      }
    }

    layout.positioning.foreach { pos =>
      val gridbox = GraphPaper.ltb2box(pos.strictBounds)
      pos.widget match {
        case l @ Panel(wid, a, i) =>
          graphPaper2.fillFg(nextFiller(), gridbox)
        case l @ Figure(wid, fig) =>
          graphPaper.fillFg(nextFiller(), gridbox)
        case l @ Identified(wid, a, id, cls)    =>
        case _ =>
      }
    }

    layout.positioning.foreach { pos =>
      val gridbox = GraphPaper.ltb2box(pos.strictBounds)
      pos.widget match {
        case Col(wid, as) => graphPaper.borderLeftRight(gridbox, Colors.Gray)
        case Row(wid, as) => graphPaper.borderTopBottom(gridbox, Colors.Red)

        // case l : RegionOverlay[A]     => l.overlays.traverse(f).map(ft => l.copy(overlays=ft))
        // case l @ Pad(a, pd, clr)      => f(a).map(Pad(_, pd, clr))
        // case l : LabeledTarget        => G.point(l.copy())
        // case l : TextBox              => G.point(l.copy())
        // case l : Reflow               => G.point(l.copy())
        // case l : Figure               => G.point(l.copy())
        // case l @ Panel(a, i)          => f(a).map(Panel(_, i))
        // case l @ Identified(a, id, cls)    => f(a).map(Identified(_, id, cls))
        case _ =>
      }
    }
    query foreach { q =>
      println(s"query: $q")
      q.intersection(graphPaper.bbox).foreach{ clippedQuery =>
        println(s"clipped query: $clippedQuery")
        graphPaper.shadeBackground(GraphPaper.ltb2box(clippedQuery), Colors.Blue3)
        graphPaper2.shadeBackground(GraphPaper.ltb2box(clippedQuery), Colors.White)
      }
    }


    val grid1 = graphPaper.asString()
    val grid2 = graphPaper2.asString()

    println(grid1)
    // println
    // println(grid2)
    // println
  }
}
