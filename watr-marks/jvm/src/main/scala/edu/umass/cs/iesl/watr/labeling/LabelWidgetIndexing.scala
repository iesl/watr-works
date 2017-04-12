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
  positioned: WidgetPositioning,
  pageId: Int@@PageID,
  pageSpaceBounds: LTBounds,
  iTextReflows: Seq[IndexableTextReflow]
)

object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object TextReflowIndexable extends SpatialIndexable[IndexableTextReflow] {
    def id(t: IndexableTextReflow): Int = t.id.unwrap
    def ltBounds(t: IndexableTextReflow): LTBounds = t.targetRegion.bbox
  }

  implicit object LabelWidgetIndexable extends SpatialIndexable[WidgetPositioning] {
    def id(t: WidgetPositioning): Int = t.widget.wid.unwrap
    def ltBounds(t: WidgetPositioning): LTBounds = t.strictBounds
  }


  import textreflow.TextReflowJsonCodecs._

  def create(docStore0: DocumentCorpus, lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[WidgetPositioning]()

    val layout0 = layoutWidgetPositions(lwidget)

    layout0.positioning.foreach({pos =>
      lwIndex.add(pos)
    })

    val targetPageRIndexes = mutable.HashMap[Int@@PageID, SpatialIndex[IndexableTextReflow]]()

    def addPage(pageId: Int@@PageID): Unit= {
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

      case l @ RegionOverlay(wid, pageId, pGeom, clipTo, overlays) =>
        addPage(pageId)

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
  def index: SpatialIndex[WidgetPositioning]
  def pageIndexes: Map[Int@@PageID, SpatialIndex[IndexableTextReflow]]

  def queryForPanels(queryPoint: Point): Seq[(Panel[Unit], WidgetPositioning)] = {
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

  def queryPage(pos: WidgetPositioning, queryBounds: LTBounds, pageId: Int@@PageID): Option[QueryHit] = {
    pos.strictBounds
      .intersection(queryBounds)
      .map { ibbox =>
        val pageSpaceBounds = ibbox.translate(pos.translation)
        val pageIndex = pageIndexes(pageId)
        val pageHits = pageIndex.queryForIntersects(pageSpaceBounds)
        QueryHit(pos, pageId, pageSpaceBounds, pageHits)
      }
  }

  def queryRegion(queryBounds: LTBounds): Seq[QueryHit] = {
    val hits = index
      .queryForIntersects(queryBounds)
      .map { pos => pos.widget match {

        case l @ RegionOverlay(wid, pageId, pGeom, clipTo, overlays) =>
          queryPage(pos, queryBounds, pageId)


        case _ => None
      }}

    hits.flatten
  }


  def labelConstrained(constraint: Constraint, queryHits: Seq[QueryHit], label: Label): Option[GeometricGroup] = {
    var changes = List[GeometricFigure]()

    val targetRegionsToBeLabeled: Seq[Seq[PageRegion]] = for {
      qhit <- queryHits
    } yield constraint match {
      case ByLabel(l) =>
        val regions = qhit.iTextReflows.map(_.targetRegion)
        changes = regions.map(_.bbox.translate(-qhit.positioned.translation)).toList
        regions

      case ByLine =>

        val regions = qhit.iTextReflows.map(_.targetRegion)
        changes = regions.map(_.bbox.translate(-qhit.positioned.translation)).toList
        regions

      case ByRegion =>
        // val regionId = docStore.addTargetRegion(qhit.pageId, qhit.pageSpaceBounds)
        // val newRegion = docStore.getTargetRegion(regionId)
        val pageStableId = docStore.getPageIdentifier(qhit.pageId)
        val pageRegion = PageRegion(pageStableId, qhit.pageSpaceBounds)
        val regions = Seq(pageRegion)
        changes = regions.map(_.bbox.translate(-qhit.positioned.translation)).toList
        regions

      case ByChar =>
        val regionss = for {
          iReflow <- qhit.iTextReflows
        } yield {
          val dbgR = iReflow.textReflow
          val dbgPr = dbgR.targetRegion()
          println(s"Clip   : ${dbgR.toText()}")
          println(s"         : ${dbgPr}  clipTo(")
          println(s"  ByChar : ${qhit.pageSpaceBounds}")

          iReflow.textReflow
            .clipToBoundingRegion(qhit.pageSpaceBounds)
            .map { case (clipped, range) =>
              val tr = clipped.targetRegion
              println(s"  clipped : ${}")
              // val regionId = docStore.addTargetRegion(tr.page.pageId, tr.bbox)
              // docStore.getTargetRegion(regionId)
              tr
            }

        }

        val regions = regionss.flatten
        changes = regions.map(_.bbox.translate(-qhit.positioned.translation)).toList

        regions
    }

    val maybeLabel = targetRegionsToBeLabeled.flatten

    maybeLabel.headOption
      .map { pageRegion =>
        val regionId = docStore.addTargetRegion(pageRegion.page.pageId, pageRegion.bbox)
        val zoneId = docStore.createZone(regionId, label)
        maybeLabel.tail.map { tr =>
          val rid = docStore.addTargetRegion(tr.page.pageId, tr.bbox)
          docStore.addZoneRegion(zoneId, rid)
        }
        docStore.getZone(zoneId)
      }


    if (changes.isEmpty) None else {
      val groupBbox = changes.map(totalBounds(_)).reduce(_ union _)
      Some(GeometricGroup(groupBbox, changes))
    }
  }

  def addLabel(queryBounds: LTBounds, constraint: Constraint, label: Label): Option[GeometricGroup] = {
    val queryHits = queryRegion(queryBounds)
    labelConstrained(constraint, queryHits, label)
  }


  val interpLabelAction: LabelAction ~> State[InterpState, ?] =
    new (LabelAction ~> State[InterpState, ?]) {

      def apply[A](fa: LabelAction[A]) =  {

        fa match {
          case act@ SelectZone(zoneId) =>

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

          case act: LabelAction.SelectRegion       => for { init <- State.get[InterpState] } yield ()
          case act: UnselectRegion     => for { init <- State.get[InterpState] } yield ()
          case act: UnselectZone       => for { init <- State.get[InterpState] } yield ()
          case act: CreateZone         => for { init <- State.get[InterpState] } yield ()
          case act: DeleteZone         => for { init <- State.get[InterpState] } yield ()
          case act: LabelZone          => for { init <- State.get[InterpState] } yield ()
          case act: CreateFigure       => for { init <- State.get[InterpState] } yield ???
          case act: QueryForRegions    => for { init <- State.get[InterpState] } yield Seq[TargetRegion]()
          case act: QueryForZones      => for { init <- State.get[InterpState] } yield Seq[Int@@ZoneID]()
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
  // map (UIState, Gesture) => (UIState, UIChanges)
  def userInteraction(uiState: UIState, gesture: Gesture): (UIResponse, LabelWidget) = {
    val initResponse = UIResponse(uiState, List())
    gesture match {

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
        (initResponse, layout.labelWidget)

    }

  }

  // def userInteraction(gesture: Gesture): Unit = {
  //   gesture match {

  //     case Click(point) =>
  //       queryForPanels(point)
  //         .map{ case(panel, qhit)  =>
  //           panel.interaction match {
  //             case InteractProg(prog) =>
  //               println(s"Interpreting ${prog} in panel ${panel}")

  //               prog.exec()

  //             case _ =>
  //           }
  //         }

  //     case DblClick(point) =>

  //     case SelectRegion(bbox) =>

  //   }

  //   ???
  // }


  def debugPrint(query: Option[LTBounds] = None): Unit = {
    val fillers = "αßΓπΣσµτΦΘΩδ∞φε∩".toList
    var _filler = -1
    def nextFiller(): Char = {
      _filler = (_filler + 1) % fillers.length
      fillers(_filler)
    }

    // println(s"Bleed: ${layout.bleedBounds}")
    // println(s"Strict: ${layout.strictBounds}")

    val w: Int = (layout.bleedBounds.width).intValue()+1
    val h: Int = (layout.bleedBounds.height).intValue()+1

    val graphPaper = GraphPaper.create(w, h)
    val graphPaper2 = GraphPaper.create(w, h)

    layout.positioning.foreach { pos =>
      val gridbox = GraphPaper.ltb2box(pos.strictBounds)

      pos.widget match {
        case l @ RegionOverlay(wid, pageId, pGeom, clipTo, overlays) =>
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

        // case l : RegionOverlay[A]     => l.overs.traverse(f).map(ft => l.copy(overs=ft))
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
      graphPaper.shadeBackground(GraphPaper.ltb2box(q), Colors.Red)
      graphPaper2.shadeBackground(GraphPaper.ltb2box(q), Colors.White)
    }


    val grid1 = graphPaper.asString()
    val grid2 = graphPaper2.asString()

    println(grid1)
    // println
    // println(grid2)
    // println
  }
}
