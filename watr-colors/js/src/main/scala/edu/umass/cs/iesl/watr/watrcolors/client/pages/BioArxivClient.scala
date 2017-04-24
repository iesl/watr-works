package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages

import parts._
import wiring._

import scala.async.Async
import scala.concurrent.Future

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.annotation._
import org.scalajs.dom
import org.scalajs.dom.ext._

import autowire._
import upickle.{default => UPickle}
import UPickle._

import labeling._
import watrmarks._
import native.mousetrap._
import native.fabric

import TypeTagPicklers._
import watrmarks.{StandardLabels => LB}

import scaladget.stylesheet.{all => sty}
import scalatags.JsDom.all._

import rx._
import scaladget.tools.JsRxTags.{ctx => _, _}

import TypeTags._
import dom.raw.MouseEvent

import scala.collection.mutable

class ClientStateRx(implicit co: Ctx.Owner) {


  val labelerType: Var[Option[String]] = Var(None)
  val documentId: Var[Option[String]] = Var(None)
  val selectionConstraint: Var[Constraint] = Var(ByLine)
  val selectedLabel: Var[Option[Label]] = Var(None)
  val selections: Var[Seq[Int@@ZoneID]] = Var(Seq())

  val doMergeZones: Var[Boolean] = Var(false)
  val doDeleteZone: Var[Boolean] = Var(false)

  def toUIState = UIState(
    selectionConstraint.now,
    selectedLabel.now,
    selections.now
  )

  val selectionInProgress: Var[Boolean] = Var(false)

  def startSelection(): Unit = {
    selectionInProgress() = true
  }

}

@JSExportTopLevel("WatrColors")
object WatrColors extends  BaseClientDefs {
  import BootstrapBits._



  object states {
    // def modState(f: UIState => UIState): Unit = {
    //   uiState = f(uiState)
    // }

    // def selectByChar(): Unit = modState(_.copy(selectionConstraint = ByChar))
    // def selectByLine(): Unit = modState(_.copy(selectionConstraint = ByLine))
    // def selectByRegion(): Unit = modState(_.copy(selectionConstraint = ByRegion))
    // def setLabel(l: Label): Unit = modState(_.copy(selectedLabel=Option(l)))

  }

  val keybindings: List[(String, (MousetrapEvent) => Unit)] = List(
    // "l t" -> ((e: MousetrapEvent) => states.setLabel(LB.Title)),
    // "l a" -> ((e: MousetrapEvent) => states.setLabel(LB.Authors)),
    // "l b" -> ((e: MousetrapEvent) => states.setLabel(LB.Abstract)),
    // "l f" -> ((e: MousetrapEvent) => states.setLabel(LB.Affiliation)),
    // "l r" -> ((e: MousetrapEvent) => states.setLabel(LB.References)),

    // "s l" -> ((e: MousetrapEvent) => states.selectByLine()),
    // "s c" -> ((e: MousetrapEvent) => states.selectByChar()),
    // "s b" -> ((e: MousetrapEvent) => states.selectByRegion()),

    "s s" -> ((e: MousetrapEvent) => clientState.foreach {_.startSelection()})
  )

  def initKeybindings() = {
    Mousetrap.reset()
    keybindings.foreach { case (str, fn) =>
      val bindFunc: MousetrapEvent => Boolean = e => {
        fn(e); true
      }

      Mousetrap.bind(str, bindFunc, "keypress")
    }
  }


  val activeFabricObjects = mutable.HashMap[Int@@WidgetID, fabric.FabricObject]()

  def uiRequestCycle(req: UIRequest) = for {
    uiResponse  <- shell.uiRequest(req)
  } {
    println("complete:uiRequest ")
    // uiState = uiResponse.uiState
    fabricCanvas.renderOnAddRemove = false
    val adds = uiResponse.changes
      .collect {
        case AddLw(wid, widget) => widget.get
      }

    val dels = uiResponse.changes
      .collect {
        case RmLw(wid, widget) =>
          // val wid = widget.get
          activeFabricObjects.get(wid).map {fobj =>
            fabricCanvas.remove(fobj)
          }
      }

    // println(s"""adding: ${adds.mkString("\n")}""")
    // println(s"""rmv: ${dels.mkString("\n")}""")

    println("begin:renderLabelWidget")
    renderLabelWidget(adds).foreach {
      case (bbox, fobjs) =>
        println(s"adding within $bbox")
        fobjs.foreach{os => os.foreach{ case (wid, obj)  =>
          // println(s"  ++> ${o}")
          activeFabricObjects.put(wid, obj)
          fabricCanvas.add(obj)
        }}
        fabricCanvas.renderAll()
        fabricCanvas.renderOnAddRemove = true
    }
  }


  object shell {
    val Client = new WebsideClient("shell")
    val api = Client[WatrShellApi]

    def uiRequest(r: UIRequest): Future[UIResponse] = {
      println("begin:uiRequest ")
      api.uiRequest(r).call()
    }

    def createDocumentLabeler(stableId: String@@DocumentID, labelerType: String): Future[(Seq[AbsPosWidget], LabelOptions)] = {
      api.createDocumentLabeler(stableId, labelerType).call()
    }

  }

  def clear(): Unit = {
    fabricCanvas.clear()
  }


  def echoLabeler(lwidget: Seq[AbsPosWidget], labelOptions: LabelOptions): Unit = Async.async {
    println("echoLabeler()")
    try {
      renderLabelWidget(lwidget).foreach {
        case (bbox, fobjs) =>
          fabricCanvas.renderOnAddRemove = false
          clear()
          fabricCanvas.setWidth(bbox.width.toInt)
          fabricCanvas.setHeight(bbox.height.toInt)

          fobjs.foreach{os => os.foreach{ case (wid, obj)  =>
            activeFabricObjects.put(wid, obj)
            fabricCanvas.add(obj)
          }}
          fabricCanvas.renderAll()
          fabricCanvas.renderOnAddRemove = true
      }
    } catch {
      case t: Throwable =>
        println(s"error ${t}, ${t.getCause}")
        t.printStackTrace()
        throw t
    }
  }


  def createLabeler(docId: String, lt: String): Unit = {
    shell.createDocumentLabeler(DocumentID(docId), lt)
      .foreach { case (lwidget, opts) =>
        echoLabeler(lwidget, opts)
      }
  }


  @JSExport
  def setupClickCatchers(clientStateRx: ClientStateRx)(implicit co: Ctx.Owner): Unit = {
    val clickcb: js.Function1[MouseEvent, Boolean] = { (event: MouseEvent) =>
      if (!clientStateRx.selectionInProgress.now) {
        println("click")

        val clickPt = getCanvasPoint(event.pageX.toInt, event.pageY.toInt)

        val req = UIRequest(clientStateRx.toUIState, Click(clickPt))
        uiRequestCycle(req)
      }
      true
    }


    val elem = dom.document
      .getElementById("canvas-container")

    elem.addEventListener("click", clickcb, useCapture=false)
  }


  def initRx(clientStateRx: ClientStateRx)(implicit co: Ctx.Owner): Unit = Rx {

    for {
      b <- clientStateRx.selectionInProgress
    } yield {
      if (b) {

        fabricCanvas.defaultCursor = "crosshair"
        fabricCanvas.renderAll()

        for {
          bbox <- getUserSelection(fabricCanvas)
        } yield {
          fabricCanvas.defaultCursor = "default"

          val req = UIRequest(clientStateRx.toUIState, SelectRegion(bbox))
          uiRequestCycle(req)

          clientStateRx.selectionInProgress() = false

        }
      }
    }

    (clientStateRx.documentId(), clientStateRx.labelerType()) match {
      case (Some(docId), Some(lt)) =>
        createLabeler(docId, lt)

      case _ => // do nothing
    }

    clientStateRx.doDeleteZone.foreach{
      case b if b =>
        println("Delete: uiRequestCycle()")
        uiRequestCycle(
          UIRequest(
            clientStateRx.toUIState,
            MenuAction(LabelAction.deleteZone(ZoneID(0)))
          )
        )
    }

    clientStateRx.doMergeZones.foreach{
      case b if b =>
        println("Merge: uiRequestCycle()")
        uiRequestCycle(
          UIRequest(
            clientStateRx.toUIState,
            MenuAction(LabelAction.mergeZones(List()))
          )
        )
    }
  }


  var clientState: Option[ClientStateRx] = None

  @JSExport
  def display(): Unit = {
    implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

    val clientStateRx = new ClientStateRx
    clientState = Option(clientStateRx)


    withBootstrapNative {

      val selectorControls = SharedLayout.zoneSelectorControls(
        clientStateRx,
        List(
          LB.Title,
          LB.Authors,
          LB.Abstract,
          LB.Affiliation,
          LB.References
        ))


      val navContent =  SharedLayout.initNavbar(List())

      initKeybindings()

      val bodyContent = div(
        selectorControls,
        div(
          ^.id:="canvas-container",
          pageStyles.canvasContainer
          // sty.marginLeft(15), sty.marginTop(25)
        )(canvas(^.id:="canvas", pageStyles.fabricCanvasStyle))
      )

      val sidebarContent = ul(`class`:="sidebar-nav")

      SharedLayout.pageSetup(navContent, bodyContent, sidebarContent).render
    }

    initRx(clientStateRx)
    setupClickCatchers(clientStateRx)
    clientStateRx.documentId() = param("doc")
    clientStateRx.labelerType() = param("lt")
  }



}
