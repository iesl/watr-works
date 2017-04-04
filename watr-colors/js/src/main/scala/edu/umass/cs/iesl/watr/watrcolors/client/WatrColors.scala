package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.collection.mutable

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

import TypeTagPicklers._
import watrmarks.{StandardLabels => LB}

import scaladget.stylesheet.{all => sheet}
import scaladget.api.{BootstrapTags => bs}
import scalatags.JsDom.all._
import sheet._

import rx._


@JSExportTopLevel("WatrColors")
object WatrColors extends LabelerRendering {


  var selectionInProgress = false

  def queryParams(): Map[String, String] = {
    val params = mutable.HashMap[String, String]()

    val href = dom.window.location.href
    if (href.contains("?")) {
      val qs = href.split("\\?", 2)(1)
      for {
        qparam <- qs.split('&')
      } {
        qparam.split('=') match {
          case Array(k) =>  params(k) = ""
          case Array(k, v) => params(k) = v
          case _ =>
        }
      }
    }
    params.toMap
  }

  var uiState: UIState = UIState(
    ByChar,
    Option(LB.Title),
    Seq()
  )

  def updateStatusText(): Unit = {
    // Display state in status bar
    val statusText = uiState.toString()
    // jQuery("#status-text").html(statusText)
  }

  object states {
    def modState(f: UIState => UIState): Unit = {
      uiState = f(uiState)
      updateStatusText()
    }

    def selectByChar(): Unit = modState(_.copy(selectionConstraint = ByChar))
    def selectByLine(): Unit = modState(_.copy(selectionConstraint = ByLine))
    def selectByRegion(): Unit = modState(_.copy(selectionConstraint = ByRegion))
    def setLabel(l: Label): Unit = modState(_.copy(selectedLabel=Option(l)))

  }

  val keybindings: List[(String, (MousetrapEvent) => Unit)] = List(
    "l t" -> ((e: MousetrapEvent) => states.setLabel(LB.Title)),
    "l a" -> ((e: MousetrapEvent) => states.setLabel(LB.Authors)),
    "l b" -> ((e: MousetrapEvent) => states.setLabel(LB.Abstract)),
    "l f" -> ((e: MousetrapEvent) => states.setLabel(LB.Affiliation)),
    "l r" -> ((e: MousetrapEvent) => states.setLabel(LB.References)),

    "s l" -> ((e: MousetrapEvent) => states.selectByLine()),
    "s c" -> ((e: MousetrapEvent) => states.selectByChar()),
    "s b" -> ((e: MousetrapEvent) => states.selectByRegion()),

    "s s" -> ((e: MousetrapEvent) => startSelection())
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

  def uiRequestCycle(req: UIRequest) = for {
    uiResponse  <- shell.uiRequest(req)
  } {
    uiState = uiResponse.uiState
    updateStatusText()
    val adds = uiResponse.changes
      .collect {
        case UIAdd(widget) => widget
      }
    val dels = uiResponse.changes
      .collect {
        case UIDel(widget) => widget
      }
    val (bbox, fobjs) = renderLabelWidget(adds)
    fabricCanvas.renderOnAddRemove = false
    fobjs.foreach{os => os.foreach(fabricCanvas.add(_)) }
    fabricCanvas.renderAll()
    fabricCanvas.renderOnAddRemove = true
  }


  @JSExport
  def startSelection(): Unit = {
    selectionInProgress = true

    fabricCanvas.defaultCursor = "crosshair"
    fabricCanvas.renderAll()

    for {
      bbox <- getUserSelection(fabricCanvas)
    } yield {
      fabricCanvas.defaultCursor = "default"
      val req = UIRequest(uiState, SelectRegion(bbox))
      uiRequestCycle(req)
      selectionInProgress = false
    }

  }

  object shell {
    val Client = new WebsideClient("shell")
    val api = Client[WatrShellApi]

    def uiRequest(r: UIRequest): Future[UIResponse] = {
      api.uiRequest(r).call()
    }

    def createDocumentLabeler(stableId: String@@DocumentID, labelerType: String): Future[(Seq[WidgetPositioning], LabelOptions)] = {
      api.createDocumentLabeler(stableId, labelerType).call()
    }

  }

  def clear(): Unit = {
    fabricCanvas.clear()
  }

  def echoLabeler(lwidget: Seq[WidgetPositioning], labelOptions: LabelOptions): Unit = Async.async {
    println("echoLabeler()")
    fabricCanvas.renderOnAddRemove = false
    clear()
    val (bbox, fobjs) = renderLabelWidget(lwidget)
    fabricCanvas.setWidth(bbox.width.toInt)
    fabricCanvas.setHeight(bbox.height.toInt)

    // val overlay = jQuery("#canvas-overlay")
    // overlay.height(bbox.height.toInt)
    // overlay.width(bbox.width.toInt)


    fobjs.foreach{os =>
      os.foreach(fabricCanvas.add(_))
    }

    fabricCanvas.renderAll()
    fabricCanvas.renderOnAddRemove = true
    val controls = createLabelerControls(labelOptions)
    val c = controls.render
    // val statusBar = dom.document.getElementById("status-controls")
    // statusBar.childNodes.foreach( statusBar.removeChild(_)  )
    // statusBar.appendChild(c)
    // updateStatusText()
  }


  def param(k: String):Option[String] = {
    val params = queryParams()
    if (params.contains(k)){
      Option(params(k))
    } else None
  }


  // val currLabelerType: Var[String] = Var("")
  val currDocumentId: Var[Option[String]] = Var(None)
  import TypeTags._
  import dom.raw.MouseEvent


  @JSExport
  def setupClickCatchers(enable: Boolean): Unit = {
    val clickcb: js.Function1[MouseEvent, Boolean] = { (event: MouseEvent) =>
      if (!selectionInProgress) {
        println("click")

        val clickPt = getCanvasPoint(event.pageX.toInt, event.pageY.toInt)
        clickPt

        val req = UIRequest(uiState, Click(clickPt))
        uiRequestCycle(req)
      }
      true
    }


    val elem = dom.document
      .getElementById("canvas-container")

    elem.addEventListener("click", clickcb, useCapture=false)
  }

  def initRx(): Unit = {
    currDocumentId.foreach { maybeDocId =>
      maybeDocId.foreach{docId =>
        shell.createDocumentLabeler(DocumentID(docId), "")
          .foreach { case (lwidget, opts) =>
            echoLabeler(lwidget, opts)
          }
      }
    }
  }


  @JSExport
  def display(): Unit = {
    implicit val ctx: Ctx.Owner = Ctx.Owner.safe()

    bs.withBootstrapNative {
      initKeybindings()

      val nav = PageLayout.initNavbar(List())

      val mainContent =
        div(
          ^.id:="canvas-container",
          pageStyles.canvasContainer,
          sheet.marginLeft(15), sheet.marginTop(25)
        )(
          canvas(^.id:="canvas", pageStyles.fabricCanvas)
        )

      PageLayout.pageSetup(nav, mainContent).render
    }

    initRx()
    val c = fabricCanvas
    setupClickCatchers(true)
    currDocumentId() = param("doc")


  }





}
