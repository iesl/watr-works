package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.async.Async
import scala.concurrent.Future

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
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
import scalatags.JsDom.{ styles  }
import sheet._
import bs._


// @JSExport
@JSExportTopLevel("WatrColors")
object WatrColors extends LabelerRendering {

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

    "s s" -> ((e: MousetrapEvent) => startSelection()),
    "s a" -> ((e: MousetrapEvent) => startSelectPoint())
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
  def startSelectPoint(): Unit = {
    for {
      clickPt <- getUserClickPoint(fabricCanvas)
    } yield {
      val req = UIRequest(uiState, Click(clickPt))
      uiRequestCycle(req)
    }
  }


  @JSExport
  def startSelection(): Unit = {

    fabricCanvas.defaultCursor = "crosshair"
    fabricCanvas.renderAll()

    for {
      bbox <- getUserSelection(fabricCanvas)
    } yield {
      fabricCanvas.defaultCursor = "default"
      val req = UIRequest(uiState, SelectRegion(bbox))
      uiRequestCycle(req)
    }

  }

  object shell {
    val Client = new WebsideClient("shell")
    val api = Client[WatrShellApi]

    def uiRequest(r: UIRequest): Future[UIResponse] = {
      api.uiRequest(r).call()
    }

  }

  @JSExport
  var interval: Double = 1000

  object WatrColorsApiListeners extends WatrColorsApi {

    @JSExport
    override def clear(): Unit = {
      fabricCanvas.clear()
    }

    @JSExport
    override def print(level: String, msg: String): Unit = {
      jQuery("#main").append(msg+" and more!")

      level match {
        case "error" => dom.console.error(msg)
        case "warn" => dom.console.warn(msg)
        case "info" => dom.console.info(msg)
        case "log" => dom.console.log(msg)
        case      _ => dom.console.log(level + ":" + msg)
      }
    }


    @JSExport
    override def echoLabeler(lwidget: Seq[WidgetPositioning], labelOptions: LabelOptions): Unit = Async.async {
      println("echoLabeler()")
      fabricCanvas.renderOnAddRemove = false
      clear()
      val (bbox, fobjs) = renderLabelWidget(lwidget)
      fabricCanvas.setWidth(bbox.width.toInt)
      fabricCanvas.setHeight(bbox.height.toInt)

      fobjs.foreach{os =>
        os.foreach(fabricCanvas.add(_))
      }

      fabricCanvas.renderAll()
      fabricCanvas.renderOnAddRemove = true
      val controls = createLabelerControls(labelOptions)
      val c = controls.render
      val statusBar = dom.document.getElementById("status-controls")
      // statusBar.childNodes.foreach( statusBar.removeChild(_)  )
      statusBar.appendChild(c)
      updateStatusText()
    }
  }

  def setupClickCatchers(): Unit = {
    import org.querki.jquery.JQueryEventObject
    import org.querki.jquery.EventHandler

    val clickcb: EventHandler = { (event: JQueryEventObject) =>
      val clickPt = getCanvasPoint(event.pageX, event.pageY)
      val req = UIRequest(uiState, Click(clickPt))
      uiRequestCycle(req)
      true
    }
    val dblclickcb: EventHandler = { (event: JQueryEventObject) =>
      val clickPt = getCanvasPoint(event.pageX, event.pageY)
      val req = UIRequest(uiState, DblClick(clickPt))
      uiRequestCycle(req)
      true
    }

    jQuery("#canvas-container").click(clickcb)
    jQuery("#canvas-container").dblclick(dblclickcb)
  }

  val host: String="localhost"
  val port: Int=9999

  @JSExport
  def display(): Unit = {

    initKeybindings()

    val websideServer = new WebsideServer(WatrColorsApiListeners)


    var success = false

    def rec(): Unit = {

      Ajax.post(s"http://$host:$port/notifications").onComplete {

        case util.Success(data) =>
          if (!success) println("WatrTable connected via POST /notifications")
          success = true
          interval = 1000


          websideServer.wire(data.responseText)
          rec()
        case util.Failure(e) =>
          if (success) println("Workbench disconnected " + e)
          success = false
          interval = math.min(interval * 2, 30000)
          dom.window.setTimeout(() => rec(), interval)
      }
    }

    lazy val canvasContainer: ModifierSeq = Seq(
      padding:="0",
      border:="0",
      margin:="0",
      position.relative
    )

    lazy val fabricCanvas: ModifierSeq = Seq(
      position.absolute,
      padding:="0",
      margin:="0",
      border := "0",
      left:="0",
      zIndex:=100,
      top:="0"
    )


    bs.withBootstrapNative {
      setupClickCatchers()
      rec()

      val page = navItem(span("/Labeler").render)

      val nav = PageLayout.initNavbar(List(page))

      val bod = div(
        div(sheet.marginLeft(15), sheet.marginTop(25))(
          div(^.id:="canvas-container", canvasContainer)(
            canvas(^.id:="canvas", fabricCanvas)
          )
        )
      )

      PageLayout.pageSetup(nav, bod).render
    }

  }





}
