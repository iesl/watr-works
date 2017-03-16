package edu.umass.cs.iesl.watr
package watrcolors
package client

import scala.async.Async
import scala.concurrent.Future

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.ext._

import autowire._
import upickle.{default => UPickle}
import UPickle._

// import geometry._
import labeling._
import watrmarks._
import native.mousetrap._

import TypeTagPicklers._
import watrmarks.{StandardLabels => LB}

@JSExport
object WatrColors extends LabelerRendering {

  var uiState: Option[UIState] = Option(UIState(
    ByChar,
    Option(LB.Title),
    Create
  ))

  def updateStatusText(): Unit = {
    // Display state in status bar
    val statusText = uiState.map(_.toString()).getOrElse { "UI State Not Set" }
    jQuery("#status-text").html(statusText)
  }
  object states {
    def modState(f: UIState => UIState): Unit = {
      uiState = uiState.map(f)
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


  @JSExport
  def startSelectPoint(): Unit = {
    uiState.map{ state =>
      for {
        clickPt <- getUserClickPoint(fabricCanvas)
      } yield {
        val req = UIRequest(state, Click(clickPt))

        for {
          uiResponse <- shell.uiRequest(req)
          change <- uiResponse.changes
          figure <- change.visual.figures
        } {
          addShape(figure, "black", "", 1f)
        }
      }
    }

  }
  @JSExport
  def startSelection(): Unit = {
    uiState.map{ state =>

      fabricCanvas.defaultCursor = "crosshair"
      fabricCanvas.renderAll()

      for {
        bbox <- getUserSelection(fabricCanvas)
      } yield {

        fabricCanvas.defaultCursor = "default"

        val req = UIRequest(state, SelectRegion(bbox))

        for {
          uiResponse <- shell.uiRequest(req)
          change <- uiResponse.changes
          figure <- change.visual.figures
        } {
          addShape(figure, "black", "", 1f)
        }
      }
    }

  }

  object shell {
    val Client = new WebsideClient("autowire")
    val api = Client[WatrShellApi]

    def uiRequest(r: UIRequest): Future[UIResponse] = {
      api.uiRequest(r).call()
    }

  }

  @JSExport
  var interval: Double = 1000

  object WatrColorsApiListeners extends WatrColorsApi {

    @JSExport
    def helloColors(msg: String): Unit = {
      println(msg)
    }

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

  @JSExport
  def main(host: String="localhost", port: Int=9999): Unit = {

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

    dom.window.addEventListener("load", (event: dom.Event) => {
      // defaultMouseHandler()
      rec()
    })
  }





}
