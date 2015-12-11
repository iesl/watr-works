package edu.umass.cs.iesl.watr
package watrcolors


import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import scala.util.Random
import scala.concurrent.Future
import scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scalatags.JsDom.all._
import upickle.default._
import upickle.Js
import autowire._

object Client extends autowire.Client[Js.Value, Reader, Writer]{
  override def doCall(req: Request): Future[Js.Value] = {
    dom.ext.Ajax.post(
      url = "/api/" + req.path.mkString("/"),
      data = upickle.json.write(Js.Obj(req.args.toSeq:_*))
    ).map(_.responseText)
     .map(upickle.json.read)
  }

  def read[Result: Reader](p: Js.Value) = readJs[Result](p)
  def write[Result: Writer](r: Result) = writeJs(r)
}

case class Keybindings(
  bindings: List[(String, (MousetrapEvent)=>Boolean)]
)

@JSExport
object WatrColorClient {

  val server = Client[WatrColorApi]

  def fileOpen(): Boolean = {
    println("called File:Open")
    true
  }

  def navNext(): Boolean = {
    server.navNext().call()
    true
  }

  def navEnter(): Boolean = {
    println("called File:Open")
    true
  }

  def navPrev(): Boolean = {
    println("called File:Open")
    true
  }


  def keybindings() =  {
    Mousetrap.bind("f o", (e: MousetrapEvent) => fileOpen)
    Mousetrap.bind("j", (e: MousetrapEvent) => navNext )
    Mousetrap.bind("k", (e: MousetrapEvent) => navPrev)
    Mousetrap.bind("enter", (e: MousetrapEvent) => navEnter)
  }


  import scalaz._
  import Scalaz._

  case class WindowPane(
    keys: Keybindings,
    html: HtmlTag
  )

  val emptyPane = div(
    p("<empty>")
  )

  val initKeys = Keybindings(List(
    "f o"   -> ((e: MousetrapEvent) => fileOpen),
    "j"     -> ((e: MousetrapEvent) => navNext),
    "k"     -> ((e: MousetrapEvent) => navPrev),
    "enter" -> ((e: MousetrapEvent) => navEnter)
  ))

  val windowPanes = List[WindowPane](WindowPane(initKeys, emptyPane))

  // val lzip = asdf.toZipper
  // val lzipr = lzip.get




  @JSExport
  def main(): Unit = {
    println("entered main fn")

    keybindings()

    val inputBox = input.render
    val outputBox = div.render

    def updateOutput() = {
      Client[WatrColorApi].list(inputBox.value).call().foreach { paths =>
        outputBox.innerHTML = ""
        outputBox.appendChild(
          ul(
            for(path <- paths) yield {
              li(path)
            }
          ).render
        )
      }
    }
    inputBox.onkeyup = {(e: dom.Event) =>
      updateOutput()
    }
    updateOutput()
    dom.document.body.appendChild(
      div(
        cls:="container",
        h1("File Browser"),
        p("Enter a file path to s"),
        inputBox,
        outputBox
      ).render
    )
  }
}
