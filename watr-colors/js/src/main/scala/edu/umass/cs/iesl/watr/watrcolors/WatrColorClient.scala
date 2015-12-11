package edu.umass.cs.iesl.watr
package watrcolors


import scala.annotation.tailrec
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
    server.navNext().call().foreach { updatemap =>
      println(updatemap.toList.mkString(", "))
    }
    true
  }
  def navPrev(): Boolean = {
    server.navPrev().call().foreach { updatemap =>
      println(updatemap.toList.mkString(", "))
    }
    true
  }

  def openCurrent(): Boolean = {
    server.openCurrent().call().foreach { updatemap =>
      println(updatemap.toList.mkString(", "))
    }
    true
  }





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
    "enter" -> ((e: MousetrapEvent) => openCurrent)
  ))

  def setKeybindings(kb: Keybindings) =  {
    kb.bindings.foreach { case(str, fn) =>
      Mousetrap.bind(str, fn)
    }
  }

  // val windowPanes = List[WindowPane](WindowPane(initKeys, emptyPane))


  @JSExport
  def main(): Unit = {
    println("inside main")

    setKeybindings(initKeys)

    val inputBox = input.render
    val outputBox = div.render


    val _ = dom.document.body.appendChild(
      div(cls:="container",
        h1("<not just empty>")
      ).render
    )
  }

}
