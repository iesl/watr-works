package edu.umass.cs.iesl.watr
package watrcolors

import scala.annotation.tailrec
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import scala.util.Random
import scala.concurrent.Future
import scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scalatags.JsDom.all._
import upickle.Js
import autowire._
import org.scalajs.jquery.jQuery
import upickle.default._

import SplitPane._

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


  def applyHtmlUpdates(updates: Seq[HtmlUpdate]): Unit = {
    updates.foreach { _ match {
        case HtmlAppend(css, content)       => jQuery(css).append(content)
        case HtmlPrepend(css, content)      => jQuery(css).prepend(content)
        case HtmlReplace(css, content)      => jQuery(css).replaceWith(content)
        case HtmlReplaceInner(css, content) => jQuery(css).html(content)
        case HtmlRemove(css)                => jQuery(css).remove()
      }
    }
  }

  def navNext(): Boolean = {
    server.navNext().call().foreach(applyHtmlUpdates(_))
    true
  }


  def navPrev(): Boolean = {
    server.navPrev().call() foreach (applyHtmlUpdates(_))
    true
  }

  def openCurrent(): Boolean = {
    server.openCurrent().call() foreach (applyHtmlUpdates(_))
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
    "j"     -> ((e: MousetrapEvent) => navNext),
    "k"     -> ((e: MousetrapEvent) => navPrev),
    "enter" -> ((e: MousetrapEvent) => openCurrent)
  ))

  def setKeybindings(kb: Keybindings) =  {
    kb.bindings.foreach { case(str, fn) =>
      Mousetrap.bind(str, fn)
    }
  }


  @JSExport
  def main(): Unit = {
    setKeybindings(initKeys)

    val _ = jQuery(dom.document).ready {() =>
      jQuery(".split-pane").splitPane();
      jQuery(".split-pane").trigger("resize");
    }

  }

}
