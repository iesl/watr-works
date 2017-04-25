package edu.umass.cs.iesl.watr
package watrcolors
package client
package parts

import scala.collection.mutable

import scalatags.JsDom.{
  TypedTag,
  Modifier
}

import org.scalajs.dom.raw.{
  HTMLElement
}

import org.scalajs.dom

import rx._
// import rx.ops._
// import scaladget.tools.JsRxTags._

trait BaseClientDefs extends LabelerRendering {
  type HtmlTag = TypedTag[HTMLElement]
  type RxHtmlTag = Rx.Dynamic[TypedTag[HTMLElement]]
  type RxModifier = Rx.Dynamic[Modifier]
  type RxHtmlTags = Rx.Dynamic[Seq[TypedTag[HTMLElement]]]

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

  def param(k: String):Option[String] = {
    val params = queryParams()
    if (params.contains(k)){
      Option(params(k))
    } else None
  }

}
