package edu.umass.cs.iesl.watr
package watrcolors

import boopickle.Default._

object Picklers {

  implicit def updatePickler = compositePickler[HtmlUpdate]
    .addConcreteType[HtmlPrepend]
    .addConcreteType[HtmlAppend]
    .addConcreteType[HtmlReplace]
    .addConcreteType[HtmlReplaceInner]
    .addConcreteType[HtmlRemove]
}


sealed trait HtmlUpdate

case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
case class HtmlAppend(css: String, content: String) extends HtmlUpdate
case class HtmlReplace(css: String, content: String) extends HtmlUpdate
case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
case class HtmlRemove(css: String) extends HtmlUpdate
