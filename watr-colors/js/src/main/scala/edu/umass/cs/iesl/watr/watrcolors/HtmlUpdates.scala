package edu.umass.cs.iesl.watr
package watrcolors


sealed trait HtmlUpdate

case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
case class HtmlAppend(css: String, content: String) extends HtmlUpdate
case class HtmlReplace(css: String, content: String) extends HtmlUpdate
case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
case class HtmlRemove(css: String) extends HtmlUpdate
