package edu.umass.cs.iesl.watr
package watrcolors

sealed trait HtmlUpdate

final case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
final case class HtmlAppend(css: String, content: String) extends HtmlUpdate
final case class HtmlReplace(css: String, content: String) extends HtmlUpdate
final case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
final case class HtmlRemove(css: String) extends HtmlUpdate

final case class RemoteCall(
  path: List[String], args: List[(String, String)]
)
