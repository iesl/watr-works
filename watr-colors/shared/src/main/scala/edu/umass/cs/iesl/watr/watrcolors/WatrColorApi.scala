package edu.umass.cs.iesl.watr
package watrcolors

import upickle.default._

// sealed trait HtmlUpdate

// case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
// case class HtmlAppend(css: String, content: String) extends HtmlUpdate
// case class HtmlReplace(css: String, content: String) extends HtmlUpdate
// case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
// case class HtmlRemove(css: String) extends HtmlUpdate

// case class HtmlUpdates(updates: List[HtmlUpdate])

// object htmlUpdates {
//   def apply(updates: HtmlUpdate*) = new HtmlUpdates(updates.toList)
// }

trait WatrColorApi {

  def navNext()     : Seq[HtmlUpdate]
  def navPrev()     : Seq[HtmlUpdate]
  def openCurrent() : Seq[HtmlUpdate]

}





