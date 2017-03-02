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


sealed trait Constraint
case object ByLine extends Constraint
case object ByChar extends Constraint
case object ByRegion extends Constraint

import geometry._
import watrmarks._

sealed trait Interact
case class Selected(bbox: LTBounds) extends Interact
case class Clicked(point: Point) extends Interact

case class ActiveLabel(l: Option[Label])

case class Create()
case class Delete()

// case class AddLabel
// case class RemoveLabel
