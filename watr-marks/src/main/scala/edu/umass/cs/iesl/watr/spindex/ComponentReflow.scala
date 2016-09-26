package edu.umass.cs.iesl.watr
package spindex

import java.net.URI
import textboxing.{TextBoxing => TB}
import GeometricFigure._

object ComponentReflow {

  case class Reflow(content: Content)

  sealed trait Align
  sealed trait AlignV extends Align
  sealed trait AlignH extends Align

  object Align {
    case object Left extends AlignH
    case object Right extends AlignH
    case object Center extends Align
    case object Top extends AlignV
    case object Bottom extends AlignV
  }


  sealed trait Content
  object Content {
    case object Space extends Content

    case class StringCC(uri: URI, text: String) extends Content
    case class Text(uri: URI, text: String) extends Content
    case class CC(c:Component, text: String) extends Content
    case class Concat(cs: Seq[Content]) extends Content

    case class Clipped(b:Reflow, clip: LTBounds) extends Content
    case class Row(bs:Seq[Reflow]) extends Content
    case class Col(bs:Seq[Reflow]) extends Content
    case class Nested(a1: Align, a2: Align, b:Reflow) extends Content

  }

}
