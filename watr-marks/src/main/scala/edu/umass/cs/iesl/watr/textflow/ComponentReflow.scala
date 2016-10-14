package edu.umass.cs.iesl.watr
package textflow

import spindex._
import java.net.URI
import GeometricFigure._
import scalaz.syntax.ToIdOps
import scalaz.syntax.std.ToListOps


case class TextFlow(flow: Seq[FlowUnit]) {
  def text: String = {
    flow.map({
      case u: FlowUnit.Atom => u.atomicComponent.chars
      case u: FlowUnit.Rewrite => u.rewrite
      case u: FlowUnit.Insert => u.value
    }).mkString
  }

}


object TextFlow extends ToListOps with ToIdOps {

  def toText(funit: FlowUnit): String = funit match {
    case u: FlowUnit.Atom => u.atomicComponent.chars
    case u: FlowUnit.Rewrite => u.rewrite
    case u: FlowUnit.Insert => u.value
  }


  def unzipTextFlow(textFlow: TextFlow): Seq[(String, FlowUnit)] = {

    def _loop(text:String, fUnits: Seq[FlowUnit]):  Seq[(String, FlowUnit)] = {
      fUnits.headOption
        .map({ headUnit =>
          val headStr = text.substring(0, headUnit.length)
          val tailStr = text.substring(headUnit.length, text.length)

          (headStr, headUnit) +: _loop(tailStr, fUnits.tail)
        })
        .getOrElse(Seq())
    }
    _loop(textFlow.text, textFlow.flow)
  }

  def foldMapTextFlow(tFlow: TextFlow, f: (String, FlowUnit) => (String, FlowUnit)): TextFlow = {

    def _loop(textFlow: Seq[(String, FlowUnit)]): Seq[(String, FlowUnit)] = {
      textFlow.headOption
        .map({ case (flowText, flowUnit) =>
          f(flowText, flowUnit) +: _loop(textFlow.tail)
        })
        .getOrElse(Seq())
    }

    val resultFlow = _loop(unzipTextFlow(tFlow))

    TextFlow(
      resultFlow.map(_._2)
    )
  }

  def append(l:String, b: TextFlow): TextFlow = {
    val rpad = FlowUnit.Insert(l)
    TextFlow(b.flow :+ rpad)
  }
  def prepend(l:String, b: TextFlow): TextFlow = {
    val lpad = FlowUnit.Insert(l)
    TextFlow(lpad +: b.flow)
  }

  def bracket(l:Char, r:Char, b: TextFlow): TextFlow = {
    bracket(l.toString(), r.toString(), b)
  }

  def bracket(l:String, r:String, b: TextFlow): TextFlow = {
    append(r, prepend(l, b))
  }

  private def mkPad(s: String) = TextFlow(Seq(FlowUnit.Insert(s)))

  def join(sep:String)(bs:TextFlow*): TextFlow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextFlow]): TextFlow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextFlow]): TextFlow = {
    val flowUnits = bs.map(unzipTextFlow(_)).flatten
    TextFlow(flowUnits.map(_._2))
  }

}

sealed trait FlowUnit {
  def length: Int
}


object FlowUnit {

  case class Atom(
    atomicComponent: AtomicComponent
  ) extends FlowUnit {
    val length: Int =  atomicComponent.chars.length
  }

  case class Rewrite(
    atom: Atom,
    rewrite: String
  ) extends FlowUnit {
    val length: Int =  rewrite.length
  }

  case class Insert(
    value: String
  ) extends FlowUnit {
    val length: Int = value.length
  }
}



object ComponentReflow {

  case class Reflow(content: Content)

  sealed trait Content
  sealed trait LineContent extends Content

  object Content {

    case class CC(c:Component, textFlow: TextFlow) extends LineContent
    case class Clipped(cc: LineContent, start: Int, len: Int) extends LineContent
    case class Concat(cs: Seq[LineContent]) extends LineContent

    case class Row(rs:Seq[Reflow]) extends Content

    // case class Clipped(b:Reflow, clip: LTBounds) extends Content
    // case class Col(bs:Seq[Reflow]) extends Content

  }

  def lineContentTextFlow(lc: LineContent): TextFlow = {
    lc match {
      case Content.CC(component, textFlow) =>
        textFlow

      case Content.Clipped(content, start, len) =>
        val tf = lineContentTextFlow(content)
        var totalLength = 0

        val slice = tf.flow
          .dropWhile( { funit =>
            val b = start > totalLength
            totalLength += funit.length
            b
          })
          .takeWhile({ funit =>
            val b = start+len >= totalLength
            totalLength += funit.length
            b
          })

        TextFlow(slice)

      case Content.Concat(rs) =>
        val combined = rs.map(lineContentTextFlow(_)).map{_.flow}
        TextFlow(combined.flatten)
    }
  }


  def stringMatch(s: String, r: Reflow): Option[Content.Clipped] = {
    r.content match {
      case lc:LineContent =>
        val textFlow = lineContentTextFlow(lc)
        val start = textFlow.text.indexOf(s)
        if (start > -1) {
          Some(Content.Clipped(lc, start, s.length))
        } else None

      case Content.Row(rs) => None
    }
  }

}
