package edu.umass.cs.iesl.watr
package textflow

import spindex._
import java.net.URI
import GeometricFigure._
import EnrichGeometricFigures._
import scalaz.syntax.ToIdOps
import scalaz.syntax.std.ToListOps
import scalaz.@@
import ComponentTypeEnrichments._
import TypeTags._


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


  def flowUnitTargetRegion(funit: FlowUnit): Option[TargetRegion] = funit match {
    case u: FlowUnit.Atom => u.atomicComponent.targetRegion.some
    case u: FlowUnit.Rewrite => u.atom.atomicComponent.targetRegion.some
    case u: FlowUnit.Insert => None
  }

  def clipToTargetRegion(textFlow: TextFlow, targetRegion: TargetRegion): Option[(TextFlow, Int@@Offset, Int@@Length)] = {
    val clippedFlow = textFlow.flow
      .zipWithIndex
      .dropWhile({case (funit, _) =>
        val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
          !intersects
      })
      .reverse
      .dropWhile({case (funit, _) =>
        val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
          !intersects
      })
      .reverse

    if (clippedFlow.isEmpty) None else {
      val start = clippedFlow.head._2
      val end = clippedFlow.last._2

      Some((TextFlow(clippedFlow.map(_._1)), Offset(start), Length(end-start)))
    }

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
