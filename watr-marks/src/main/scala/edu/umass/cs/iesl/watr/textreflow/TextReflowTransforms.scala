package edu.umass.cs.iesl.watr
package textreflow

import spindex._
import play.api.libs.json._
import Json._

trait TextReflowFormats extends ComponentDataTypeFormats {

}

object TextReflowTransforms extends TextReflowFormats {
  import play.api.libs.json._
  import spindex._
  import TextReflowF._
  import GeometricFigure._
  // import EnrichGeometricFigures._
  import TypeTags._

  def extractTargetRegions(t: TextReflowF[(TextReflow, Seq[TargetRegion])]): Seq[TargetRegion] = {
    val emptyTargetRegion = TargetRegion(
      RegionID(0), PageID(0), LTBounds(0, 0, 0, 0)
    )

    def zeros(i: Int) = Seq.fill(i)(emptyTargetRegion)

    t match {
      case Atom    (c, ops)               =>
        val ac = c.asInstanceOf[AtomicComponent]
        val charAtom = ac.pageAtom
        // ops.chars.map(_ => pageAtom.targetRegion)
        ???

      case Insert  (value)                 => zeros(value.length())
      case Rewrite ((from, attr), to)      => attr ++ zeros(to.length)
      case Bracket (pre, post, (a, attr))  => zeros(pre.length) ++ attr ++ zeros(post.length)
      case Flow    (labels, atomsAndattrs) => atomsAndattrs.flatMap(_._2)
      case Labeled (labels, (a, attr))     => attr
    }
  }


  def serializeTextReflow(t: TextReflowF[(TextReflow, Seq[JsValue])]): Seq[JsValue] = {
    val emptyTargetRegion = TargetRegion(
      RegionID(0), PageID(0), LTBounds(0, 0, 0, 0)
    )

    def zeros(i: Int) = Seq.fill(i)(emptyTargetRegion)

    t match {
      case Atom(c, ops) =>
        val ac = c.asInstanceOf[AtomicComponent]
        val pageAtom = ac.pageAtom
        val pageAtomJs = toJson(pageAtom)
        Seq(obj("atom" -> arr(jstr(ops.toString()), pageAtomJs)))

      case Insert (value) =>
          Seq(obj("ins" -> jstr(value)))

      case Rewrite ((from, attr), to) =>
        Seq(obj("sub" -> arr(
          attr,
          JsString(to.toString))
        ))

      case Bracket (pre, post, (a, attr))  =>
        Seq(obj("br" -> arr(
          jstr(pre),
          attr,
          jstr(post)
        )))

      case Flow(labels, atomsAndattrs) =>
        Seq(obj("as" -> arr(
          atomsAndattrs.flatMap(_._2)
        )))

      case Labeled(labels, (a, attr)) =>
        labels.map(toJson(_))
        Seq(obj("lb" -> arr(
          attr
        )))
    }
  }

}
