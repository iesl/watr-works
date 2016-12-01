package edu.umass.cs.iesl.watr
package textreflow


object TextReflowTransforms {
  import spindex._
  import TextReflowF._
  import GeometricFigure._
  import TypeTags._

  import play.api.libs.json._

  def extractTargetRegions(t: TextReflowF[(TextReflow, Seq[TargetRegion])]): Seq[TargetRegion] = {
    val emptyTargetRegion = TargetRegion(
      RegionID(0), PageID(0), LTBounds(0, 0, 0, 0)
    )

    def zeros(i: Int) = Seq.fill(i)(emptyTargetRegion)

    t match {
      case Atom    (c, ops)               =>
        val ac = c.asInstanceOf[AtomicComponent]
        val pageAtom = ac.pageAtom
        ops.chars.map(_ => pageAtom.region)

      case Insert  (value)                 => zeros(value.length())
      case Rewrite ((from, attr), to)      => attr ++ zeros(to.length)
      case Bracket (pre, post, (a, attr))  => zeros(pre.length) ++ attr ++ zeros(post.length)
      case Flow    (labels, atomsAndattrs) => atomsAndattrs.flatMap(_._2)
      case Labeled (labels, (a, attr))     => attr
    }
  }

  def serializeTextReflow(t: TextReflowF[(TextReflow, Seq[TargetRegion])]): Seq[TargetRegion] = {
    val emptyTargetRegion = TargetRegion(
      RegionID(0), PageID(0), LTBounds(0, 0, 0, 0)
    )

    def zeros(i: Int) = Seq.fill(i)(emptyTargetRegion)

    t match {
      case Atom    (c, ops)               =>
        val ac = c.asInstanceOf[AtomicComponent]
        val pageAtom = ac.pageAtom
        ops.chars.map(_ => pageAtom.region)

      case Insert  (value)                 => zeros(value.length())
      case Rewrite ((from, attr), to)      => attr ++ zeros(to.length)
      case Bracket (pre, post, (a, attr))  => zeros(pre.length) ++ attr ++ zeros(post.length)
      case Flow    (labels, atomsAndattrs) => atomsAndattrs.flatMap(_._2)
      case Labeled (labels, (a, attr))     => attr
    }
  }

  def serializeTextReflow(textReflow: TextReflow): JsValue = {
    ???
  }
}
