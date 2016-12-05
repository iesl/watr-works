package edu.umass.cs.iesl.watr
package textreflow

import spindex._
import play.api.libs.json._
import Json._


import watrmarks.Label
trait TextReflowJsonFormats extends ComponentDataTypeFormats {
  import play.api.libs.json._
  import spindex._
  import TextReflowF._
  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  def textReflowToJson(textReflow: TextReflow): JsValue = {
    val res = textReflow.cata(attributePara(serializeTextReflow))
    res.toPair._1
  }

  def serializeTextReflow(t: TextReflowF[(TextReflow, JsValue)]): JsValue = t match {
    case Atom(c, ops)                     => obj("atom" -> arr(jstr(ops.toString()), toJson(c.asInstanceOf[PageAtom])))
    case Insert (value)                   => obj("ins" -> jstr(value))
    case Rewrite ((from, attrJs), to)     => obj("sub" -> arr(attrJs, JsString(to.toString)))
    case Bracket (pre, post, (a, attrJs)) => obj("br" -> arr(jstr(pre), attrJs, jstr(post)))
    case Flow(labels, atomsAndattrs)      => obj("as" -> arr(packLabels(labels), atomsAndattrs.map(_._2)))
    case Labeled(labels, (a, attrJs))     => obj("lb" -> arr(packLabels(labels), attrJs))
  }


  def unpackLabels(js: JsValue): Set[Label] = js match {
    case JsArray(labelJs)  => Set(labelJs.map(_.as[Label]):_*)
    case _ => ???
  }

  def packLabels(labels: Set[Label]): JsValue =
    toJson(labels.map(toJson(_)).toList)


  def jsonToTextReflow(jsValue: JsValue): TextReflow = {
    jsValue
      .ana[TextReflow](unfoldJsonToTextReflow)
      .cata(attributePara(refoldJsonTextReflow))
      .toPair._1
  }

  def unfoldJsonToTextReflow: Coalgebra[TextReflowF, JsValue] = jsValue => {

  jsValue match {
    case JsObject(fields) =>
      val field0 = fields.toList.headOption.getOrElse { sys.error("Empty object while unserializing text reflow") }
      field0 match {
        case ("atom", JsArray(Seq(JsString(ops), pageAtomJs)))           => Atom(pageAtomJs.as[PageAtom], new TextReflowAtomOps(ops.toString()))
        case ("ins", JsString(value))                                    => Insert(value)
        case ("sub", JsArray(Seq(fromJsValue, JsString(toStr))))         => Rewrite(fromJsValue, toStr)
        case ("br", JsArray(Seq(JsString(pre), jsAttr, JsString(post)))) => Bracket(pre, post, jsAttr)
        case ("as", JsArray(Seq(labels, JsArray(jsAttrs))))              => Flow(unpackLabels(labels), jsAttrs.toList)
        case ("lb", JsArray(Seq(labels, jsAttr)))                        => Labeled(unpackLabels(labels), jsAttr)
        case (_, _)                                                      => sys.error(s"couldn't match JsValue= ${jsValue}")
      }
    case _ => ???
  }
}

  // type GAlgebra                   [W[_],            F[_],        A]          = F[W[A]]   => A
  def refoldJsonTextReflow: GAlgebra[(TextReflow, ?), TextReflowF, TextReflow] = t => fixf(t match {
    case Atom(c, ops)                   => Atom(c, ops)
    case Insert (value)                 => Insert(value)
    case Rewrite ((from, attr), to)     => Rewrite(attr, to)
    case Bracket (pre, post, (a, attr)) => Bracket(pre, post, attr)
    case Flow(labels, atomsAndattrs)    => Flow(labels, atomsAndattrs.map(_._2))
    case Labeled(labels, (a, attr))     => Labeled(labels, attr)

  })

}

object TextReflowTransforms extends TextReflowJsonFormats {}

// def unserializeTextReflow(jsValue: JsValue): Option[TextReflowF[TextReflow \/ JsValue]] = {
//   jsValue match {
//     case JsObject(Seq(("atom",
//       JsArray(Seq(
//         JsString(ops), pageAtomJs
//       ))
//     ))) =>
//       Option(Atom(
//         pageAtomJs.validate[PageAtom],
//         new TextReflowAtomOps(ops.toString())
//       ))

//     case JsObject(Seq(("ins",
//       JsString(value)
//     ))) =>
//       Option(Insert(value))

//     case JsObject(Seq(("sub",
//       JsArray(Seq(
//         fromJsValue,
//         JsString(toStr)
//       ))

//     ))) =>
//       Option(Rewrite(
//         \/-(value),
//         toStr
//       ))

//       None
//     case _ =>
//       None

//   }
// }

// def extractTargetRegions(t: TextReflowF[(TextReflow, Seq[TargetRegion])]): Seq[TargetRegion] = {
//   val emptyTargetRegion = TargetRegion(
//     RegionID(0), PageID(0), LTBounds(0, 0, 0, 0)
//   )
//   def zeros(i: Int) = Seq.fill(i)(emptyTargetRegion)
//   t match {
//     case Atom    (c, ops)               =>
//       val ac = c.asInstanceOf[AtomicComponent]
//       val charAtom = ac.pageAtom
//       // ops.chars.map(_ => pageAtom.targetRegion)
//       ???
//     case Insert  (value)                 => zeros(value.length())
//     case Rewrite ((from, attr), to)      => attr ++ zeros(to.length)
//     case Bracket (pre, post, (a, attr))  => zeros(pre.length) ++ attr ++ zeros(post.length)
//     case Flow    (labels, atomsAndattrs) => atomsAndattrs.flatMap(_._2)
//     case Labeled (labels, (a, attr))     => attr
//   }
// }

// def serializeTextReflow2(t: TextReflowF[(TextReflow, Seq[JsValue])]): Seq[JsValue] = {
//   val emptyTargetRegion = TargetRegion(
//     RegionID(0), PageID(0), LTBounds(0, 0, 0, 0)
//   )

//   def zeros(i: Int) = Seq.fill(i)(emptyTargetRegion)

//   t match {
//     case Atom(c, ops) =>
//       val ac = c.asInstanceOf[AtomicComponent]
//       val pageAtom = ac.pageAtom
//       val pageAtomJs = toJson(pageAtom)
//       Seq(obj("atom" -> arr(jstr(ops.toString()), pageAtomJs)))

//     case Insert (value) =>
//       Seq(obj("ins" -> jstr(value)))

//     case Rewrite ((from, attr), to) =>
//       Seq(obj("sub" -> arr(
//         attr,
//         JsString(to.toString))
//       ))

//     case Bracket (pre, post, (a, attr))  =>
//       Seq(obj("br" -> arr(
//         jstr(pre),
//         attr,
//         jstr(post)
//       )))

//     case Flow(labels, atomsAndattrs) =>
//       Seq(obj("as" -> arr(
//         atomsAndattrs.flatMap(_._2)
//       )))

//     case Labeled(labels, (a, attr)) =>
//       labels.map(toJson(_))
//       Seq(obj("lb" -> arr(
//         attr
//       )))
//   }
// }
