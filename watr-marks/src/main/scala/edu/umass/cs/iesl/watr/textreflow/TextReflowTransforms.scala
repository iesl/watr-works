package edu.umass.cs.iesl.watr
package textreflow

import geometry._
import play.api.libs.json._
import Json._
import watrmarks.Label

trait TextReflowJsonFormats extends ComponentDataTypeFormats {
  import play.api.libs.json._
  import TextReflowF._
  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  def textReflowToJson(textReflow: TextReflow): JsValue = {
    val res = textReflow.cata(attributePara(serializeTextReflow))
    res.toPair._1
  }

  def serializeTextReflow(t: TextReflowF[(TextReflow, JsValue)]): JsValue = t match {
    case Atom(c, ops)                     => obj("a" -> arr(jstr(ops.toString()), toJson(c.asInstanceOf[PageAtom])))
    case Insert (value)                   => jstr(value)
    case Rewrite ((from, attrJs), to)     => obj("s" -> arr(attrJs, JsString(to.toString)))
    case Bracket (pre, post, (a, attrJs)) => obj("b" -> arr(jstr(pre), attrJs, jstr(post)))
    case Mask    (mL, mR, (a, attrJs))    => obj("m" -> arr(mL, mR, attrJs))
    case Flow(atomsAndattrs)              => toJson(atomsAndattrs.map(_._2))
    case Labeled(labels, (a, attrJs))     => obj("l" -> arr(packLabels(labels), attrJs))
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
      case JsArray(jsAttrs) => Flow(jsAttrs.toList)
      case JsString(value) => Insert(value)
      case JsObject(fields) =>
        val field0 = fields.toList.headOption.getOrElse { sys.error("Empty object while unserializing text reflow") }
        field0 match {
          case ("a", JsArray(Seq(JsString(ops), pageAtomJs)))             => Atom(pageAtomJs.as[PageAtom], new TextReflowAtomOps(ops.toString()))
          case ("s", JsArray(Seq(fromJsValue, JsString(toStr))))          => Rewrite(fromJsValue, toStr)
          case ("b", JsArray(Seq(JsString(pre), jsAttr, JsString(post)))) => Bracket(pre, post, jsAttr)
          case ("m", JsArray(Seq(JsNumber(mL), JsNumber(mR), jsAttr)))     => Mask(mL.toInt, mR.toInt, jsAttr)
          case ("l", JsArray(Seq(labels, jsAttr)))                        => Labeled(unpackLabels(labels), jsAttr)
          case (_, _)                                                     => sys.error(s"couldn't match JsValue= ${jsValue}")
        }
      case _ => ???
    }
  }

  def refoldJsonTextReflow: GAlgebra[(TextReflow, ?), TextReflowF, TextReflow] = t => fixf(t match {
    case Atom(c, ops)                   => Atom(c, ops)
    case Insert(value)                  => Insert(value)
    case Rewrite ((from, attr), to)     => Rewrite(attr, to)
    case Bracket (pre, post, (a, attr)) => Bracket(pre, post, attr)
    case Mask    (mL, mR, (a, attr))    => Mask(mL, mR, attr)
    case Flow(atomsAndattrs)            => Flow(atomsAndattrs.map(_._2))
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
