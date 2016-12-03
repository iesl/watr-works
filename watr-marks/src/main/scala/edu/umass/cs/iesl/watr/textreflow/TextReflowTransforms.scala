package edu.umass.cs.iesl.watr
package textreflow

import spindex._
import play.api.libs.json._
import Json._
import scalaz._
// import Scalaz._

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


  // def extract2s[T](implicit T: Corecursive.Aux[T, Exp])
  //     : Int => Exp[T \/ Int] = x =>
  // if (x == 0) Num(x)
  // else if (x % 2 == 0) Mul(-\/(Num[T](2).embed), \/-(x.toInt / 2))
  // else Num(x)
  //
  // def extract2sNot5[T](x: Int)(implicit T: Corecursive.Aux[T, Exp])
  //     : Option[Exp[T \/ Int]] =
  //   if (x == 5) None else extract2s[T].apply(x).some
  //
  // 12.apoM[Fix[Exp]](extract2sNot5[Fix[Exp]])

  def unfoldJsonToTextReflow(jsValue: JsValue): Option[TextReflowF[TextReflow \/ JsValue]] = {
    jsValue match {
      case JsArray(jsAttrs) =>
        // Option(jsAttrs.toList.map(\/-(_)))
        None

      case JsObject(Seq(("atom",
        JsArray(Seq(
          JsString(ops), pageAtomJs
        ))
      ))) =>
        Option(Atom(
          pageAtomJs.validate[PageAtom],
          new TextReflowAtomOps(ops.toString())
        ))

      case JsObject(Seq(("ins",
        JsString(value)
      ))) =>
        Option((Insert(value)))

      case JsObject(Seq(("sub",
        JsArray(Seq(
          fromJsValue,
          JsString(toStr)
        ))
      ))) =>
        Option(Rewrite(
          \/-(fromJsValue),
          toStr
        ))
      case JsObject(Seq(("br",
        JsArray(Seq(
          JsString(pre), jsAttr, JsString(post)
        ))
      ))) =>
        Option(Bracket(
          pre, post, \/-(jsAttr)
        ))

      case JsObject(Seq(("as",
        JsArray(jsAttrs)
      ))) =>
        Option(Flow(Set(),
          jsAttrs.toList.map(\/-(_))
        ))

      case JsObject(Seq(("lb",
        JsArray(Seq(
          JsString(pre), jsAttr
        ))
      ))) =>
        Option(Labeled(Set(),
          \/-(jsAttr)
        ))
    }
  }


  def serializeTextReflow(t: TextReflowF[(TextReflow, JsValue)]): JsValue = {
    val emptyTargetRegion = TargetRegion(
      RegionID(0), PageID(0), LTBounds(0, 0, 0, 0)
    )

    def zeros(i: Int) = Seq.fill(i)(emptyTargetRegion)

    t match {
      case Atom(c, ops) =>
        val ac = c.asInstanceOf[AtomicComponent]
        val pageAtom = ac.pageAtom
        val pageAtomJs = toJson(pageAtom)
        obj("atom" -> arr(jstr(ops.toString()), pageAtomJs))

      case Insert (value) =>
        // obj("ins" -> jstr(value))
        obj("ins" -> value)

      case Rewrite ((from, attr), to) =>
        obj("sub" -> arr(
          attr,
          JsString(to.toString))
        )

      case Bracket (pre, post, (a, attr))  =>
        obj("br" -> arr(
          jstr(pre),
          attr,
          jstr(post)
        ))

      case Flow(labels, atomsAndattrs) =>
        obj("as" ->
          atomsAndattrs.map(_._2)
        )

      case Labeled(labels, (a, attr)) =>
        labels.map(toJson(_))
        obj("lb" -> arr(
          attr
        ))
    }
  }
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

}

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
