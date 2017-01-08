package edu.umass.cs.iesl.watr
package watrcolors


sealed trait HtmlUpdate

final case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
final case class HtmlAppend(css: String, content: String) extends HtmlUpdate
final case class HtmlReplace(css: String, content: String) extends HtmlUpdate
final case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
final case class HtmlRemove(css: String) extends HtmlUpdate

// trait HtmlUpdatePicklers {
//   import boopickle.DefaultBasic._

//   implicit val pickler0 = compositePickler[HtmlUpdate]

//   implicit val p1: Pickler[HtmlAppend] = PicklerGenerator.generatePickler[HtmlAppend]
//   implicit val p2: Pickler[HtmlPrepend] = PicklerGenerator.generatePickler[HtmlPrepend]
//   implicit val p3: Pickler[HtmlRemove] = PicklerGenerator.generatePickler[HtmlRemove]
//   implicit val p4: Pickler[HtmlReplaceInner] = PicklerGenerator.generatePickler[HtmlReplaceInner]
//   implicit val p5: Pickler[HtmlReplace] = PicklerGenerator.generatePickler[HtmlReplace]

//   pickler0
//     .addConcreteType[HtmlPrepend]
//     .addConcreteType[HtmlAppend]
//     .addConcreteType[HtmlRemove]
//     .addConcreteType[HtmlReplaceInner]
//     .addConcreteType[HtmlReplace]
// }

final case class RemoteCall(
  path: List[String], args: List[(String, Array[Byte])]
)


object RemoteCall {
  import boopickle.DefaultBasic._
  implicit val rcp: Pickler[RemoteCall] = PicklerGenerator.generatePickler[RemoteCall]
}

import geometry._

import boopickle._
import boopickle.DefaultBasic._

trait TextReflowBoopicklers extends GeometryBoopicklers {
  // import TextReflowF._
  import watrmarks.Label
  import boopickle.Default._

  implicit val TextReflowTPickler: P[TextReflowT] = new P[TextReflowT] {
    override def pickle(tr: TextReflowT)(implicit state: PickleState): Unit = {
      Pickle.intoBytes(tr)
    }

    override def unpickle(implicit state: UnpickleState): TextReflowT = {
      state.unpickle[TextReflowT]
    }
  }

  implicit val TextReflowPickler: P[TextReflow] = new P[TextReflow] {
    override def pickle(tr: TextReflow)(implicit state: PickleState): Unit = {
      Pickle.intoBytes(tr.unFix)
    }

    override def unpickle(implicit state: UnpickleState): TextReflow = {
      state.unpickle[TextReflow]
    }
  }
}


import TypeTagPicklers._

trait GeometryBoopicklers extends PicklerHelper {
  import PicklerGenerator._

  import watrmarks._
  import geometry._


  implicit val pGeometricFigure = compositePickler[GeometricFigure]
  implicit val pLTBounds        = generatePickler[LTBounds]
  implicit val pLBBounds        = generatePickler[LBBounds]
  implicit val pPoint           = generatePickler[Point]
  implicit val pLine            = generatePickler[Line]

  pGeometricFigure
    .addConcreteType[LTBounds]
    .addConcreteType[LBBounds]
    .addConcreteType[Point]
    .addConcreteType[Line]

  // implicit val pPageGeometry = generatePickler[PageGeometry]
  implicit val pTargetRegion   = generatePickler[TargetRegion]
  implicit val pCharAtom       = generatePickler[CharAtom]
  // implicit val pTargetFigure = generatePickler[TargetFigure]
  // implicit val pLabel        = generatePickler[Label]
  // implicit val pZone         = generatePickler[Zone]


  // implicit val p02 = transformPickler[CharAtom, (TargetRegion, String, Int)](
  //   t => new CharAtom(t._1, t._2, if(t._3==0) None else Some(t._3)))(
  //   t => (t.targetRegion, t.char, t.wonkyCharCode.getOrElse(0)))

}


object TypeTagPicklers extends PicklerHelper {

  implicit val Int_RegionID_Pickler: Pickler[Int @@ RegionID] = new Pickler[Int @@ RegionID] {
    override def pickle(obj: Int @@ RegionID)(implicit state: PickleState): Unit = write[Int](obj.unwrap)
    override def unpickle(implicit state: UnpickleState): Int @@ RegionID = RegionID(read[Int])
  }
  implicit val String_DocumentID_Pickler: Pickler[String @@ DocumentID] = new Pickler[String @@ DocumentID] {
    override def pickle(obj: String @@ DocumentID)(implicit state: PickleState): Unit = write[String](obj.unwrap)
    override def unpickle(implicit state: UnpickleState): String @@ DocumentID = DocumentID(read[String])
  }
  implicit val Int_PageID_Pickler: Pickler[Int @@ PageID] = new Pickler[Int @@ PageID] {
    override def pickle(obj: Int @@ PageID)(implicit state: PickleState): Unit = write[Int](obj.unwrap)
    override def unpickle(implicit state: UnpickleState): Int @@ PageID = PageID(read[Int])
  }
  implicit val Int_LabelID_Pickler: Pickler[Int @@ LabelID] = new Pickler[Int @@ LabelID] {
    override def pickle(obj: Int @@ LabelID)(implicit state: PickleState): Unit = write[Int](obj.unwrap)
    override def unpickle(implicit state: UnpickleState): Int @@ LabelID = LabelID(read[Int])
  }
}
