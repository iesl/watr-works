package edu.umass.cs.iesl.watr
package watrcolors


sealed trait HtmlUpdate

final case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
final case class HtmlAppend(css: String, content: String) extends HtmlUpdate
final case class HtmlReplace(css: String, content: String) extends HtmlUpdate
final case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
final case class HtmlRemove(css: String) extends HtmlUpdate

trait HtmlUpdatePicklers {
  import boopickle.DefaultBasic._

  implicit val pickler0 = compositePickler[HtmlUpdate]

  implicit val p1: Pickler[HtmlAppend] = PicklerGenerator.generatePickler[HtmlAppend]
  implicit val p2: Pickler[HtmlPrepend] = PicklerGenerator.generatePickler[HtmlPrepend]
  implicit val p3: Pickler[HtmlRemove] = PicklerGenerator.generatePickler[HtmlRemove]
  implicit val p4: Pickler[HtmlReplaceInner] = PicklerGenerator.generatePickler[HtmlReplaceInner]
  implicit val p5: Pickler[HtmlReplace] = PicklerGenerator.generatePickler[HtmlReplace]

  pickler0
    .addConcreteType[HtmlPrepend]
    .addConcreteType[HtmlAppend]
    .addConcreteType[HtmlRemove]
    .addConcreteType[HtmlReplaceInner]
    .addConcreteType[HtmlReplace]
}

final case class RemoteCall(
  path: Seq[String], args: Seq[(String, Array[Byte])]
)

trait RemoteCallPicklers {
  import boopickle.DefaultBasic._


  implicit val rcp: Pickler[RemoteCall] = PicklerGenerator.generatePickler[RemoteCall]
}

