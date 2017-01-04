package edu.umass.cs.iesl.watr

import scalaz.Tag
import scalaz.@@

sealed trait SHA1String

// Documents are identified by the SHA1 hash of their contents
sealed trait DocumentID extends SHA1String

sealed trait ZoneID
sealed trait LabelID
sealed trait RegionID

sealed trait PageID
sealed trait CharID
sealed trait ComponentID

sealed trait MentionID
sealed trait ClusterID
sealed trait RelationID


sealed trait Ranging
sealed trait Offset
sealed trait Length

sealed trait Percent

object TypeTags extends TypeTags

trait TypeTags {
  val SHA1String = Tag.of[SHA1String]

  val DocumentID = Tag.of[DocumentID]

  val ZoneID = Tag.of[ZoneID]
  val RegionID = Tag.of[RegionID]
  val PageID = Tag.of[PageID]
  val CharID = Tag.of[CharID]
  val ComponentID = Tag.of[ComponentID]
  val LabelID = Tag.of[LabelID]

  val MentionID  = Tag.of[MentionID]
  val ClusterID  = Tag.of[ClusterID]
  val RelationID = Tag.of[RelationID]

  val Ranging = Tag.of[Ranging]
  val Offset = Tag.of[Offset]
  val Length = Tag.of[Length]

  val Percent = Tag.of[Percent]

  // implicit class TagOps[A, T](val value: A@@T) extends AnyVal {
  //   def unwrap: A = Tag.of[T].unwrap(value)
  // }

  val emptyDocId: String@@DocumentID =
    DocumentID("empty")

  import scala.reflect._

  def formatTaggedType[T:ClassTag](tt: Int @@ T): String = {
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    s"${tagClsname}:${tt.unwrap}"
  }


  import boopickle.DefaultBasic._
  // import PicklerGenerator._


  // (implicit tpickler: Pickler[TagT]): Pickler[Int @@ TagT] = {
  implicit def IntTypeTag_Pickler[TagT]: Pickler[Int @@ TagT] = {
    transformPickler(
      (t:Int) => Tag.of[TagT](t))(
      t => (t.unwrap))
  }
  implicit def StringTypeTag_Pickler[TagT]: Pickler[String @@ TagT] = {
    transformPickler(
      (t:String) => Tag.of[TagT](t))(
      t => (t.unwrap))
  }

}
