package edu.umass.cs.iesl.watr
package corpora
package database

import doobie._
import doobie.implicits._

import shapeless._
import geometry._
import corpora._
import TypeTags._
import scala.reflect.runtime.universe._


trait DoobieImplicits extends DoobiePredef {
  val R = RelationModel

  type Int4 = Int :: Int :: Int :: Int :: HNil

  implicit val readLTBounds: Read[LTBounds] =
    Read[Int4].map({
      case l :: t :: w :: h :: HNil =>
        LTBounds.IntReps(l, t, w, h)
    })

  implicit val writeLTBounds: Write[LTBounds] =
    Write[Int4].contramap({ ltb => 
      val LTBounds.IntReps(l, t, w, h) = ltb
      l :: t :: w :: h :: HNil
    })

  // implicit val LTBoundsMeta: Composite[LTBounds] =
  //   Composite[Int4].imap({
  //     case l :: t :: w :: h :: HNil =>
  //       LTBounds.IntReps(l, t, w, h)
  //   })({ltb =>
  //     val LTBounds.IntReps(l, t, w, h) = ltb
  //     l :: t :: w :: h :: HNil
  //   })


  implicit val StrDocumentIDMeta: Meta[String @@ DocumentID] =
    Meta[String].imap(str => DocumentID(str))(docId => docId.unwrap)

  private implicit def TypeTagMeta[T: TypeTag](
    f: Int => Int@@T)(
    implicit T: TypeTag[Int@@T]
  ): Meta[Int@@T] = Meta[Int].timap(n => f(n))(_.unwrap)

  private implicit def StrTypeTagMeta[T: TypeTag](
    f: String => String@@T)(
    implicit T: TypeTag[String@@T]
  ): Meta[String@@T] = Meta[String].timap(n => f(n))(_.unwrap)

  implicit val DocumentIDMeta    : Meta[Int@@DocumentID   ] = TypeTagMeta[DocumentID   ](DocumentID   (_))
  implicit val TextReflowIDMeta  : Meta[Int@@TextReflowID ] = TypeTagMeta[TextReflowID ](TextReflowID (_))
  implicit val RegionIDMeta      : Meta[Int@@RegionID     ] = TypeTagMeta[RegionID     ](RegionID     (_))
  implicit val PageIDMeta        : Meta[Int@@PageID       ] = TypeTagMeta[PageID       ](PageID       (_))
  implicit val CharIDMeta        : Meta[Int@@CharID       ] = TypeTagMeta[CharID       ](CharID       (_))
  implicit val ImageIDMeta       : Meta[Int@@ImageID      ] = TypeTagMeta[ImageID      ](ImageID      (_))
  implicit val PageNumMeta       : Meta[Int@@PageNum      ] = TypeTagMeta[PageNum      ](PageNum      (_))
  implicit val ZoneIDMeta        : Meta[Int@@ZoneID       ] = TypeTagMeta[ZoneID       ](ZoneID       (_))
  implicit val LabelIDMeta       : Meta[Int@@LabelID      ] = TypeTagMeta[LabelID      ](LabelID      (_))
  implicit val UserIDMeta        : Meta[Int@@UserID       ] = TypeTagMeta[UserID       ](UserID       (_))
  implicit val LockIDMeta        : Meta[Int@@LockID       ] = TypeTagMeta[LockID   ](LockID   (_))

  implicit val WorkflowIDMeta      : Meta[String@@WorkflowID   ] = StrTypeTagMeta[WorkflowID   ](WorkflowID (_))
  implicit val StatusCodeMeta      : Meta[String@@StatusCode   ] = StrTypeTagMeta[StatusCode   ](StatusCode (_))
  implicit val EmailAddrMeta       : Meta[String@@EmailAddr    ] = StrTypeTagMeta[EmailAddr    ](EmailAddr  (_))
  implicit val CorpusPathMeta      : Meta[String@@CorpusPath   ] = StrTypeTagMeta[CorpusPath   ](CorpusPath (_))
  implicit val CorpusPathQueryMeta : Meta[String@@CorpusPathQuery] = StrTypeTagMeta[CorpusPathQuery ](CorpusPathQuery (_))
  implicit val AnnotationIDMeta    : Meta[Int@@AnnotationID    ] = TypeTagMeta[AnnotationID    ](AnnotationID (_))
  implicit val LabelSchemaIDMeta   : Meta[Int@@LabelSchemaID   ] = TypeTagMeta[LabelSchemaID   ](LabelSchemaID (_))




}
