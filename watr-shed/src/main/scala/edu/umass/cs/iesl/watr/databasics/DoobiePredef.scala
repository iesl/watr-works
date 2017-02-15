package edu.umass.cs.iesl.watr
package databasics

import doobie.imports._
import shapeless._
import geometry._

import TypeTags._
import utils.EnrichNumerics._

trait DoobiePredef {

  def putStrLn(s: => String): ConnectionIO[Unit] =
    FC.delay(println(s))


  type Int4 = Int :: Int :: Int :: Int :: HNil

  implicit val LTBoundsMeta: Composite[LTBounds] =
    Composite[Int4].xmap({
      case l :: t :: w :: h :: HNil =>
        val (bl, bt, bw, bh) = (itod(l), itod(t), itod(w), itod(h))
        LTBounds(bl, bt, bw, bh)
    },{ltb =>
      val LTBounds(l, t, w, h) = ltb
      val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
      bl :: bt :: bw :: bh :: HNil
    })



  implicit val StrDocumentIDMeta: Meta[String @@ DocumentID] =
    Meta[String].nxmap(
      str => DocumentID(str),
      docId => docId.unwrap
    )

  import scala.reflect.runtime.universe._


  def TypeTagMeta[T: TypeTag](
    f: Int => Int@@T)(
    implicit T: TypeTag[Int@@T]
  ): Meta[Int@@T] = Meta[Int].xmap(n => f(n), _.unwrap)


  implicit val DocumentIDMeta   : Meta[Int@@DocumentID   ] = TypeTagMeta[DocumentID   ](DocumentID  (_))
  implicit val TextReflowIDMeta : Meta[Int@@TextReflowID ] = TypeTagMeta[TextReflowID ](TextReflowID(_))
  implicit val RegionIDMeta     : Meta[Int@@RegionID     ] = TypeTagMeta[RegionID     ](RegionID    (_))
  implicit val PageIDMeta       : Meta[Int@@PageID       ] = TypeTagMeta[PageID       ](PageID      (_))
  implicit val ImageIDMeta      : Meta[Int@@ImageID      ] = TypeTagMeta[ImageID      ](ImageID     (_))
  implicit val PageNumMeta      : Meta[Int@@PageNum      ] = TypeTagMeta[PageNum      ](PageNum     (_))
  implicit val ZoneIDMeta       : Meta[Int@@ZoneID       ] = TypeTagMeta[ZoneID       ](ZoneID      (_))
  implicit val LabelIDMeta      : Meta[Int@@LabelID      ] = TypeTagMeta[LabelID      ](LabelID     (_))


}
