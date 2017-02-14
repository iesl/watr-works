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

  implicit val DocumentIDMeta: Meta[Int @@ DocumentID] =
    Meta[Int].xmap(
      n => DocumentID(n),
      docId => docId.unwrap
    )

  implicit val TextReflowIDMeta: Meta[Int @@ TextReflowID] =
    Meta[Int].xmap(
      n => TextReflowID(n),
      tt => tt.unwrap
    )

  implicit val RegionIDMeta: Meta[Int @@ RegionID] =
    Meta[Int].xmap(
      n => RegionID(n),
      tt => tt.unwrap
    )
  implicit val PageIDMeta: Meta[Int @@ PageID] =
    Meta[Int].xmap(
      n => PageID(n),
      tt => tt.unwrap
    )

  implicit val PageNumMeta: Meta[Int @@ PageNum] =
    Meta[Int].xmap(
      n => PageNum(n),
      tt => tt.unwrap
    )

  implicit val ZoneIDMeta: Meta[Int @@ ZoneID] =
    Meta[Int].xmap(
      n => ZoneID(n),
      tt => tt.unwrap
    )

  implicit val TargetRegionMeta: Composite[TargetRegion] =
    Composite[(Int@@RegionID) :: (String@@DocumentID) :: (Int@@PageNum) :: LTBounds :: HNil].xmap({
      case regionId :: docId :: pageNum :: ltb :: HNil =>
        TargetRegion(regionId, docId, pageNum, ltb)
    },{targetRegion =>
      val TargetRegion(regionId, docId, pageNum, ltb@LTBounds(bl, bt, bw, bh)) = targetRegion
      regionId :: docId :: pageNum :: ltb :: HNil
    })

  case class ZonedRegion(
    id: Int@@ZoneID,
    targetRegion: TargetRegion
  )

  implicit val ZonedRegionMeta: Composite[ZonedRegion] =
    Composite[(Int@@ZoneID) :: TargetRegion :: HNil]
      .xmap({ case zoneId :: tr :: HNil =>
        ZonedRegion(zoneId, tr)
      },{case zonedRegion =>
          zonedRegion.id :: zonedRegion.targetRegion :: HNil
      })

  // implicit val DocumentMeta     : Composite[Model.Document] =
  //   Composite[(Int@@DocumentID) :: (String@@DocumentID) :: HNil].xmap({
  //     case f0 :: f1 :: HNil =>
  //       Model.Document(f0, f1)
  //     },{ case model =>
  //         model.prKey :: model.stableId :: HNil
  //     })

  implicit val PageMeta         : Composite[Model.Page] =
    Composite[(Int@@PageID) :: (Int@@DocumentID) :: (Int@@PageNum) :: LTBounds :: HNil].xmap({
      case f0 :: f1 :: f2 :: f3 :: HNil =>
        Model.Page(f0, f1, f2, f3 )
      },{ case model =>
          model.prKey :: model.document :: model.pagenum :: model.bounds :: HNil
      })

  // implicit val TargetRegionMeta : Composite[Model.TargetRegion] =
  //   Composite[(Int@@RegionID) :: (Int@@PageID) :: LTBounds :: String :: HNil].xmap({
  //     case f0 :: f1 :: f2 :: f3 :: HNil =>
  //       Model.TargetRegion(f0, f1, f2, f3)
  //     },{ case model =>
  //         model.prKey :: model.page :: model.bounds :: model.uri :: HNil
  //     })

  // implicit val ZoneMeta         : Composite[Model.Zone] =
  // implicit val TextReflowMeta   : Composite[Model.TextReflow]
  // implicit val LabelMeta        : Composite[Model.Label]
  // implicit val LabelWidgetMeta  : Composite[Model.LabelWidget]
  // implicit val LabelingTaskMeta : Composite[Model.LabelingTask]


}
