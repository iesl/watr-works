package edu.umass.cs.iesl.watr
package databasics

import doobie.imports._
import shapeless._
import geometry._

trait DoobiePredef {

  def putStrLn(s: => String): ConnectionIO[Unit] =
    FC.delay(println(s))

  def dtoi(d: Double): Int = (d*100.0).toInt
  def itod(i: Int): Double = (i.toDouble)/100.0

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

  implicit val DocumentIDMeta: Meta[String @@ DocumentID] =
    Meta[String].nxmap(
      str => DocumentID(str),
      docId => docId.unwrap
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

  implicit val ZoneIDMeta: Meta[Int @@ ZoneID] =
    Meta[Int].xmap(
      n => ZoneID(n),
      tt => tt.unwrap
    )

  implicit val TargetRegionMeta: Composite[TargetRegion] =
    Composite[(Int@@RegionID) :: (String@@DocumentID) :: (Int@@PageID) :: LTBounds :: HNil].xmap({
      case regionId :: docId :: pageId :: ltb :: HNil =>
        TargetRegion(regionId, docId, pageId, ltb)
    },{targetRegion =>
      val TargetRegion(regionId, docId, pageId, ltb@LTBounds(bl, bt, bw, bh)) = targetRegion
      regionId :: docId :: pageId :: ltb :: HNil
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



}
