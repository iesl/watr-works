package edu.umass.cs.iesl.watr
package spindex

import scalaz.@@
import watrmarks._

import IndexShapeOperations._

case class TargetRegion(
  id: Int@@RegionID,
  target: Int@@PageID,
  bbox: LTBounds
)

case class Zone(
  id: Int@@ZoneID,
  regions: List[TargetRegion]
) {
  def withLabel(l: Label) = ZoneAndLabel(
    this.id, l
  )
}

case class PageGeometry(
  id: Int@@PageID,
  bounds: LTBounds,
  borders:Option[Borders]
)

case class PageRegions(
  id: Int@@PageID,
  regions: Seq[PageRegion]
)

sealed trait PageRegion {
  def region: TargetRegion
}

// object PageRegion {
//   def apply(
//     region: TargetRegion,
//     char: String,
//     subs: String = "",
//     wonkyCharCode: Option[Int] = None
//   ) = CharRegion(region, char, subs, wonkyCharCode)

//   def apply(region: TargetRegion) = ImgRegion(region)



// }


class CharRegion(
  val region: TargetRegion,
  val char: String,
  val subs: String = "",
  val wonkyCharCode: Option[Int] = None
) extends PageRegion {
  // override def toString = {
  //   " "
  // }
}

object CharRegion {
  def unapply(r: CharRegion): Option[(TargetRegion, String, String, Option[Int])] =
    Some((r.region, r.char, r.subs, r.wonkyCharCode))

  def apply(region: TargetRegion,
     char: String,
     subs: String = "",
     wonkyCharCode: Option[Int] = None
  ): CharRegion = new CharRegion(region, char, subs, wonkyCharCode)

}


class ImgRegion(
  val region: TargetRegion
) extends PageRegion

object ImgRegion {
  def unapply(rg: ImgRegion): Option[(TargetRegion)] = Some(rg.region)
  def apply(region: TargetRegion): ImgRegion = new ImgRegion(region)

}


case class FontInfo(
  // fontName: String,
  fontFamily: String,
  fontSize: String
)

case class ZoneAndLabel(zoneId: Int@@ZoneID, label:Label)

case class ZoneRecords(
  id: String,
  target: String,
  pageGeometries: Seq[PageGeometry],
  zones: Seq[Zone],
  labels: Seq[ZoneAndLabel],
  pageRegions: Seq[PageRegions]
)

trait ComponentDataTypeFormats extends TypeTagFormats {
  import play.api.libs.json._


  implicit def FormatLBBounds         = Json.format[LBBounds]
  implicit def FormatLTBounds         = Json.format[LTBounds]
  implicit def FormatTargetedBounds   = Json.format[TargetRegion]
  implicit def FormatBorders          = Json.format[Borders]
  implicit def FormatLabel            = Json.format[Label]
  implicit def FormatPageGeometry     = Json.format[PageGeometry]
  implicit def FormatZone             = Json.format[Zone]
  implicit def FormatZoneAndLabel     = Json.format[ZoneAndLabel]
  implicit def FormatCharRegion       = Json.format[CharRegion]
  implicit def FormatImgRegion        = Json.format[ImgRegion]
  implicit def FormatPageRegion:Format[PageRegion]       = ??? // Json.format[PageRegion]
  implicit def FormatPageRegions:Format[PageRegions]      = ??? // Json.format[PageRegions]
  implicit def FormatZoneRecords:Format[ZoneRecords]      = ??? // Json.format[ZoneRecords]


}

object ComponentTypeEnrichments {

  implicit class RicherZone(val zone: Zone) extends AnyVal {

    def area(): Double = {
      zone.regions.foldLeft(0d){ case (acc, a) =>
        a.bbox.area
      }
    }

  }
  implicit class RicherPageRegion(val pageRegion: PageRegion) extends AnyVal {

    def prettyPrint: String = pageRegion match {
      case b: CharRegion => b.prettyPrint
      case b: ImgRegion => "<img>"
    }

    def bestGuessChar: String = pageRegion match {
      case b: CharRegion => b.bestGuessChar
      case b: ImgRegion => ""
    }

    def isSpace: Boolean = pageRegion match {
      case b: CharRegion => b.isSpace
      case b: ImgRegion => false
    }

    def isChar: Boolean = pageRegion match {
      case b: CharRegion => true
      case b: ImgRegion => false
    }
    // def isWonky: Boolean = charRegion.wonkyCharCode.isDefined


  }


  implicit class RicherCharRegion(val charRegion: CharRegion) extends AnyVal {

    def debugPrint: String = {
      val bbox = charRegion.region.bbox.prettyPrint

      val wonk = charRegion.wonkyCharCode
        .map({ code =>
          if (code==32) { "<sp>"  }
          else { s"?:#${code}?" }
        }) getOrElse { "" }

      val subs = if (!charRegion.subs.isEmpty()) s"@`charRegion.subs`" else ""


      s"""${charRegion.char}${subs} ${wonk} ${bbox}"""
    }

    def prettyPrint: String = {
      charRegion.wonkyCharCode
        .map({ code =>
          if (code==32) { "_"  }
          else { s"#${code}?" }
        })
        .getOrElse({
          if (!charRegion.subs.isEmpty()) charRegion.subs
          else charRegion.char
        })
    }

    def bestGuessChar: String = {
      charRegion.wonkyCharCode
        .map({ code =>
          if (code==32) { s" "  }
          else { s"#{${code}}" }
        })
        .getOrElse({
          if (!charRegion.subs.isEmpty()) charRegion.subs
          else charRegion.char
        })
    }

    def isWonky: Boolean = charRegion.wonkyCharCode.isDefined

    def isSpace: Boolean = charRegion.wonkyCharCode.exists(_==32)

  }
}
