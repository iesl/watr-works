package edu.umass.cs.iesl.watr
package spindex

import scalaz.@@
import watrmarks._

import IndexShapeOperations._
import GeometricFigure._

case class TargetRegion(
  id: Int@@RegionID,
  target: Int@@PageID,
  bbox: LTBounds
)

case class Zone(
  id: Int@@ZoneID,
  regions: Seq[TargetRegion]
) {
  def withLabel(l: Label) = ZoneAndLabel(
    this.id, l
  )
}

case class PageGeometry(
  id: Int@@PageID,
  bounds: LTBounds
)

case class PageAtoms(
  id: Int@@PageID,
  regions: Seq[PageAtom]
)

sealed trait PageAtom {
  def region: TargetRegion
}

class CharAtom(
  val region: TargetRegion,
  val char: String,
  val subs: String = "",
  val wonkyCharCode: Option[Int] = None
) extends PageAtom

object CharAtom {
  def unapply(r: CharAtom): Option[(TargetRegion, String, String, Option[Int])] =
    Some((r.region, r.char, r.subs, r.wonkyCharCode))

  def apply(region: TargetRegion,
     char: String,
     subs: String = "",
     wonkyCharCode: Option[Int] = None
  ): CharAtom = new CharAtom(region, char, subs, wonkyCharCode)

}


class ImgAtom(
  val region: TargetRegion
) extends PageAtom

object ImgAtom {
  def unapply(rg: ImgAtom): Option[(TargetRegion)] = Some(rg.region)
  def apply(region: TargetRegion): ImgAtom = new ImgAtom(region)

}

case class GlyphInstance(
  glyphHash: String
)


case class GlyphClass(
  glyphHash: String,
  char: Char,
  fontInfo: FontClass
)



case class FontClass(
  name: String,
  familyName: String,
  vendorName: String,
  italicAngle: Int, // = rnd(italic angle * 100)
  weight: String
)

case class ZoneAndLabel(zoneId: Int@@ZoneID, label:Label)

case class ZoneRecords(
  id: String,
  target: String,
  pageGeometries: Seq[PageGeometry],
  zones: Seq[Zone],
  labels: Seq[ZoneAndLabel],
  pageRegions: Seq[PageAtoms]
)

trait ComponentDataTypeFormats extends TypeTagFormats {
  import play.api.libs.json._


  implicit def FormatLBBounds         = Json.format[LBBounds]
  implicit def FormatLTBounds         = Json.format[LTBounds]
  implicit def FormatTargetedBounds   = Json.format[TargetRegion]
  implicit def FormatLabel            = Json.format[Label]
  implicit def FormatPageGeometry     = Json.format[PageGeometry]
  implicit def FormatZone             = Json.format[Zone]
  implicit def FormatZoneAndLabel     = Json.format[ZoneAndLabel]
  implicit def FormatCharAtom       = Json.format[CharAtom]
  implicit def FormatImgAtom        = Json.format[ImgAtom]
  implicit def FormatPageAtom:Format[PageAtom]       = ??? // Json.format[PageAtom]
  implicit def FormatPageAtoms:Format[PageAtoms]      = ??? // Json.format[PageAtoms]
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
  implicit class RicherPageAtom(val pageRegion: PageAtom) extends AnyVal {

    def prettyPrint: String = pageRegion match {
      case b: CharAtom => b.prettyPrint
      case b: ImgAtom => "<img>"
    }

    def bestGuessChar: String = pageRegion match {
      case b: CharAtom => b.bestGuessChar
      case b: ImgAtom => ""
    }

    def isSpace: Boolean = pageRegion match {
      case b: CharAtom => b.isSpace
      case b: ImgAtom => false
    }

    def isChar: Boolean = pageRegion match {
      case b: CharAtom => true
      case b: ImgAtom => false
    }
    // def isWonky: Boolean = charRegion.wonkyCharCode.isDefined


  }


  implicit class RicherCharAtom(val charRegion: CharAtom) extends AnyVal {

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
