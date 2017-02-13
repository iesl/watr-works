package edu.umass.cs.iesl.watr
package geometry

import scalaz.Equal
import scalaz.syntax.equal._

import watrmarks._

import TypeTags._

import GeometryImplicits._

case class TargetRegion(
  id: Int@@RegionID,
  stableId: String@@DocumentID,
  pageNum: Int@@PageNum,
  bbox: LTBounds
) {
  lazy val uri = {
    import PageComponentImplicits._
    this.uriString
  }
  override def toString = s"""<${uri}>"""
}

object TargetRegion {

  implicit val EqualTargetRegion: Equal[TargetRegion] = Equal.equal((a, b) => (a, b) match {
    case (TargetRegion(id, stableId, targetPage, bbox), TargetRegion(id2, stableId2, targetPage2, bbox2)) =>
      (id.unwrap==id2.unwrap
        && stableId.unwrap==stableId.unwrap
        && targetPage.unwrap==targetPage2.unwrap
        && (bbox: GeometricFigure) === bbox2
      )
    case (_, _) => false
  })

  def fromUri(uriString: String): TargetRegion = {
    val Array(stableId, pageId, l, t, w, h) = uriString.split("\\+")

    TargetRegion(RegionID(0), DocumentID(stableId), PageNum(pageId.toInt),
      LTBounds(l.toDouble, t.toDouble, w.toDouble, h.toDouble)
    )
  }
}

// More General version of TargetRegion
case class TargetFigure(
  id: Int@@RegionID,
  pageId: Int@@PageNum,
  figure: GeometricFigure
) {
  override def toString = s"""<fig.${id} pg.${pageId} ${figure.toString}>"""
}

case class Zone(
  id: Int@@ZoneID,
  regions: Seq[TargetRegion],
  labels: Seq[Label]
)

case class PageGeometry(
  id: Int@@PageNum,
  bounds: LTBounds
)

sealed trait PageAtom {
  def targetRegion: TargetRegion

  implicit val EqualPageAtom: Equal[PageAtom] = Equal.equal((a, b)  => (a, b) match {
    case (CharAtom(t, c, w), CharAtom(t2, c2, w2)) =>
      t===t2 && c==c2 && w==w2
    case (_, _) => false

  })
}

case class CharAtom(
  targetRegion: TargetRegion,
  char: String,
  wonkyCharCode: Option[Int] = None
) extends PageAtom {
  override def toString = s"CharAtom($char, $targetRegion)"
}

object CharAtom {
  implicit val EqualCharAtom: Equal[CharAtom] = Equal.equal((a, b)  => (a, b) match {
    case (CharAtom(t, c, w), CharAtom(t2, c2, w2)) =>
      t===t2 && c==c2 && w==w2
    case (_, _) => false
  })
}


class ImgAtom(
  val targetRegion: TargetRegion
) extends PageAtom

object ImgAtom {
  def unapply(rg: ImgAtom): Option[(TargetRegion)] = Some(rg.targetRegion)
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


object PageComponentImplicits {
  def createTargetRegionUri(stableId: String@@DocumentID, pageNum:Int@@PageNum, bbox: LTBounds): String = {
    s"${stableId}+${pageNum}+${bbox.uriString}"
  }

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

  }

  implicit class RicherPageGeometry(val thePageGeometry: PageGeometry) extends AnyVal {
    def toTargetRegion(stableId: String@@DocumentID): TargetRegion = {
      TargetRegion(
        RegionID(0),
        stableId,
        thePageGeometry.id,
        thePageGeometry.bounds
      )
    }
  }

  implicit class RicherTargetRegion(val theTargetRegion: TargetRegion) extends AnyVal {
    def union(r: TargetRegion): TargetRegion = {
      if (theTargetRegion.pageNum != r.pageNum) {
        sys.error(s"""cannot union theTargetRegions from different pages: ${theTargetRegion} + ${r}""")
      }
      theTargetRegion.copy(bbox = theTargetRegion.bbox union r.bbox)
    }

    def intersects(r: TargetRegion): Boolean = {
      val samePage = theTargetRegion.pageNum == r.pageNum
      samePage && (theTargetRegion.bbox intersects r.bbox)
    }

    def splitHorizontal(r: TargetRegion): List[TargetRegion] = {
      if (theTargetRegion.pageNum != r.pageNum) {
        sys.error(s"""cannot union theTargetRegions from different pages: ${theTargetRegion} + ${r}""")
      }

      val leftX = r.bbox.toWesternPoint.x
      val rightX = r.bbox.toEasternPoint.x
      val trbbox = theTargetRegion.bbox
      val leftRights = if (trbbox.intersectsX(leftX)){
        val splitLeft = trbbox.splitHorizontal(leftX)
        if (trbbox.intersectsX(rightX)){
          val splitRight = trbbox.splitHorizontal(rightX)
          splitLeft.head :: splitLeft.tail
        } else {
          List(splitLeft.head)
        }
      } else if (trbbox.intersectsX(rightX)){
        trbbox.splitHorizontal(rightX).tail
      } else {
        List()
      }

      leftRights.map({ltb=>
        theTargetRegion.copy(
          bbox = ltb
        )
      })
    }

    def prettyPrint(): String = {
      val pg = theTargetRegion.pageNum
      val bbox = theTargetRegion.bbox.prettyPrint
      s"""<target pg:${pg} ${bbox}"""
    }

    def uriString: String = {
      createTargetRegionUri(
        theTargetRegion.stableId,
        theTargetRegion.pageNum,
        theTargetRegion.bbox
      )
    }
  }


  implicit class RicherCharAtom(val charRegion: CharAtom) extends AnyVal {

    def debugPrint: String = {
      val bbox = charRegion.targetRegion.bbox.prettyPrint

      val wonk = charRegion.wonkyCharCode
        .map({ code =>
          if (code==32) { "<sp>"  }
          else { s"?:#${code}?" }
        }) getOrElse { "" }

      s"""${charRegion.char} ${wonk} ${bbox}"""
    }

    def prettyPrint: String = {
      charRegion.wonkyCharCode
        .map({ code =>
          if (code==32) { "_"  }
          else { s"#${code}?" }
        })
        .getOrElse({
          charRegion.char
        })
    }

    def bestGuessChar: String = {
      charRegion.wonkyCharCode
        .map({ code =>
          if (code==32) { s" "  }
          else { s"#{${code}}" }
        })
        .getOrElse({
          // if (charRegion)
          charRegion.char
        })
    }

    def isWonky: Boolean = charRegion.wonkyCharCode.isDefined

    def isSpace: Boolean = charRegion.wonkyCharCode.exists(_==32)

  }
}
