package edu.umass.cs.iesl.watr
package geometry

import scalaz.Equal
import scalaz.syntax.equal._

import watrmarks._
import TypeTags._
import geometry.syntax._

/**

  Identifying a rectangular region on a page:

  - Stable Identifier: string that uniquely identfies a particular document (e.g., a filename, SHA1-hash of file content, etc)
  - Page number: 0-indexed
  - page geometry: bounding box as taken from PDF (MediaBox, CropBox, or whatever was specified in PDF)
  - bounding box: left/top/width/height bounds of region

  - DocumentID (as Int): corpus-specific identifier for document (document table primary key)
  - PageID: documentId + page number + page geometry
  - RegionID: PageID + bounding box

  */

sealed trait PageIdentifier

case class StablePageID(
  stableId: String@@DocumentID,
  pageNum: Int@@PageNum
) extends PageIdentifier

case class RecordedPageID(
  pageId: Int@@PageID,
  stablePageId: StablePageID
) extends PageIdentifier


sealed trait GeometricRegion


case class PageRegion(
  pageId: Int@@PageID,
  bbox: LTBounds,
  regionId: Option[Int@@RegionID] = None
) extends GeometricRegion

case class TargetRegion(
  id: Int@@RegionID,
  stableId: String@@DocumentID,
  pageNum: Int@@PageNum,
  bbox: LTBounds
) extends GeometricRegion {
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

case class CharAtom(
  id: Int@@CharID,
  targetRegion: TargetRegion,
  char: String,
  wonkyCharCode: Option[Int] = None
)   {
  override def toString = s"CharAtom($char, $targetRegion)"

  def bbox: LTBounds = targetRegion.bbox
}

object CharAtom {
  implicit val EqualCharAtom: Equal[CharAtom] = Equal.equal((a, b)  => (a, b) match {
    case (CharAtom(i, t, c, w), CharAtom(i2, t2, c2, w2)) =>
      i == i2
    case (_, _) => false
  })
}


// case class GlyphClass(
//   glyphHash: String,
//   char: Char,
//   fontInfo: FontClass
// )

// case class FontClass(
//   name: String,
//   familyName: String,
//   vendorName: String,
//   italicAngle: Int, // = rnd(italic angle * 100)
//   weight: String
// )


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

  implicit class RicherPageRegion(val thePageRegion: PageRegion) extends AnyVal {
    def union(r: PageRegion): PageRegion = {
      val samePage = thePageRegion.pageId == r.pageId
      if (!samePage) {
        sys.error(s"""cannot union PageRegions from different pages: ${thePageRegion} + ${r}""")
      }
      thePageRegion.copy(bbox = thePageRegion.bbox union r.bbox)
    }

    def intersects(r: PageRegion): Boolean = {
      val samePage = thePageRegion.pageId == r.pageId
      samePage && (thePageRegion.bbox intersects r.bbox)
    }

    // def intersects(r: TargetRegion): Boolean = {
    //   val samePage = thePageRegion.pageId == r.pageId
    //   samePage && (thePageRegion.bbox intersects r.bbox)
    // }


    def intersection(b: LTBounds): Option[PageRegion] = {
      thePageRegion.bbox
        .intersection(b)
        .map(b => thePageRegion.copy(bbox=b))
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


    def intersection(b: LTBounds): Option[TargetRegion] = {
      theTargetRegion.bbox
        .intersection(b)
        .map(b => theTargetRegion.copy(bbox=b))
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
      val bbox = charRegion.bbox.prettyPrint

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
