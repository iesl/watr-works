package edu.umass.cs.iesl.watr
package geometry

import scalaz.Equal
import geometry.syntax._
import watrmarks._

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

case class StablePageID(
  stableId: String@@DocumentID,
  pageNum: Int@@PageNum
) {
  override def toString = s"""${stableId}/pg${pageNum}"""
}

case class RecordedPageID(
  pageId: Int@@PageID,
  stable: StablePageID
) {
  override def toString = s"""${stable}@${pageId}"""
}


case class PageRegion(
  page: RecordedPageID,
  bbox: LTBounds
)

case class TargetRegion(
  id: Int@@RegionID,
  page: RecordedPageID,
  bbox: LTBounds
) {
  lazy val uri = {
    import PageComponentImplicits._
    this.uriString
  }
  override def toString = s"""${page}/${id}@${bbox.prettyPrint}"""
}

object TargetRegion {

  implicit val EqualTargetRegion: Equal[TargetRegion] =
    Equal.equal((a, b) => a.id==b.id)

}


case class PageGeometry(
  id: Int@@PageNum,
  bounds: LTBounds
)

case class CharAtom(
  id: Int@@CharID,
  charRegion: PageRegion,
  char: String,
  wonkyCharCode: Option[Int] = None
)  {
  override def toString = s"CharAtom($char, $charRegion)"

  def bbox: LTBounds = charRegion.bbox
}

object CharAtom {

  implicit val EqualCharAtom: Equal[CharAtom] = 
    Equal.equal((a, b)  => a.id==b.id )

}

case class Zone(
  id: Int@@ZoneID,
  regions: Seq[TargetRegion],
  label: Label
)


object PageComponentImplicits {
  def createTargetRegionUri(stableId: String@@DocumentID, pageNum:Int@@PageNum, bbox: LTBounds): String = {
    s"${stableId}+${pageNum}+${bbox.uriString}"
  }

  implicit class RicherPageRegion(val thePageRegion: PageRegion) extends AnyVal {
    def union(r: PageRegion): PageRegion = {
      val samePage = thePageRegion.page.pageId == r.page.pageId
      if (!samePage) {
        sys.error(s"""cannot union PageRegions from different pages: ${thePageRegion} + ${r}""")
      }
      thePageRegion.copy(bbox = thePageRegion.bbox union r.bbox)
    }
    def intersects(r: PageRegion): Boolean = {
      val samePage = thePageRegion.page.pageId == r.page.pageId
      samePage && (thePageRegion.bbox intersects r.bbox)
    }

    def intersection(b: LTBounds): Option[PageRegion] = {
      thePageRegion.bbox
        .intersection(b)
        .map(b => thePageRegion.copy(bbox=b))
    }
  }

  implicit class RicherTargetRegion(val theTargetRegion: TargetRegion) extends AnyVal {
    def union(r: TargetRegion): TargetRegion = {
      if (theTargetRegion.page.pageId != r.page.pageId) {
        sys.error(s"""cannot union theTargetRegions from different pages: ${theTargetRegion} + ${r}""")
      }
      theTargetRegion.copy(bbox = theTargetRegion.bbox union r.bbox)
    }

    def intersects(r: TargetRegion): Boolean = {
      val samePage = theTargetRegion.page.pageId == r.page.pageId
      samePage && (theTargetRegion.bbox intersects r.bbox)
    }


    def intersection(b: LTBounds): Option[TargetRegion] = {
      theTargetRegion.bbox
        .intersection(b)
        .map(b => theTargetRegion.copy(bbox=b))
    }

    def splitHorizontal(r: TargetRegion): List[TargetRegion] = {
      if (theTargetRegion.page.pageId != r.page.pageId) {
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
      theTargetRegion.toString
    }

    def uriString: String = {
      createTargetRegionUri(
        theTargetRegion.page.stable.stableId,
        theTargetRegion.page.stable.pageNum,
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
