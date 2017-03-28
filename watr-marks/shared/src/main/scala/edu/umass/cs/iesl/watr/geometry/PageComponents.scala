package edu.umass.cs.iesl.watr
package geometry

import scalaz.Equal
import scalaz.syntax.equal._

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
) extends PageIdentifier {
  override def toString = s"""${stableId}/pg${pageNum}"""
}

case class RecordedPageID(
  pageId: Int@@PageID,
  stable: StablePageID
) extends PageIdentifier {
  override def toString = s"""${stable}@${pageId}"""

}

// sealed trait GeometricRegion

// case class PgeRegion(
//   pageId: Int@@PageID,
//   bbox: LTBounds,
//   regionId: Option[Int@@RegionID] = None
// )

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
// stableId: String@@DocumentID,
// pageNum: Int@@PageNum,

object TargetRegion {

  // implicit val EqualTargetRegion: Equal[TargetRegion] = Equal.equal((a, b) => (a, b) match {
  //   case (TargetRegion(id, stableId, targetPage, bbox), TargetRegion(id2, stableId2, targetPage2, bbox2)) =>
  //     (id.unwrap==id2.unwrap
  //       && stableId.unwrap==stableId.unwrap
  //       && targetPage.unwrap==targetPage2.unwrap
  //       && (bbox: GeometricFigure) === bbox2
  //     )
  //   case (_, _) => false
  // })

}


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


object PageComponentImplicits {
  def createTargetRegionUri(stableId: String@@DocumentID, pageNum:Int@@PageNum, bbox: LTBounds): String = {
    s"${stableId}+${pageNum}+${bbox.uriString}"
  }

  // implicit class RicherPageGeometry(val thePageGeometry: PageGeometry) extends AnyVal {
  //   def toTargetRegion(stableId: String@@DocumentID): TargetRegion = {
  //     TargetRegion(
  //       RegionID(0),
  //       stableId,
  //       thePageGeometry.id,
  //       thePageGeometry.bounds
  //     )
  //   }
  // }

  // implicit class RicherPgeRegion(val thePageRegion: PgeRegion) extends AnyVal {
  //   def union(r: PgeRegion): PgeRegion = {
  //     val samePage = thePgeRegion.pageId == r.pageId
  //     if (!samePage) {
  //       sys.error(s"""cannot union PgeRegions from different pages: ${thePgeRegion} + ${r}""")
  //     }
  //     thePgeRegion.copy(bbox = thePgeRegion.bbox union r.bbox)
  //   }
  //   def intersects(r: PgeRegion): Boolean = {
  //     val samePage = thePgeRegion.pageId == r.pageId
  //     samePage && (thePgeRegion.bbox intersects r.bbox)
  //   }
  //   // def intersects(r: TargetRegion): Boolean = {
  //   //   val samePage = thePgeRegion.pageId == r.pageId
  //   //   samePage && (thePgeRegion.bbox intersects r.bbox)
  //   // }
  //   def intersection(b: LTBounds): Option[PgeRegion] = {
  //     thePgeRegion.bbox
  //       .intersection(b)
  //       .map(b => thePgeRegion.copy(bbox=b))
  //   }
  // }

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
