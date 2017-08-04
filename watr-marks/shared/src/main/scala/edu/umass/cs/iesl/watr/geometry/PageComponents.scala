package edu.umass.cs.iesl.watr
package geometry

import scalaz.Equal
import geometry.syntax._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import watrmarks._
import TypeTags._

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

case class StablePage(
  stableId: String@@DocumentID,
  pageNum: Int@@PageNum,
  pageId: Int@@PageID = PageID(-1)
) {
  override def toString = s"""${stableId}/pg${pageNum}"""
  // def pageId: Int@@PageID = idOpt.getOrElse{ sys.error(s"no id for ${toString}") }
}


case class PageRegion(
  page: StablePage,
  bbox: LTBounds,
  regionId: Int@@RegionID = RegionID(-1)
) {
  // def regionId: Int@@RegionID = idOpt.getOrElse{ sys.error(s"no id for ${page} ${bbox}") }
  def id: Int@@RegionID = regionId
}

// case class PageRegion(
//   stableRegion: PageRegion
// ) {
//   def id: Int@@RegionID

// }

object PageRegion {
  implicit val EqualPageRegion: Equal[PageRegion] =
    Equal.equal{
      case (PageRegion(p1, bbox1, _), PageRegion(p2, bbox2, _)) =>
        p1 == p2 && bbox1 === bbox2

      // case (a, b) => (a.idOpt, b.idOpt) match

    }
}


case class PageGeometry(
  id: Int@@PageNum,
  bounds: LTBounds
)

sealed trait PageItem {
  def pageRegion: PageRegion
  def bbox: LTBounds = pageRegion.bbox
}

object PageItem {

  case class ImageAtom(
    override val pageRegion: PageRegion
  ) extends PageItem


  val VERTICAL   = 0
  val HORIZONTAL = 1
  val SLANTED    = 2
  val EMPTY      = 3

  case class Path(
    override val pageRegion: PageRegion,
    points: Seq[Point]
  ) extends PageItem {

    lazy val uniquePoints = if (points.length > 1 ) {
      points.sliding(2).map{
        case Seq(p1, p2)  =>
          if (p1.x == p2.x && p1.y == p2.y) {
            List(p2)
          } else List(p1, p2)
      }.flatten.toSeq
    } else points


    def makeLineWithOrientation(p1: Point, p2: Point): (Line, Int) = {
      if (p1.x == p2.x && p1.y != p2.y) {
        (Line(p1, p2), VERTICAL)
      } else if (p1.x != p2.x && p1.y == p2.y) {
        (Line(p1, p2), HORIZONTAL)
      } else if (p1.x == p2.x && p1.y == p2.y) {
        (Line(p1, p2), EMPTY)
      } else {
        (Line(p1, p2), SLANTED)
      }
    }

    lazy val linesWithOrientation: Seq[(Line, Int)] = {
      if (uniquePoints.length > 1) {

        val segments = uniquePoints.sliding(2).map{
          case Seq(p1, p2)  =>  makeLineWithOrientation(p1, p2)
        }.toSeq

        val finalSegment: Seq[(Line, Int)] =
          if (uniquePoints.length > 2) {
            val l = makeLineWithOrientation(uniquePoints.head, uniquePoints.last)
            if (l._2 != EMPTY) Seq(l) else Seq()
          } else Seq()

        segments ++ finalSegment
      } else Seq[(Line, Int)]()
    }

    def lines(): Seq[Line] = {
      linesWithOrientation.map(_._1)
    }

    def slantedLines(): Seq[Line] = {
      linesWithOrientation.filter(_._2==SLANTED).map(_._1)
    }

    def horizontalLines(): Seq[Line] = {
      linesWithOrientation.filter(_._2==HORIZONTAL).map(_._1)
    }

    def verticalLines(): Seq[Line] = {
      linesWithOrientation.filter(_._2==VERTICAL).map(_._1)
    }
  }
}

case class CharAtom(
  id: Int@@CharID,
  override val pageRegion: PageRegion,
  char: String,
  wonkyCharCode: Option[Int] = None
) extends PageItem {
  override def toString = s"CharAtom($char, $pageRegion)"

}


object CharAtom {
  implicit val EqualCharAtom: Equal[CharAtom] =
    Equal.equal((a, b)  => a.id==b.id )
}

case class Zone(
  id: Int@@ZoneID,
  regions: Seq[PageRegion],
  label: Label
)


object PageComponentImplicits {
  def createPageRegionUri(stableId: String@@DocumentID, pageNum:Int@@PageNum, bbox: LTBounds): String = {
    s"${stableId}+${pageNum}+${bbox.uriString}"
  }

  // implicit class RicherPageRegion(val thePageRegion: PageRegion) extends AnyVal {
  //   def union(r: PageRegion): PageRegion = {
  //     val samePage = thePageRegion.page.pageId == r.page.pageId
  //     if (!samePage) {
  //       sys.error(s"""cannot union PageRegions from different pages: ${thePageRegion} + ${r}""")
  //     }
  //     thePageRegion.copy(bbox = thePageRegion.bbox union r.bbox)
  //   }

  //   def intersects(pageId: Int@@PageID, bbox: LTBounds): Boolean = {
  //     val samePage = thePageRegion.page.pageId == pageId
  //     samePage && (thePageRegion.bbox intersects bbox)
  //   }
  //   def intersects(r: PageRegion): Boolean = {
  //     intersects(r.page.pageId, r.bbox)
  //   }

  //   def intersection(b: LTBounds): Option[PageRegion] = {
  //     thePageRegion.bbox
  //       .intersection(b)
  //       .map(b => thePageRegion.copy(bbox=b))
  //   }
  //   def uriString: String = {
  //     createPageRegionUri(
  //       thePageRegion.page.stableId,
  //       thePageRegion.page.pageNum,
  //       thePageRegion.bbox
  //     )
  //   }
  // }

  implicit class RicherPageRegion(val thePageRegion: PageRegion) extends AnyVal {
    def union(r: PageRegion): PageRegion = {
      if (thePageRegion.page.pageId != r.page.pageId) {
        sys.error(s"""cannot union thePageRegions from different pages: ${thePageRegion} + ${r}""")
      }
      thePageRegion.copy(bbox = thePageRegion.bbox union r.bbox)
    }

    def intersects(pageId: Int@@PageID, bbox: LTBounds): Boolean = {
      val samePage = thePageRegion.page.pageId == pageId
      samePage && (thePageRegion.bbox intersects bbox)
    }
    def intersects(r: PageRegion): Boolean = {
      intersects(r.page.pageId, r.bbox)
    }


    def intersection(b: LTBounds): Option[PageRegion] = {
      thePageRegion.bbox
        .intersection(b)
        .map(b => thePageRegion.copy(bbox=b))
    }

    // def splitHorizontal(r: PageRegion): List[PageRegion] = {
    //   if (thePageRegion.page.pageId != r.page.pageId) {
    //     sys.error(s"""cannot union thePageRegions from different pages: ${thePageRegion} + ${r}""")
    //   }

    //   val splitBoxes = r.bbox.splitHorizontal

    //   splitBoxes.map { ltb =>
    //     thePageRegion.copy(
    //       bbox = ltb
    //     )
    //   }
    // }

    def prettyPrint(): String = {
      thePageRegion.toString
    }

    def uriString: String = {
      createPageRegionUri(
        thePageRegion.page.stableId,
        thePageRegion.page.pageNum,
        thePageRegion.bbox
      )
    }
  }


  implicit class RicherCharAtom(val charAtom: CharAtom) extends AnyVal {

    def debugPrint: String = {
      val bbox = charAtom.bbox.prettyPrint

      val wonk = charAtom.wonkyCharCode
        .map({ code =>
          if (code==32) { "<sp>"  }
          else { s"?:#${code}?" }
        }) getOrElse { "" }

      s"""${charAtom.char} ${wonk} ${bbox}"""
    }

    def prettyPrint: String = {
      charAtom.wonkyCharCode
        .map({ code =>
          if (code==32) { "_"  }
          else { s"#${code}?" }
        })
        .getOrElse({
          charAtom.char
        })
    }

    def bestGuessChar: String = {
      charAtom.wonkyCharCode
        .map({ code =>
          if (code==32) { s" "  }
          else { s"#{${code}}" }
        })
        .getOrElse({
          // if (charAtom)
          charAtom.char
        })
    }

    def isWonky: Boolean = charAtom.wonkyCharCode.isDefined

    def isSpace: Boolean = charAtom.wonkyCharCode.exists(_==32)
    def isControl: Boolean = charAtom.wonkyCharCode.exists(_<32)
    def isNonPrintable: Boolean = charAtom.wonkyCharCode.exists(_<=32)

  }
}

// case class PageRegion(
//   id: Int@@RegionID,
//   page: RecordedPageID,
//   bbox: LTBounds
// ) {
//   lazy val uri = {
//     import PageComponentImplicits._
//     this.uriString
//   }
//   override def toString = s"""${page}/${id}@${bbox.prettyPrint}"""

//   def toPageRegion(): PageRegion = {
//     PageRegion(page, bbox)

//   }
// }



// case class StablePageID(
//   stableId: String@@DocumentID,
//   pageNum: Int@@PageNum
// ) {
//   override def toString = s"""${stableId}/pg${pageNum}"""
// }


// case class RecordedPageID(
//   pageId: Int@@PageID,
//   stable: StablePageID
// ) {
//   override def toString = s"""${stable}@${pageId}"""
// }


// case class PageRegion(
//   page: RecordedPageID,
//   bbox: LTBounds
// )

// case class TargetRegion(
//   id: Int@@RegionID,
//   page: RecordedPageID,
//   bbox: LTBounds
// ) {
//   lazy val uri = {
//     import PageComponentImplicits._
//     this.uriString
//   }
//   override def toString = s"""${page}/${id}@${bbox.prettyPrint}"""

//   def toPageRegion(): PageRegion = {
//     PageRegion(page, bbox)

//   }
// }
