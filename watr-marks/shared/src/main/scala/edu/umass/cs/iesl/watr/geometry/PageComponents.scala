package edu.umass.cs.iesl.watr
package geometry

import scalaz.Equal
import geometry.syntax._
import scalaz.syntax.equal._

import _root_.io.circe, circe.syntax._, circe._
import io.circe.generic._

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


import GeometryCodecs._


@JsonCodec
sealed trait StableIdentifier {
  def stableId: String@@DocumentID
}

@JsonCodec
case class StableDocument(
  stableId: String@@DocumentID,
) extends StableIdentifier

@JsonCodec
case class StablePage(
  stableId: String@@DocumentID,
  pageNum: Int@@PageNum
) extends StableIdentifier {
  override def toString = s"""${stableId}/pg${pageNum}"""
}


@JsonCodec
case class PageRegion(
  page: StablePage,
  bbox: LTBounds
) extends StableIdentifier {
  def stableId: String@@DocumentID = page.stableId
}

object PageRegion {
  implicit val EqualPageRegion: Equal[PageRegion] =
    Equal.equal{
      case (PageRegion(p1, bbox1), PageRegion(p2, bbox2)) =>
        p1 == p2 && bbox1 === bbox2
    }
}


@JsonCodec
case class PageGeometry(
  pageNum: Int@@PageNum,
  bounds: LTBounds
)

@JsonCodec
sealed trait PageItem {
  def pageRegion: PageRegion
  def bbox: LTBounds = pageRegion.bbox
}

object PageItem {

  @JsonCodec
  case class ImageAtom(
    override val pageRegion: PageRegion
  ) extends PageItem


  val VERTICAL   = 0
  val HORIZONTAL = 1
  val SLANTED    = 2
  val EMPTY      = 3

  @JsonCodec
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

  @JsonCodec
  case class CharAtom(
    id: Int@@CharID,
    override val pageRegion: PageRegion,
    char: String
  ) extends PageItem {
    override def toString = s"CharAtom($char, $pageRegion)"
  }

  object CharAtom {
    implicit val EqualCharAtom: Equal[PageItem.CharAtom] =
      Equal.equal((a, b)  => a.id==b.id )

    import circe.generic.semiauto._

    // implicit def Encode_CharAtom: Encoder[PageItem.CharAtom] = deriveEncoder
    // implicit def Decode_CharAtom: Decoder[PageItem.CharAtom] =  deriveDecoder
  }
}


object PageComponentImplicits extends GeometricOps {
  def createPageRegionUri(stableId: String@@DocumentID, pageNum:Int@@PageNum, bbox: LTBounds): String = {
    s"${stableId}+${pageNum}+${bbox.uriString}"
  }

  implicit class RicherPageRegion(val thePageRegion: PageRegion) extends AnyVal {
    def union(r: PageRegion): PageRegion = {
      if (thePageRegion.page != r.page) {
        sys.error(s"""cannot union thePageRegions from different pages: ${thePageRegion} + ${r}""")
      }

      thePageRegion.copy(bbox = thePageRegion.bbox union r.bbox)
    }


    def intersection(b: LTBounds): Option[PageRegion] = {
      thePageRegion.bbox
        .intersection(b)
        .map(b => thePageRegion.copy(bbox=b))
    }


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


  implicit class RicherCharAtom(val charAtom: PageItem.CharAtom) extends AnyVal {

    def debugPrint: String = {
      val bbox = charAtom.bbox.prettyPrint
      s"""${charAtom.char} ${bbox}"""
    }

    def prettyPrint: String = {
      charAtom.char

    }

  }
}
