package edu.umass.cs.iesl.watr
package watrmarks

import java.io.InputStream
import play.api.libs.json
import scalaz._
import Scalaz._

object ZoneIterator extends SpatialJsonFormat{

  def load(zoneJsonIS: InputStream): Option[PageIterator] = {
    load(json.Json.parse(zoneJsonIS))
  }

  def load(jvalue: json.JsValue): Option[PageIterator] = {
    val zoneIndex = jvalue
      .validate[ZoneRecords]
      .map { ZoneIndexer.loadSpatialIndices(_) }
      .getOrElse { sys.error("could not load zone records") }

     zoneIndex
      .pageGeometries.toList
      .map(_._1)
      .sortBy(PageID.unwrap(_))
      .toZipper
      .map(zipper => new PageIterator(zipper, zoneIndex))

  }

}


class PageIterator(
  focus: Zipper[Int@@PageID],
  zoneIndex: ZoneIndexer
) {

  lazy val currPageID = focus.focus

  def nextPage: Option[PageIterator] = {
    focus.next.map(new PageIterator(_, zoneIndex))
  }

  def getZones(label: Label): Stream[ZoneIterator] = {
    val maybeZones = zoneIndex.zoneLabelMap
      .filter({case (zoneId, labels) =>
        (zoneIndex.zoneMap(zoneId).bboxes.exists(
          tbnds => tbnds.target==currPageID
        )) &&
        labels.contains(label)
      })
      .map(_._1)
      .toList
      .sortBy{ZoneID.unwrap(_)}
      .toZipper
      .map(zipper => new ZoneIterator(zipper, label, zoneIndex))

    val zoneStream = unfold[Option[ZoneIterator], ZoneIterator](
      maybeZones
    )(_ match {
      case Some(cur) => Some((cur, cur.next))
      case None => None
    })
    zoneStream
  }

}



class ZoneIterator(
  focii: Zipper[Int@@ZoneID],
  label: Label,
  zoneIndex: ZoneIndexer
) {


  override def toString = {
    s"""${label}#${getFocus}"""
  }

  private def getFocus() = focii.focus
  // zoneIndex.getPageIterator

  def addLabel(label: Label) = ???

  def getLabels(): Seq[Label] = ???

  def next: Option[ZoneIterator] = {
    focii.next.map(new ZoneIterator(_, label, zoneIndex))
  }

  def getBoundingBoxes(): Seq[TargetedBounds] = {
    zoneIndex.zoneMap(getFocus).bboxes
  }

  import StandardLabels._

  def getTokens(): Seq[(Zone, Label)] = {
    val res = getBoundingBoxes.map{ bbox =>
      zoneIndex.query(bbox.target, bbox.bbox)
    }.flatten

    val tokens = res.map{ zone =>
      val labels = zoneIndex.zoneLabelMap(zone.id)
      labels
        .filter(_ matches Token)
        .map(l => (zone, l))
    }
    tokens.flatten
  }

  def getText(): String = {
    val tokens = getTokens.map({case (z, l) =>
      s"""${l.value.getOrElse("?")}"""
    })
    tokens.mkString(" ")
  }

  def zoneIterator(label: String) = ???


}
