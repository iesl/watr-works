package edu.umass.cs.iesl.watr
package segment

import geometry._
import watrmarks.Label
import corpora.DocumentZoningApi
import spindex._

trait DocumentScopeSegmenter extends SegmentationCommons { self =>

  lazy val docScope = self

  def mpageIndex: MultiPageIndex

  def docStore: DocumentZoningApi

  def docStats: DocumentLayoutStats

  def stableId: String@@DocumentID
  def docId: Int@@DocumentID

  def pageSegmenters(): Seq[PageScopeSegmenter]

  def createZone(label: Label, pageRegions: Seq[PageRegion]): Option[Int@@ZoneID] = {
    docStore.labelRegions(label, pageRegions)
  }
}

trait PageScopeSegmenter extends PageScopeTracing with SegmentationCommons { self =>
  lazy val pageScope = self

  def docScope: DocumentScopeSegmenter

  def pageId: Int@@PageID
  def pageNum: Int@@PageNum
  def pageStats: PageLayoutStats

  def docStore: DocumentZoningApi = docScope.docStore

  def mpageIndex: MultiPageIndex = docScope.mpageIndex
  def pageGeometry = docStore.getPageGeometry(pageId)
  def pageIndex: PageIndex = mpageIndex.getPageIndex(pageNum)

  def labelRegion(bbox: LTBounds, label: Label, text: Option[String] = None): RegionComponent = {
    val regionId = docStore.addTargetRegion(pageId, bbox)
    val pageRegion = docStore.getTargetRegion(regionId)
    mpageIndex.createRegionComponent(pageRegion, label, text)
  }

  def deleteComponentsWithLabel(l: Label): Unit = {
    pageIndex.getComponentsWithLabel(l)
      .foreach { cc =>
        pageIndex.removeComponent(cc)
      }
  }

}

trait SegmentationCommons {
  def modalValue(ccs: Seq[Component], f: Component => Int): Option[Int] = {
    ccs.groupBy{f(_)}.toSeq
      .sortBy({ case (_, atoms) => atoms.length })
      .reverse.headOption.map(_._1)
  }

}
