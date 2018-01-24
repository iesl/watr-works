package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import geometry._
import watrmarks._

import utils.IdGenerator
import geometry.syntax._

import TypeTags._
import corpora._

import segment.{SegmentationLabels => LB}
import utils.ExactFloats._
import java.nio.{file => nio}
import extract.ExtractedItem


/**

  MultiPageIndex manages:
    - PageIndexes, one per PDF page

    - Zones, each of which is a list of Components, potentially crossing PageIndexes
    - Relations and Props, e.g., {ClusterID -hasMember-> MentionID}, {id hasProp isTargetEntity}
      - nb. this is a kludge that will be removed at some point

  */


class MultiPageIndex(
  stableId: String@@DocumentID,
  val docStore: DocumentZoningApi,
  val extractedItems: Seq[(Seq[ExtractedItem], PageGeometry)]

) {
  lazy val docId: Int@@DocumentID =
    docStore.getDocument(stableId).getOrElse(sys.error("MultiPageIndex created for non-existent document"))


  def getStableId(): String@@DocumentID = stableId

  val pageIndexes = mutable.HashMap[Int@@PageNum, PageIndex]()

  // ID generators
  val componentIdGen = IdGenerator[ComponentID]()

  private def initPages(): Unit = {
    val extractedItemCount = (0 +: extractedItems.map(_._1.length)).sum

    val itemArray = new Array[ExtractedItem](extractedItemCount+1)

    extractedItems.foreach { case (items, _) =>
      println(s"items len: ${items.length}")
      items.foreach { item => itemArray(item.id.unwrap) = item }
    }

    var itemArrayOffsetForPage: Int = 1

    extractedItems.foreach { case (items, pageGeometry) =>

      val pageIndex = new PageIndex(
        pageGeometry,
        itemArray,
        itemArrayOffsetForPage, items.length
      )

      itemArrayOffsetForPage = itemArrayOffsetForPage + items.length
      val existing = pageIndexes.put(pageGeometry.pageNum, pageIndex)
      existing.foreach { e => sys.error("adding new page w/existing id") }
    }
  }

  initPages()

  val relations = mutable.ArrayBuffer[Relation.Record]()
  val props = mutable.ArrayBuffer[Prop.PropRec]()

  def addRelations(rs: Seq[Relation.Record]): Unit = {
    relations ++= rs
  }

  def addProps(rs: Seq[Prop.PropRec]): Unit = {
    props ++= rs
  }

  def getPageIndex(pageNum: Int@@PageNum) = pageIndexes(pageNum)

  // TODO this should be pushed into the PageIndex class
  def labelRegion(components: Seq[Component], role: Label): Option[(RegionComponent, PageRegion)] = {
    if (components.isEmpty) None else {
      val totalBounds = components.map(_.bounds).reduce(_ union _)
      val targetPages = components.map(_.pageNum.unwrap)
      val numOfTargetPages =  targetPages.toSet.size

      if (numOfTargetPages != 1) {
        sys.error(s"""cannot label connected components from different pages (got pages=${targetPages.mkString(", ")})""")
      }

      val pageNum =  PageNum(targetPages.head)

      val pageId = docStore.getPage(docId, pageNum).get
      val regionId = docStore.addTargetRegion(pageId, totalBounds)
      val targetRegion = docStore.getTargetRegion(regionId)
      val pageRegion = PageRegion(targetRegion.page, targetRegion.bbox)

      val region = createRegionComponent(pageRegion, role)

      Some((region, targetRegion))
    }
  }


  def createRegionComponent(targetRegion: PageRegion, role: Label, text:Option[String] = None): RegionComponent = {
    val region = RegionComponent(componentIdGen.nextId, role, targetRegion, text)
    addComponent(region)

    region
  }

  def addCharAtom(pageAtom: CharAtom): AtomicComponent = {
    val c = AtomicComponent(componentIdGen.nextId, pageAtom)
    addComponent(c)
    c
  }

  def addPathItem(path: PageItem.Path): Seq[RegionComponent] = {

    val slineCCs = path.slantedLines
      .map{ line =>
        val region = path.pageRegion.copy(bbox = line.bounds.copy(height=0.01.toFloatExact))
        val c = createRegionComponent(region, LB.LinePath)
        addComponent(c)
        c
      }

    val hlineCCs = path.horizontalLines
      .map{ line =>
        val region = path.pageRegion.copy(bbox = line.bounds.copy(height=0.01.toFloatExact))
        val c = createRegionComponent(region, LB.HLinePath)
        addComponent(c)
        c
      }
    val vlineCCs = path.verticalLines()
      .map{ line =>
        val region = path.pageRegion.copy(bbox = line.bounds.copy(width=0.01.toFloatExact))
        val c = createRegionComponent(region, LB.VLinePath)
        addComponent(c)
        c
      }
    val region = path.pageRegion
    val c = createRegionComponent(region, LB.PathBounds)
    addComponent(c)

    Seq(c) ++ hlineCCs ++ vlineCCs ++ slineCCs
  }

  def addImageAtom(pageAtom: PageItem.ImageAtom): RegionComponent = {
    // println(s"addImageAtom ${pageAtom.pageRegion}")
    val c = createRegionComponent(pageAtom.pageRegion, LB.Image)
    addComponent(c)
    c
  }

  def addComponent(c: Component): Component = {
    val pageNum = c.pageRegion.page.pageNum
    getPageIndex(pageNum)
      .components.addComponent(c)
  }

  def getPageGeometry(p: Int@@PageNum) = pageIndexes(p).pageGeometry

  def getPages(): List[Int@@PageNum] = {
    pageIndexes.keys.toList.sortBy(PageNum.unwrap(_))
  }


}
