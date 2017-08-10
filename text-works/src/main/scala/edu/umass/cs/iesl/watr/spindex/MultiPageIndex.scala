package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import geometry._
import watrmarks._

import utils.IdGenerator
import geometry.syntax._

import textreflow.data._
import TypeTags._
import corpora._

import watrmarks.{StandardLabels => LB}
import utils.ExactFloats._
import java.nio.{file => nio}


/**

  MultiPageIndex manages:
    - PageIndexes, one per PDF page

    - Zones, each of which is a list of Components, potentially crossing PageIndexes
    - Relations and Props, e.g., {ClusterID -hasMember-> MentionID}, {id hasProp isTargetEntity}
      - nb. this is a kludge that will be removed at some point

  */

object MultiPageIndex {

  def load(rootPath: nio.Path): MultiPageIndex = {
    ???
  }
}

class MultiPageIndex(
  stableId: String@@DocumentID,
  val docStore: DocumentZoningApi
) {
  lazy val docId: Int@@DocumentID =
    docStore.getDocument(stableId).getOrElse(sys.error("MultiPageIndex created for non-existent document"))


  def getStableId(): String@@DocumentID = stableId


  val pageIndexes = mutable.HashMap[Int@@PageNum, PageIndex]()

  // ID generators
  val componentIdGen = IdGenerator[ComponentID]()

  def getTextReflow(zoneId: Int@@ZoneID): Option[TextReflow] = {
    docStore.getTextReflowForZone(zoneId)
  }

  val relations = mutable.ArrayBuffer[Relation.Record]()
  val props = mutable.ArrayBuffer[Prop.PropRec]()

  def addRelations(rs: Seq[Relation.Record]): Unit = {
    relations ++= rs
  }

  def addProps(rs: Seq[Prop.PropRec]): Unit = {
    props ++= rs
  }


  def getPageIndex(pageNum: Int@@PageNum) = pageIndexes(pageNum)

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

      val region = createRegionComponent(pageRegion, role, None)
      // componentIdToRegionId.put(region.id, targetRegion.id)

      Some((region, targetRegion))
    }
  }


  def createRegionComponent(targetRegion: PageRegion, role: Label, text:Option[String]): RegionComponent = {
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
        val c = createRegionComponent(region, LB.LinePath, None)
        addComponent(c)
        c
      }

    val hlineCCs = path.horizontalLines
      .map{ line =>
        val region = path.pageRegion.copy(bbox = line.bounds.copy(height=0.01.toFloatExact))
        val c = createRegionComponent(region, LB.HLinePath, None)
        addComponent(c)
        c
      }
    val vlineCCs = path.verticalLines()
      .map{ line =>
        val region = path.pageRegion.copy(bbox = line.bounds.copy(width=0.01.toFloatExact))
        val c = createRegionComponent(region, LB.VLinePath, None)
        addComponent(c)
        c
      }
    val region = path.pageRegion
    val c = createRegionComponent(region, LB.PathBounds, None)
    addComponent(c)

    Seq(c) ++ hlineCCs ++ vlineCCs ++ slineCCs
  }

  def addImageAtom(pageAtom: PageItem.ImageAtom): RegionComponent = {
    // println(s"addImageAtom ${pageAtom.pageRegion}")
    val c = createRegionComponent(pageAtom.pageRegion, LB.Image, None)
    addComponent(c)
    c
  }

  def getPageAtoms(pageNum: Int@@PageNum): Seq[AtomicComponent] = {
    getPageIndex(pageNum).getPageAtoms
  }

  def getImageAtoms(pageNum: Int@@PageNum): Seq[RegionComponent] = {
    getPageIndex(pageNum).getImageAtoms
  }

  def addComponent(c: Component): Component = {
    val pageNum = c.pageRegion.page.pageNum
    getPageIndex(pageNum)
      .addComponent(c)
  }

  def getPageGeometry(p: Int@@PageNum) = pageIndexes(p).pageGeometry

  def getPages(): List[Int@@PageNum] = {
    pageIndexes.keys.toList.sortBy(PageNum.unwrap(_))
  }


  def addPage(pageGeometry: PageGeometry): PageIndex = {
    val pageIndex = new PageIndex(
      pageGeometry
    )

    val existing = pageIndexes.put(pageGeometry.id, pageIndex)

    existing.foreach { e =>
      sys.error("adding new page w/existing id")
    }
    pageIndex
  }



}
