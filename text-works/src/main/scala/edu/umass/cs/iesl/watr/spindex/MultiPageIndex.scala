package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable

import geometry._

import utils.IdGenerator

import TypeTags._
import corpora._

import extract.ExtractedItem

/**
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

  def getPageGeometry(p: Int@@PageNum) = pageIndexes(p).pageGeometry

  def getPages(): List[Int@@PageNum] = {
    pageIndexes.keys.toList.sortBy(PageNum.unwrap(_))
  }


}
