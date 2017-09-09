package edu.umass.cs.iesl.watr
package spindex

import watrmarks._
import geometry._
import watrmarks.{StandardLabels => LB}
import scala.collection.mutable


object Component {

  import rindex._
  implicit object ComponentIndexable extends RTreeIndexable[Component] {
    def id(t: Component): Int = t.id.unwrap
    def ltBounds(t: Component): LTBounds = t.bounds
  }

}

sealed trait Component {
  def id: Int@@ComponentID

  def roleLabel: Label

  def pageRegion(): PageRegion

  def bounds(): LTBounds = pageRegion().bbox

  def getStableID(): String@@DocumentID = pageRegion.page.stableId

  lazy val pageNum = pageRegion.page.pageNum

  protected [spindex] val labelSet: mutable.Set[Label] = mutable.Set.empty[Label]

  protected [spindex] def addLabel(l: Label): Unit = labelSet += l
  protected [spindex] def removeLabel(l: Label): Unit =  labelSet -= l

  def hasLabel(l: Label): Boolean = labels.contains(l)
  def labels(): Set[Label] = labelSet.toSet

  def formatLabels(): String = {
    labels.toSeq
      .filter(_ != roleLabel)
      .sortBy(_.fqn).mkString("[", "; ", "]")
  }

  // def formatWithLabels(): String = {
  //   s"<${roleLabel.key}.${id} ${formatLabels()}>"
  // }
}

case class RegionComponent(
  id: Int@@ComponentID,
  override val roleLabel: Label,
  override val pageRegion: PageRegion,
  text: Option[String] = None
) extends Component {

  override def toString(): String = {
    s"<${roleLabel.key}.${id} ${formatLabels}>"
  }
}

case class AtomicComponent(
  id: Int@@ComponentID,
  charAtom: CharAtom,
  override val roleLabel: Label = LB.PageAtom
) extends Component {


  def pageRegion: PageRegion = charAtom.pageRegion

  def chars: String = charAtom.char

  override def toString(): String = {
    s"<`${chars}`${id} ${formatLabels()}>"
  }
}
