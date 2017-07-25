package edu.umass.cs.iesl.watr
package spindex

import watrmarks._
import geometry._
// import geometry.syntax._
import watrmarks.{StandardLabels => LB}


object Component {
  import rindex._
  implicit object ComponentIndexable extends RTreeIndexable[Component] {
    def id(t: Component): Int = t.id.unwrap
    def ltBounds(t: Component): LTBounds = t.bounds
  }

}
sealed trait Component {
  def id: Int@@ComponentID

  def mpageIndex: MultiPageIndex
  def vtrace = mpageIndex.vtrace

  def roleLabel: Label

  lazy val pageNum = mpageIndex.getPageForComponent(this)

  def getDocumentID() = mpageIndex.docId
  def getStableID() = mpageIndex.getStableId()

  def getPageGeometry(): PageGeometry = {
    mpageIndex.getPageGeometry(pageNum)
  }

  def targetRegion: PageRegion
  def bounds: LTBounds

  def chars: String

  def orientation: Double = 0.0d // placeholder until this is implemented for real

  def addLabel(l: Label): Component = {
    mpageIndex.addLabel(this, l)
  }

  def removeLabel(l: Label): Component = {
    mpageIndex.removeLabel(this, l)
  }

  def getLabels(): Set[Label] = {
    mpageIndex.getLabels(this)
  }

  def getLabel(l: Label): Option[Label] = {
    getLabels().filter(_ == l).headOption
  }
}

case class RegionComponent(
  id: Int@@ComponentID,
  override val roleLabel: Label,
  initPageRegion: PageRegion,
  override val mpageIndex: MultiPageIndex,
  text: Option[String] = None
) extends Component {

  def chars: String = text.getOrElse("")

  def targetRegion: PageRegion = initPageRegion
  def bounds: LTBounds = targetRegion.bbox

  override def toString(): String = {
    val lls = getLabels.mkString(",")
    s"<${roleLabel.key}.${id} ${targetRegion}${lls}>"
  }
}

case class AtomicComponent(
  id: Int@@ComponentID,
  charAtom: CharAtom,
  override val mpageIndex: MultiPageIndex
) extends Component {


  def roleLabel: Label = LB.PageAtom

  val bounds = charAtom.bbox

  def targetRegion: PageRegion = charAtom.pageRegion

  def chars: String = charAtom.char

  override def toString(): String = {
    val lls = getLabels.mkString(",")
    s"<`${chars}`${id} ${charAtom.bbox.prettyPrint}$lls>"
  }
}
