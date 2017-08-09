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

  // import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
  // object Serialization  {
  //   def serialize[C <: Component](value: C): Array[Byte] = {
  //     val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
  //     val oos = new ObjectOutputStream(stream)
  //     oos.writeObject(value)
  //     oos.close
  //     stream.toByteArray
  //   }

  //   def deserialize[C <: Component](bytes: Array[Byte]): C = {
  //     val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
  //     val value = ois.readObject
  //     ois.close
  //     value.asInstanceOf[C]
  //   }
  // }
}

sealed trait Component {
  def id: Int@@ComponentID

  def roleLabel: Label

  def setRole(l: Label): Component

  def pageRegion(): PageRegion

  def bounds(): LTBounds = pageRegion().bbox

  def chars: String

  def getStableID(): String@@DocumentID = pageRegion.page.stableId

  lazy val pageNum = pageRegion.page.pageNum

  protected [spindex] val labelSet: mutable.Set[Label] = mutable.Set.empty[Label]
  protected [spindex] def addLabel(l: Label): Unit = labelSet.add(l)
  protected [spindex] def removeLabel(l: Label): Unit = labelSet.remove(l)

  def hasLabel(l: Label): Boolean = labels.contains(l)
  def labels(): Set[Label] = labelSet.toSet

}

case class RegionComponent(
  id: Int@@ComponentID,
  override val roleLabel: Label,
  override val pageRegion: PageRegion,
  text: Option[String] = None
) extends Component {

  def setRole(l: Label): Component = copy(roleLabel = l)

  def chars: String = text.getOrElse("")

  override def toString(): String = {
    s"<${roleLabel.key}.${id} ${pageRegion}"
  }
}

case class AtomicComponent(
  id: Int@@ComponentID,
  charAtom: CharAtom,
  override val roleLabel: Label = LB.PageAtom
) extends Component {

  def setRole(l: Label): Component = copy(roleLabel = l)

  def pageRegion: PageRegion = charAtom.pageRegion

  def chars: String = charAtom.char

  override def toString(): String = {
    s"<`${chars}`${id} ${charAtom.bbox.prettyPrint}>"
  }
}
