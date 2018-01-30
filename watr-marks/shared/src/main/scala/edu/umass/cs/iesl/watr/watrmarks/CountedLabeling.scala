package edu.umass.cs.iesl.watr
package watrmarks


import scala.collection.mutable
import textboxing.{TextBoxing => TB}, TB._

object WeightedLabeling {

  def apply(): WeightedLabeling = new WeightedLabeling {}

}


trait WeightedLabeling {

  private val pins = mutable.ArrayBuffer[BioPin]()

  def uniquePins(): Seq[BioPin] = {
    pins.toSet.toSeq
  }

  def countedPins(): Seq[(BioPin, Int)] = {
    uniquePins().map { pin =>
      (pin, pins.count(_ == pin))
    }
  }

  def addPin(p: BioPin): Unit = pins.append(p)

  def hasPin(p: BioPin): Boolean = {
    pins.contains(p)
  }
}
