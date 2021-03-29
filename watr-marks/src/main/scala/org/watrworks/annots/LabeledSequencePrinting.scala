package org.watrworks
package annots

import watrmarks._
import textboxing.{TextBoxing => TB}, TB._
import utils.{RelativeDirection => Dir, Orientation}

/**
  * Various utilities to output string representations of BIOLU labeled sequences
  */

object LabeledSequencePrinting {

  /*
   * Options:
   *   Horizontal structure
   *        Layered pins
   *          Spelled labels  e.g,
   *          Symbolic, Marginal keys (e.g., ^~~$  {A: Author})
   *        Color coded (ansi escapes)
   *   Vertical structure
   *      line per label



   F$ M$ Las$
   Author~~~$ Author~$
   Authors~~~~~~~~~~~$
   */


  def formatBioPins(pins: Seq[BioPin], oriention: Orientation): TB.Box = {
    pins.map{ p =>
      val labelChar = p.label.fqn.head.toString()
      val pinRep = if (p.isUnit || p.isBegin) {
        labelChar.toUpperCase()
      } else {
        labelChar.toLowerCase()
      }
      pinRep.box
    }

    oriention match {
      case Dir.Up =>
      case Dir.Down =>
      case Dir.Left =>
      case Dir.Right =>
    }
    ???
  }

  // def formatBioLabelSequence[A <: LabelTarget](labeledSequence: LabeledSequence[A], oriention: Orientation): TB.Box = {
  //   val zero1 = List[Label]()
  //   val zero2 = List[TB.Box]()
  //   val init = (zero1, zero2)
  //   val bioPinBoxes = labeledSequence.labelTargets()
  //     .foldLeft(init){ case ((labelStack, boxReps), labelTarget) =>
  //       labelTarget.pins.reverse
  //       (labelStack, boxReps)
  //     }
  //   ???
  // }



  def bioPinsBoxFormat(pins: Seq[BioPin]): TB.Box = {
    vjoins(left, pins.map{ p =>
      val labelChar = p.label.fqn.head.toString()
      val pinRep = if (p.isUnit || p.isBegin) {
        labelChar.toUpperCase()
      } else {
        labelChar.toLowerCase()
      }
      pinRep.box
    })
  }

  def labeledSequenceBoxFormat[A <: LabelTarget](labeledSequence: LabeledSequence[A]): TB.Box = {
    val bioPinBoxes = labeledSequence.labelTargets().map{ labelTarget =>
      bioPinsBoxFormat(labelTarget.pins.toSeq)
    }

    hjoins(bottom, bioPinBoxes)
  }

  def createRuler(start: Int, len: Int): TB.Box = {
    val digitMarkers = (start to start+len).map{ i =>
      if (i==start || i % 10 == 0) {
        vjoins(i.toString.map(_.box))
      } else {
        i.toString.last.box
      }
    }
    hjoins(bottom, digitMarkers)
  }
}
