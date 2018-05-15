package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
import watrmarks._
import textboxing.{TextBoxing => TB}, TB._

import scala.scalajs.js.annotation._
import utils.Cursor
import utils.DoOrDieHandlers._

import scalaz._

object LabelTarget {
  type SetType[A] = mutable.ArrayStack[A]
  type PinSet = SetType[BioPin]
  def PinSet() = mutable.ArrayStack[BioPin]()

  def apply[A](a: A): LabelTarget = new LabelTarget {
    val value = a;
  }

}

@JSExportAll
trait LabelTarget {

  val pins: LabelTarget.PinSet = LabelTarget.PinSet()

  def labels: LabelTarget.SetType[Label] = pins.map(_.label)

  def addPin(p: BioPin): Unit = pins.push(p)

  def addLabel(l: Label): Unit = addPin(l.U)

  def removeLabel(l: Label): Unit = {
    while(hasLabel(l)) {
      pins.pop()
    }
  }

  def hasLabel(l: Label): Boolean = {
    pins.exists(_.label == l)
  }

  def hasPin(p: BioPin): Boolean = {
    pins.contains(p)
  }

  def topPin(): Option[BioPin] = {
    if (pins.nonEmpty) {
      Some(pins.top)
    } else None
  }

  def topLabel(): Option[Label] =
    topPin().map(_.label)

  def showPinsVert(): Box = {
    vjoins(left, pins.toList.reverse.map(_.pinChar.toString.box))
  }

  def findPin(l: Label): Option[(BioPin, Int)] = {
    val pinIndex = pins.indexWhere(_.label == l)
    if (pinIndex > -1) Some( (pins(pinIndex), pinIndex) )
    else None
  }

}

object LabeledSequence {

  def addBioLabel(label: Label, cells: Seq[LabelTarget]): Unit = {
    if (cells.length==1) {
      cells.foreach(_.addPin(label.U))
    } else if (cells.length > 1) {
      cells.head.addPin(label.B)
      cells.last.addPin(label.L)
      cells.drop(1).dropRight(1).foreach(
        _.addPin(label.I)
      )
    }
  }

}

trait LabeledSequence[A <: LabelTarget] {

  def labelTargets(): Seq[A]

  def toCursor(): Option[Cursor[A]] = {
    Cursor.init(labelTargets().toList.toZipper)
  }

  def addBioLabel(label: Label, begin: Int=0, len: Int = Int.MaxValue): Unit = {
    for {
      cinit <- toCursor()
      cbegin <- cinit.move(begin)
    } {
      val win = cbegin.toWindow()
        .slurpRight { case (wincurr, _) => wincurr.length < len }

      LabeledSequence.addBioLabel(label, win.cells)
    }
  }



  def findPin(c: A, l: Label): Option[(BioPin, Int)] = {
    val pinIndex = c.pins.indexWhere(_.label == l)
    if (pinIndex > -1) Some( (c.pins(pinIndex), pinIndex) )
    else None

  }



  def get(offset: Int): Option[A] = {
    if (0 <= offset && offset < labelTargets().length) {
      Some(labelTargets()(offset))
    } else None
  }
  // Find the span of grid cells that have the same labeling as the cell at offset

  def findIdenticallyLabeledSiblings(offset: Int): Option[(Int, Seq[A])] = {
    for {
      cell    <- get(offset)
      extents <- cell.labels.headOption match {
        case Some(label) => findLabelExtents(offset, label)
        case None        => findUnlabeledExtents(offset)
      }
    } yield extents
  }


  def findUnlabeledExtents(offset: Int): Option[(Int, Seq[A])] = {
    for {
      zip                  <- labelTargets().toList.toZipper
      atRowColZ            <- zip.move(offset)
    } yield {
      val focusCell = atRowColZ.focus
      val rights = atRowColZ.rights.takeWhile(_.pins.isEmpty)
      val lefts = atRowColZ.lefts.takeWhile(_.pins.isEmpty)

      val start = offset - lefts.length
      val labelEnd = offset + rights.length
      val seq = lefts.reverse ++ (focusCell +: rights)
      (start, seq)
    }
  }


  // Find the range of cells that overlap with cell at offset and that have the given label
  def findLabelExtents(offset: Int, label: Label): Option[(Int, Seq[A])] = {

    def findLabelEnd(zip: Zipper[A]): Zipper[A] = {
      zip.findNext{ cell =>
        findPin(cell, label).exists(_._1.isLast)
      } getOrElse {
        sys.error("could not find label end")
      }
    }
    def findLabelBegin(zip: Zipper[A]): Zipper[A] = {
      zip.findPrevious{ cell =>
        findPin(cell, label).exists(_._1.isBegin)
      } getOrElse {
        sys.error("could not find label begin")
      }
    }

    for {
      zip                  <- labelTargets().toList.toZipper
      atRowColZ            <- zip.move(offset)

      focusCell             = atRowColZ.focus
      (focusPin, pinIndex) <- findPin(focusCell, label)

      (beginZ, endZ) = {
        if (focusPin.isBegin)        (atRowColZ, findLabelEnd(atRowColZ))
        else if (focusPin.isInside)  (findLabelBegin(atRowColZ), findLabelEnd(atRowColZ))
        else if (focusPin.isLast)    (findLabelBegin(atRowColZ), atRowColZ)
        else if (focusPin.isUnit)    (atRowColZ, atRowColZ)
        else                         sys.error(s"findLabelExtents: unknown pin type: ${focusPin}")
      }
    } yield {
      val labelStart = beginZ.lefts.length

      val labelSpan = (endZ.focus +: endZ.lefts).reverse
        .drop(labelStart)

      (labelStart, labelSpan)
    }

  }

  def unlabelNear(offset: Int, label: Label): Unit = {
    findLabelExtents(offset, label).foreach{ case (offset, indexedSeq) =>
      indexedSeq.foreach{ case cell =>
        cell.removeLabel(label)
      }
    }
  }
}
