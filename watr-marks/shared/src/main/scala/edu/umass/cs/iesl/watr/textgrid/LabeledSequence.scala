package edu.umass.cs.iesl.watr
package textgrid


import scala.collection.mutable
import watrmarks._
import textboxing.{TextBoxing => TB}, TB._

import scala.scalajs.js.annotation._
import utils.{Cursor, Cursors, Window}

import utils.DoOrDieHandlers._


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
    if (hasLabel(l)) {
      while(hasLabel(l)) {
        pins.pop()
      }
    }
  }

  def hasLabel(l: Label): Boolean = {
    pins.exists(_.label == l)
  }

  def hasPin(p: BioPin): Boolean = {
    pins.contains(p)
  }

  def topLabel(): Option[Label] = {
    if (pins.nonEmpty) {
      Some(pins.top.label)
    } else None
  }

  def topPin(): Option[BioPin] = {
    if (pins.nonEmpty) {
      Some(pins.top)
    } else None
  }

  def showPinsVert(): Box = {
    vjoins(left, pins.toList.reverse.map(_.pinChar.toString.box))
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

  def addBioLabel(label: Label): Unit = {
    val rowC = this.toCursor.get
    val win = rowC.toWindow.slurpRight{ case (window, next) =>
      window.length <= labelTargets().length
    }

    LabeledSequence.addBioLabel(label, win.cells)
  }



  def findPin(c: A, l: Label): Option[(BioPin, Int)] = {
    val pinIndex = c.pins.indexWhere(_.label == l)
    if (pinIndex > -1) Some( (c.pins(pinIndex), pinIndex) )
    else None

  }


  private def haveSameLabels(cell1: A, cell2: A): Boolean = {
    val p1s = cell1.pins
    val p2s = cell2.pins
    p1s.length == p2s.length && {
      p1s.zip(p2s).map{ case (p1, p2) =>
        p1.label == p2.label
      } forall (b => b)
    }
  }

  def get(offset: Int): Option[A] = {
    if (0 <= offset && offset < labelTargets().length) {
      Some(labelTargets()(offset))
    } else None
  }

  // Find the span of grid cells that have the same labeling as the cell at offset

  def findIdenticallyLabeledSiblings(offset: Int): Option[Seq[(A, Int, Int)]] = {
    // get(offset).map { cell =>

    //   cell.labels.headOption.map { label =>
    //     val extents = findLabelExtents(offset, label).orDie("illegal state in label extents")
    //     val (pre, post) =  extents.span { case cell => rw!=row && cl!=col }
    //     val postIdenticals = post.takeWhile{ case (cell, rw, cl) =>
    //       haveSameLabels(cell, cell)
    //     }

    //     val preIdenticals = pre.reverse.takeWhile{ case (cell, _, _) =>
    //       haveSameLabels(cell, cell)
    //     }

    //     preIdenticals.reverse ++ postIdenticals

    //   } getOrElse {
    //     // find span of unlabeled siblings
    //     val (pre, post) = indexedCells().span { case (cell, rw, cl) => cell != cell }
    //     val postIdenticals = post.takeWhile{ case (cell, rw, cl) =>
    //       cell.pins.isEmpty
    //     }

    //     val preIdenticals = pre.reverse.takeWhile{ case (cell, _, _) =>
    //       cell.pins.isEmpty
    //     }

    //     preIdenticals.reverse ++ postIdenticals
    //   }
    // }
    ???
  }


  // Find the range of cells that overlap with cell at offset and that have the given label
  def findLabelExtents(offset: Int, label: Label): Option[(Int, Seq[A])] = {
    import scalaz._

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
      zip <- labelTargets().toList.toZipper
      // atRowColZ <- zip.findZ{ case (cell, crow, ccol) => crow==row && ccol==col }
      atRowColZ <- zip.move(offset)

      focusCell = atRowColZ.focus
      (focusPin, pinIndex) <- findPin(focusCell, label)

      (beginZ, endZ) = {
        if (focusPin.isBegin)        (atRowColZ, findLabelEnd(atRowColZ))
        else if (focusPin.isInside)  (findLabelBegin(atRowColZ), findLabelEnd(atRowColZ))
        else if (focusPin.isLast)    (findLabelBegin(atRowColZ), atRowColZ)
        else if (focusPin.isUnit)    (atRowColZ, atRowColZ)
        else                         sys.error("findLabelExtents: unknown pin type")
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
