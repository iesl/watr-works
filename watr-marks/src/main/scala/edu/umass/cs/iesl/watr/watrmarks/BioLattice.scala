package edu.umass.cs.iesl.watr
package watrmarks

import dom._

case class BioColumn(
  pins: Set[BioPin] = Set(),
  char: Char,
  domAndIndex: Option[(DomCursor, Int)],
  fonts: Option[FontInfo],
  bounds: Option[TextBounds]
) {

  def labels: Set[BioPin] = {
    domAndIndex match  {
      case Some((dom,index)) =>
        val ancestorLabels = dom.loc
          .path
          .collect{ case d => d.isInstanceOf[Grp] }
          .map{ case d => d.asInstanceOf[Grp].labels.toSet }
          .reduce(_ ++ _)

        ancestorLabels
      case None =>
        Set()
    }
  }

  import TB._

  def showIcon: TB.Box = {
    TB.hjoin(sep=",")(
      s"'${char}",
      TB.hjoin(sep=",")(
        pins.map(_.showBox).toList:_*
      )
    )
  }

  def showBox: TB.Box = {
    val showDom = domAndIndex.map{case (dom, i) =>
      s" /${dom.showBox}#${i}".box
    } getOrElse TB.nullBox

    TB.vjoin()(
      showDom,
      TB.hjoin(sep=",")(
        s"'${char}",
        TB.hjoin(sep=",")(
          pins.map(_.showBox).toList:_*
        )
      )
    )
  }
}

case class BioLattice(
  columns: List[BioColumn] = List()
) {
  override def toString = columns.mkString("bricks\n  ", "\n  ", "\n/bricks")

  def showBox: TB.Box = {
    columns
      .map(_.showBox)
      .mkVBox("")
  }

  def initLatticeCursor(l: BioLabel): Option[LatticeCursor] =
    BioLattice.initCursor(l, columns, Forward)

}

object BioLattice {
  def initFromBrickColumns(brickColumns: BrickColumns): BioLattice = {
    val cols = brickColumns
      .columns
      .zipWithIndex
      .map ({ case (col, i) =>
        BioColumn(col.pins, col.char, None, None, None)
      })

    BioLattice(cols)
  }

  def initFromDom(dom: WatrDom): BioLattice = {
    var currentPageNum = 0

    var currentPageLabel: BioPin = PageLabel.fencePost

    dom.toDomCursor.unfoldTSpansCursors.foldLeft(
      BioLattice()
    )({ case (accLattice, tspanCursor) =>
      val bioBrick = tspanCursor.getLabelAsTSpan.bioBrick

      val (pageLefts, pageFocus, _)  = tspanCursor.loc
        .parents.reverse
        .drop(2).head

      debugReport("current page label", currentPageNum, currentPageLabel, pageFocus)
      val pGrp = pageFocus.asInstanceOf[Grp]

      if (!pGrp.labels.exists(_ == currentPageLabel)) {
        currentPageLabel = pGrp.labels.head
        currentPageNum += 1
      }


      val cols = bioBrick
        .columns
        .zipWithIndex
        .map ({ case (col, i) =>
          BioColumn(
            col.pins,
            col.char,
            Some(tspanCursor -> i),
            col.font,
            col.bounds
          )
        })

      BioLattice(accLattice.columns++cols)

    })

  }

  def initCursor(
    l: BioLabel,
    startingColumns: List[BioColumn],
    direction: Direction
  ): Option[LatticeCursor] = {

    def hasUnitPin(pins: Set[BioPin]) = pins.exists(_ == l.U)
    def hasBPin(pins: Set[BioPin]) = direction.isBackward && pins.exists(_ == l.B)
    def hasLPin(pins: Set[BioPin]) = direction.isForward && pins.exists(_ == l.L)

    l match {
      case CharLabel =>
        if(startingColumns.length>0) Some(
          LatticeCursor(l,
            current = startingColumns.take(1),
            prevs   = (if (direction.isForward) List() else startingColumns.drop(1)),
            nexts   = (if (direction.isForward) startingColumns.drop(1) else List())
          )
        ) else None

      case PageLabel =>
        val (colsBeforeLabel, colsStartingWithLabel) =
          startingColumns.span({lcol =>
            val domPins = lcol.labels

            val hasPin = domPins.exists{_.label == PageLabel}

            !hasPin
          })

        // val pageFencePost: Set[BioPin] = colsStartingWithLabel.head.labels
        val pageFencePost: BioPin = colsStartingWithLabel.head.labels.head

        val (colsWithLabelMinusOne, colsAfterLabelPlusOne) =
          colsStartingWithLabel.span({lcol =>
            lcol.labels.head == pageFencePost
          })

        val colsWithLabel =  colsWithLabelMinusOne ++ colsAfterLabelPlusOne.take(1)
        val colsAfterLabel = colsAfterLabelPlusOne.drop(1)

        if (colsWithLabel.length>0) {
          Some(LatticeCursor(l,
            current = direction.isForward? colsWithLabel | colsWithLabel.reverse,
            prevs   = direction.isForward? colsBeforeLabel.reverse | colsBeforeLabel,
            nexts   = direction.isForward? colsAfterLabel | colsAfterLabel.reverse
          ))
        } else None

      case _ =>

        val (colsBeforeLabel, colsStartingWithLabel) =
          startingColumns.span({lcol =>
            val hasPin = lcol.pins.exists{_.label == l}

            !hasPin
          })

        val (colsWithLabelMinusOne, colsAfterLabelPlusOne) =
          colsStartingWithLabel.span({lcol =>
            !(hasUnitPin(lcol.pins)
              || hasBPin(lcol.pins)
              || hasLPin(lcol.pins))
          })

        val colsWithLabel =  colsWithLabelMinusOne ++ colsAfterLabelPlusOne.take(1)
        val colsAfterLabel = colsAfterLabelPlusOne.drop(1)

        if (colsWithLabel.length>0) {
          Some(LatticeCursor(l,
            current = direction.isForward? colsWithLabel | colsWithLabel.reverse,
            prevs   = direction.isForward? colsBeforeLabel.reverse | colsBeforeLabel,
            nexts   = direction.isForward? colsAfterLabel | colsAfterLabel.reverse
          ))
        } else None
    }
  }

}

// debugReport(colsWithLabelMinusOne, colsAfterLabelPlusOne)
// if (l.name=="foo") {
//   import _root_.ammonite.repl.Main
//   import Main._
//   Main.debug("self" -> this,
//     "l"->l,
//     "startingColumns"->startingColumns,
//     "direction" -> direction,
//     "colsWithLabelMinusOne" -> colsWithLabelMinusOne,
//     "colsAfterLabelPlusOne" -> colsAfterLabelPlusOne
//   )
// }
