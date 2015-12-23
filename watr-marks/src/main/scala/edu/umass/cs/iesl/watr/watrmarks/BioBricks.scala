package edu.umass.cs.iesl.watr.watrmarks



case class FontDictionary(
  dict: Map[String, FontInfo] = Map()
)

case class FontInfo(
  // fontName: String,
  fontFamily: String,
  fontSize: String
)

case class TextBounds(
  left: Double,
  bottom: Double,
  width: Double,
  height: Double
)

case class BrickColumn(
  pins: Set[BioPin] = Set(),
  char: Char,
  font: Option[FontInfo],
  bounds: Option[TextBounds]
) {
  // override def toString = s"""${char}; pins:${pins.mkString(",")}; fnt:${font}; b:${bounds}"""
  override def toString = s"""'${char}' pins:${pins.mkString(",")}"""

  def showBox: TB.Box = {
    TB.hjoin(sep=",")(
      s"'${char}",
      TB.hjoin(sep=",")(
        pins.map(_.showBox).toList:_*
      )
    )
  }
}


case class BrickColumns(
  columns: List[BrickColumn] = List()
) {
  override def toString = columns.mkString("bricks\n  ", "\n  ", "\n/bricks")

  def showBox: TB.Box = {
    columns
      .map(_.showBox)
      .mkVBox("")
  }

  def initBrickCursor(l: BioLabel): Option[BrickCursor] =
    BrickColumns.initCursor(l, columns)

}

object BrickColumns {

  def initCursor(
    l: BioLabel,
    startingColumns: List[BrickColumn],
    searchForward: Boolean = true
  ): Option[BrickCursor] = {
    // debugReport("initCursor()", startingColumns)
    l match {
      case CharLabel =>
        if(startingColumns.length>0) {
          Some(BrickCursor(l,
            current = startingColumns.take(1),
            prevs   = (if (searchForward) List() else startingColumns.drop(1)),
            nexts   = (if (searchForward) startingColumns.drop(1) else List())
          ))
        } else
          None
      case _ =>
        // debugReport("matching label", l)
        val (colsBeforeLabel, colsStartingWithLabel) =
          startingColumns.span({lcol =>
            val hasPin = lcol.pins.exists{_.label == l}
            // debugReport("examining pin", l, lcol.pins, hasPin)
            // lcol.pins.exists{_.label != l}
              !hasPin
          })

        // debugReport(colsBeforeLabel, colsStartingWithLabel)

        val (colsWithLabelMinusOne, colsAfterLabelPlusOne) =
          colsStartingWithLabel.span({lcol =>
            lcol.pins.exists{ pin =>
              (pin != l.U
                && (
                  (searchForward && pin != l.L)
                    || (!searchForward && pin != l.B)))
            }
          })

        // debugReport(colsWithLabelMinusOne, colsAfterLabelPlusOne)

        val colsWithLabel =  colsWithLabelMinusOne ++ colsAfterLabelPlusOne.take(1)
        val colsAfterLabel = colsAfterLabelPlusOne.drop(1)

        if (colsWithLabel.length>0) {
          Some(BrickCursor(l,
            current = (if (searchForward) colsWithLabel else colsWithLabel.reverse),
            prevs   = (if (searchForward) colsBeforeLabel.reverse else colsBeforeLabel),
            nexts   = (if (searchForward) colsAfterLabel else colsAfterLabel.reverse)
          ))
        } else None
    }



  }

  // def initCursor(l: BioLabel, columns: List[BrickColumn], searchForward: Boolean = true): Option[BrickCursor] = {

  //   l match {
  //     case CharLabel =>
  //       if(columns.length>0) {
  //         Some(BrickCursor(l,
  //           current = columns.take(1),
  //           prevs   = (if (searchForward) List() else columns.drop(1)),
  //           nexts   = (if (searchForward) columns.drop(1) else List())
  //         ))
  //       } else
  //         None
  //     case _ =>
  //       debugReport("matching label", l)
  //       val (colsBeforeLabel, colsStartingWithLabel) =
  //         columns.span({lcol =>
  //           lcol.pins.exists{_.label != l}
  //         })

  //       val (colsWithLabelMinusOne, colsAfterLabelPlusOne) =
  //         colsStartingWithLabel.span({lcol =>
  //           lcol.pins.exists{ pin =>
  //             (pin != l.U
  //               && (
  //                 (searchForward && pin != l.L)
  //                 || (!searchForward && pin != l.B)))
  //           }
  //         })

  //       val colsWithLabel =  colsWithLabelMinusOne ++ colsAfterLabelPlusOne.take(1)
  //       val colsAfterLabel = colsAfterLabelPlusOne.drop(1)

  //       if (colsWithLabel.length>0) {
  //         Some(BrickCursor(l,
  //           current = (if (searchForward) colsWithLabel else colsWithLabel.reverse),
  //           prevs   = (if (searchForward) colsBeforeLabel.reverse else colsBeforeLabel),
  //           nexts   = (if (searchForward) colsAfterLabel else colsAfterLabel.reverse)
  //         ))
  //       } else None
  //   }
  // }

}





object biolu {

  def parseBioLine(bioLine: String, continueChar: Char, dict: BioLabelDictionary): Map[Int, BioPin] = {
    var currChar = continueChar
    def currLabel = dict(currChar.toLower)

    (bioLine
      .toIndexedSeq.zipWithIndex
      .filter(p => p._1 != ' ').map {
      case (c, i) =>
        i -> (c match {
          case '-' => currLabel.O
          case '~' => currLabel.I
          case '$' => currLabel.L
          case t if t.isLower => currChar = t; currLabel.B
          case t if t.isUpper => currChar = t; currLabel.U
          case x  => sys.error(s"no mapping for char '${x}'")

        })
    }).toMap
  }

  def parseBioTypes(typeString: String): List[(String, Char)] = {
    typeString.split(", ").map(pairString => {
      val Array(str, c) = pairString.split(": ")
      (str, c.toCharArray()(0))
    }).toList
  }

  def parseBioBrick(
    brickString: String,
    dict: BioLabelDictionary,
    maybeText: Option[String],
    maybeBounds: Option[List[TextBounds]] = None,
    maybeFontInfo: Option[List[FontInfo]] = None
  ): BrickColumns = {



    val bioBrick = bioParsers.parseBioBrick(brickString)
      .left.map(err => sys.error(s"error parsing ${brickString}: $err"))
      .right.get


    val p = bioBrick.pinRows
      .map({ pinrow =>
        parseBioLine(
          pinrow.biostr,
          pinrow.continuationChar,
          dict)
      })

    val allpins = p.toList
      .flatten
      .groupBy(_._1)
      .map{ case (k, v) => k -> v.map(_._2).toSet }
      .toMap

    val text = (bioBrick.text.map(_.text) orElse maybeText)
      .getOrElse(sys.error("no text provided or found in bio brick"))
      .toCharArray

    val bounds  = maybeBounds
      .map(_.map(Some(_)))
      .getOrElse { List.fill(text.length)(None:Option[TextBounds]) }

    val fontInfos  = maybeFontInfo
      .map(_.map(Some(_)))
      .getOrElse { List.fill(text.length)(None:Option[FontInfo]) }

    val cols = text
      .zip(bounds)
      .zip(fontInfos)
      .zipWithIndex
      .map{ case(((c, bound), fontInfo), i) =>
        BrickColumn(
          allpins.getOrElse(i, Set()),
          c, fontInfo, bound
        )
    }

    BrickColumns(cols.toList)

  }

}


