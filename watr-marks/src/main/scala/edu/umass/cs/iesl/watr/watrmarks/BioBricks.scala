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
}


case class BrickColumns(
  columns: List[BrickColumn] = List()
) {
  override def toString = columns.mkString("bricks\n  ", "\n  ", "\n/bricks")

  def initBrickCursor(l: BioLabel): Option[BrickCursor] =
    BrickColumns.initCursor(l, columns)

}

object BrickColumns {



  def initCursor(l: BioLabel, columns: List[BrickColumn], searchForward: Boolean = true): Option[BrickCursor] = {

    l match {
      case CharLabel =>
        if(columns.length>0) {
          Some(BrickCursor(l,
            current=columns.take(1),
            prevs = (if (searchForward) List() else columns.drop(1)),
            nexts = (if (searchForward) columns.drop(1) else List())
          ))
        } else
          None
      case _ =>
        val (colsBeforeLabel, colsStartingWithLabel) =
          columns.span({lcol =>
            lcol.pins.exists{_.label != l}
          })

        val (colsWithLabelMinusOne, colsAfterLabelPlusOne) =
          colsStartingWithLabel.span({lcol =>
            lcol.pins.exists{ pin =>
              (pin != l.U
                && (
                  (searchForward && pin != l.L)
                  || (!searchForward && pin != l.B)))
            }
          })

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

  // def initCursor(l: BioLabel, columns: List[BrickColumn]): Option[BrickCursor] = {
  //   l match {
  //     case CharLabel =>
  //       if(columns.length>0) {
  //         Some(BrickCursor(l,
  //           columns.take(1),
  //           List(),
  //           columns.drop(1)
  //         ))
  //       } else
  //         None
  //     case _ =>
  //       val (colsBeforeLabel, colsStartingWithLabel) =
  //         columns.span({lcol =>
  //           lcol.pins.exists{_.label != l}
  //         })
  //       val (colsWithLabelMinusOne, colsAfterLabelPlusOne) =
  //         colsStartingWithLabel.span({lcol =>
  //           lcol.pins.exists{ pin =>
  //             pin != l.U && pin != l.L
  //           }
  //         })
  //       val colsWithLabel =  colsWithLabelMinusOne ++ colsAfterLabelPlusOne.take(1)
  //       val colsAfterLabel = colsAfterLabelPlusOne.drop(1)

  //       if (colsWithLabel.length>0) {
  //         Some(BrickCursor(l,
  //           colsWithLabel,
  //           colsBeforeLabel.reverse,
  //           colsAfterLabel
  //         ))
  //       } else None
  //   }
  // }

}





object biolu {

  def parseBioLine(bioLine: String, continueChar: Option[Char], dict: BioLabelDictionary): Map[Int, BioPin] = {
    var currChar = continueChar.getOrElse(' ')
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

  // def parseBioConstraints(constraintString: String): ConstraintRange = {
  //   val typeList = constraintString.split('.')
  //   val size = typeList.size
  //   val lastType = typeList(size - 1)
  //   val con = if (lastType == "char") {
  //     CharCon
  //   } else {
  //     SegmentCon(lastType)
  //   }

  //   if (size == 1) {
  //     Single(con)
  //   } else {
  //     Range(typeList(0), con)
  //   }
  // }


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
      .map{ pinrow => parseBioLine(pinrow.biostr, None, dict) }

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




// sealed trait Constraint

// // /** Constructor for char constraints
// //   *
// //   * It is used by Annotator instances to constrain labels of annotation types
// //   * to the the primitive unit of characters
// //   */
// case object CharCon extends Constraint
// case object CharConstraint extends Constraint

// // /** Constructor for segment constraints
// //   *
// //   * It is used by Annotator instances to constrain labels of annotation types
// //   * to the index pairs containing the B or U labels of other annotation types
// //   */
// case class SegmentCon(annotationTypeName: String) extends Constraint
// case class BioConstraint(label: BioLabel) extends Constraint


// // @deprecated("","")
// sealed trait ConstraintRange


// /** Constructor to create a constraint range
//   *
//   * It is used by Annotator instances to hold an annotation type string
//   * and a constraint such that the annotation type is constrained by the constraint
//   * or is constrained by a SegmentCon whose annotation type has the qualities
//   * of the previously mentioned annotation type string
//   */
// @deprecated("","")
// case class Range(annoTypeName: String, con: Constraint) extends ConstraintRange


// /** Constructor to make a constraint range consisting of just a single constraint **/
// @deprecated("","")
// case class Single(constraint: Constraint) extends ConstraintRange
