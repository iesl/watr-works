package edu.umass.cs.iesl.watr.watrmarks


sealed trait BioPin {
  def label: BioLabel
}

case class BPin(label: BioLabel) extends BioPin
case class IPin(label: BioLabel) extends BioPin
case class OPin(label: BioLabel) extends BioPin
case class LPin(label: BioLabel) extends BioPin
case class UPin(label: BioLabel) extends BioPin

class BioLabel(val namespace: String, val name: String, val c: Char, val constraint: Constraint) {
  lazy val B = BPin(this)
  lazy val I = IPin(this)
  lazy val O = OPin(this)
  lazy val L = LPin(this)
  lazy val U = UPin(this)
}


trait BioLabelDictionary {
  def apply(c: Char): BioLabel
  def apply(s: String): BioLabel
}


object BioLabel {

  def apply(ns: String, name: String, c: Char, constraint: BioLabel) =
    new BioLabel(ns, name, c, BioConstraint(constraint))

  def apply(ns: String, name: String) =
    new BioLabel(ns, name, name(0), CharConstraint)

  def apply(name: String) =
    new BioLabel(name, name, name(0), CharConstraint)

}

case class LabeledColumn(
  labels: Set[BioPin] = Set()
)

case class LabeledSpan(
  columns: List[LabeledColumn] = List()
)


case class LabeledLocation(
  current: List[LabeledColumn],
  prevs: List[LabeledColumn] = List(),
  nexts: List[LabeledColumn] = List()
) {
  def next(label: BioLabel): LabeledLocation = {
    LabeledLocation(current, prevs, nexts)
  }

  def addLabel(label: BioLabel): LabeledLocation = {
    this
  }

  def toLabeledSpan = LabeledSpan(
    prevs.reverse ++ current ++ nexts
  )

}


object biolu {

  def parseBioLine(bioLine: String, ochar: Option[Char], dict: BioLabelDictionary): Map[Int, BioPin] = {
    var currChar = ochar.getOrElse(' ')

    (bioLine
      .toIndexedSeq.zipWithIndex
      .filter(p => p._1 != ' ').map {
      case (c, i) =>
        i -> (c match {
          case '-' => dict(currChar).O
          case '~' => dict(currChar).I
          case '$' => dict(currChar).L
          case t if t.isLower => dict(currChar).B
          case t if t.isUpper => dict(currChar).U
        })
    }).toMap
  }
  def parseBioBricks(labelString: String): Map[Int, Label] = {
    (labelString
      .toIndexedSeq.zipWithIndex
      .filter(p => p._1 != ' ').map {
      case (c, i) =>
        i -> (c match {
          case '-' => O
          case '~' => I
          case '$' => L
          case t if t.isLower => B(t)
          case t if t.isUpper => U(t.toLower)
        })
    }).toMap
  }

  def parseBioTypes(typeString: String): List[(String, Char)] = {
    typeString.split(", ").map(pairString => {
      val Array(str, c) = pairString.split(": ")
      (str, c.toCharArray()(0))
    }).toList
  }

  def parseBioConstraints(constraintString: String): ConstraintRange = {
    val typeList = constraintString.split('.')
    val size = typeList.size
    val lastType = typeList(size - 1)
    val con = if (lastType == "char") {
      CharCon
    } else {
      SegmentCon(lastType)
    }

    if (size == 1) {
      Single(con)
    } else {
      Range(typeList(0), con)
    }
  }



  // def parseBioBlock(blockString: String): List[(Map[Int, Label], List[(String, Char)], ConstraintRange)] = {
  def parseBioBlock(blockString: String): Unit = {

    val parsed = bioParsers.parseBioBlock(blockString)
      .left.map(err => sys.error(s"error parsing ${blockString}: $err"))
      .right.get

    parsed.map{ case (c, biostr, bioObj) =>
      val bioline = parseBioBricks(biostr)

    }

    //   val labelMap = parseBioBricks(labelStr)
    //   val typePairList = parseBioTypes(defDict)


    // fullPattern.r.findAllIn(blockString).toList.reverse.map(spanString => {
    //   println("here!")
    //   spanString match {
    //     case fullPattern.r(labelString, typeString, _, constraintString, _) =>
    //       println("here(full)!")

    //       val labelMap = parseBioBricks(labelString)
    //       val typePairList = parseBioTypes(typeString)
    //       val constraintRange = parseBioConstraints(constraintString)
    //       (labelMap, typePairList, constraintRange)
    //     case _ =>
    //       println("here _!")
    //       sys.error("")
    //   }

    // })

  }

}













/** Constructors for labels
  * They are used by Annotator instances for annotating characters of text
  */
sealed trait Label
case class B(c: Char) extends Label
case object I extends Label
case object O extends Label
case object L extends Label
case class U(c: Char) extends Label


sealed trait Constraint

// /** Constructor for char constraints
//   *
//   * It is used by Annotator instances to constrain labels of annotation types
//   * to the the primitive unit of characters
//   */
case object CharCon extends Constraint
case object CharConstraint extends Constraint

// /** Constructor for segment constraints
//   *
//   * It is used by Annotator instances to constrain labels of annotation types
//   * to the index pairs containing the B or U labels of other annotation types
//   */
case class SegmentCon(annotationTypeName: String) extends Constraint
case class BioConstraint(label: BioLabel) extends Constraint


// @deprecated("","")
sealed trait ConstraintRange


/** Constructor to create a constraint range
  *
  * It is used by Annotator instances to hold an annotation type string
  * and a constraint such that the annotation type is constrained by the constraint
  * or is constrained by a SegmentCon whose annotation type has the qualities
  * of the previously mentioned annotation type string
  */
@deprecated("","")
case class Range(annoTypeName: String, con: Constraint) extends ConstraintRange


/** Constructor to make a constraint range consisting of just a single constraint **/
@deprecated("","")
case class Single(constraint: Constraint) extends ConstraintRange
